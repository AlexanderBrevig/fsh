open Core
open Lwt
open Types

(* ========== Shell Execution ========== *)

(* Execute shell command *)
let exec_word state =
  (* Stack layout: [cmd; <optional Int depth>; arg1; arg2; ...] *)
  (* Check if item after command is an Int (optional depth parameter) *)
  let depth_limit, stack' =
    match state.Types.stack with
    | String cmd :: Int n :: rest -> Some n, String cmd :: rest
    | _ -> None, state.stack
  in

  (* Separate Output values (for stdin) from String values (for args) *)
  (* First, extract the command *)
  let cmd, stack'' =
    match stack' with
    | String c :: rest -> c, rest
    | _ -> failwith "exec: no command"
  in

  (* Now collect arguments up to depth limit (if specified) *)
  let rec collect acc_strings acc_outputs count = function
    | String s :: rest when Option.is_none depth_limit || count < Option.value_exn depth_limit ->
      collect (s :: acc_strings) acc_outputs (count + 1) rest
    | Int i :: rest when Option.is_none depth_limit || count < Option.value_exn depth_limit ->
      (* Convert integers to strings for use as command arguments *)
      collect (Int.to_string i :: acc_strings) acc_outputs (count + 1) rest
    | (String _ | Int _) :: _ as rest when Option.is_some depth_limit ->
      (* Hit depth limit, stop collecting *)
      List.rev acc_strings, List.rev acc_outputs, rest
    | Output o :: rest ->
      collect acc_strings (o :: acc_outputs) count rest
    | rest ->
      List.rev acc_strings, List.rev acc_outputs, rest
  in

  let cmd_args, outputs, remaining = collect [] [] 0 stack'' in

  (* Concatenate all outputs to create stdin *)
  let stdin_data = String.concat ~sep:"" outputs in
  let has_stdin = not (String.is_empty stdin_data) in

  (* Build the command *)
  let cmd_with_args = String.concat ~sep:" " (cmd :: cmd_args) in
  let full_cmd =
    if has_stdin then
      sprintf "echo -n %s | %s" (Filename.quote stdin_data) cmd_with_args
    else
      cmd_with_args
  in

  let output, exit_code =
    try
      let ic = Core_unix.open_process_in full_cmd in
      let result = In_channel.input_all ic in
      let status = Core_unix.close_process_in ic in
      let code =
        match status with
        | Ok () -> 0
        | Error (`Exit_non_zero n) -> n
        | Error (`Signal s) -> 128 + Signal_unix.to_system_int s
      in
      result, code
    with
    | e -> sprintf "Error: %s" (Exn.to_string e), 127
  in
  state.last_exit_code <- exit_code;
  state.stack <- Output output :: remaining;
  return ()
;;

(* Push last exit code onto stack *)
let exit_code_word state =
  state.stack <- Int state.last_exit_code :: state.stack;
  return ()
;;

(* ========== Environment Variable Builtins ========== *)

(* Get environment variable: key -> value *)
let getenv_word = function
  | [] -> Errors.stack_underflow "getenv"
  | String key :: rest ->
      (match Sys.getenv key with
       | Some value -> return (String value :: rest)
       | None -> return (String "" :: rest))
  | Output _ :: _ | Int _ :: _ ->
      Errors.requires_type ~op:"getenv" ~typ:"string"
;;

(* Set environment variable: value key -> *)
let setenv_word = function
  | [] | [_] -> Errors.stack_underflow "setenv"
  | String key :: String value :: rest ->
      Core_unix.putenv ~key ~data:value;
      return rest
  | _ -> Errors.requires_type ~op:"setenv" ~typ:"two strings (value key)"
;;

(* Unset environment variable: key -> *)
let unsetenv_word = function
  | [] -> Errors.stack_underflow "unsetenv"
  | String key :: rest ->
      Core_unix.unsetenv key;
      return rest
  | _ -> Errors.requires_type ~op:"unsetenv" ~typ:"string"
;;

(* Helper: modify colon-separated environment variable with custom combiner *)
let modify_env_var ~op ~combiner = function
  | [] | [_] -> Errors.stack_underflow op
  | String key :: String value :: rest ->
      let new_value =
        match Sys.getenv key with
        | Some existing -> combiner existing value
        | None -> value
      in
      Core_unix.putenv ~key ~data:new_value;
      return rest
  | _ -> Errors.requires_type ~op ~typ:"two strings (value key)"
;;

(* Append to colon-separated environment variable: value key -> *)
let env_append stack =
  modify_env_var ~op:"env-append" ~combiner:(fun existing value -> existing ^ ":" ^ value) stack
;;

(* Prepend to colon-separated environment variable: value key -> *)
let env_prepend stack =
  modify_env_var ~op:"env-prepend" ~combiner:(fun existing value -> value ^ ":" ^ existing) stack
;;

(* Push all environment variables onto stack *)
let env_all stack =
  let env_array = Core_unix.environment () in
  return (Array.fold_right env_array ~init:stack ~f:(fun entry acc ->
    String entry :: acc))
;;

(* ========== Directory Navigation ========== *)

(* Expand tilde in path *)
let expand_tilde path =
  if String.is_prefix path ~prefix:"~" then
    match Sys.getenv "HOME" with
    | Some home -> home ^ String.drop_prefix path 1
    | None -> path
  else
    path
;;

(* Change directory: path -> *)
let cd_word state =
  match state.Types.stack with
  | [] -> Errors.stack_underflow "cd"
  | String path :: rest ->
      let expanded_path = expand_tilde path in
      Errors.with_unix_error ~op:"cd" (fun () ->
        Core_unix.chdir expanded_path;
        state.stack <- rest;
        return ())
  | _ -> Errors.requires_type ~op:"cd" ~typ:"string"
;;

(* Push current directory and change to new one: path -> *)
let pushd_word state =
  match state.Types.stack with
  | [] -> Errors.stack_underflow "pushd"
  | String path :: rest ->
      let current = Core_unix.getcwd () in
      let expanded_path = expand_tilde path in
      Errors.with_unix_error ~op:"pushd" (fun () ->
        Core_unix.chdir expanded_path;
        state.dir_stack <- current :: state.dir_stack;
        state.stack <- rest;
        return ())
  | _ -> Errors.requires_type ~op:"pushd" ~typ:"string"
;;

(* Pop directory from stack and change to it *)
let popd_word state =
  match state.Types.dir_stack with
  | [] -> failwith "popd: directory stack empty"
  | dir :: rest ->
      Errors.with_unix_error ~op:"popd" (fun () ->
        Core_unix.chdir dir;
        state.dir_stack <- rest;
        return ())
;;

(* Exit from current word definition *)
let exit_word _state =
  raise Types.Word_exit
;;

(* ========== Registration ========== *)

let register dict =
  let reg = Builtins_base.register_builtin dict in
  let reg_state = Builtins_base.register_state_builtin dict in
  (* Execution *)
  reg_state ~name:"exec" ~fn:exec_word ~doc:"( args... cmd -- output ) Execute shell command with arguments";
  reg_state ~name:"?" ~fn:exit_code_word ~doc:"( -- code ) Push exit code of last command";
  (* Control flow *)
  reg_state ~name:"exit" ~fn:exit_word ~doc:"( -- ) Exit from current word definition";
  (* Environment *)
  reg ~name:"getenv" ~fn:getenv_word ~doc:"( key -- value ) Get environment variable";
  reg ~name:"setenv" ~fn:setenv_word ~doc:"( key value -- ) Set environment variable";
  reg ~name:"unsetenv" ~fn:unsetenv_word ~doc:"( key -- ) Unset environment variable";
  reg ~name:"env-append" ~fn:env_append ~doc:"( key value -- ) Append to colon-separated env var";
  reg ~name:"env-prepend" ~fn:env_prepend ~doc:"( key value -- ) Prepend to colon-separated env var";
  reg ~name:"env" ~fn:env_all ~doc:"( -- vars... ) Push all environment variables";
  (* Directory navigation *)
  reg_state ~name:"cd" ~fn:cd_word ~doc:"( path -- ) Change directory";
  reg_state ~name:"pushd" ~fn:pushd_word ~doc:"( path -- ) Push current dir and change to path";
  reg_state ~name:"popd" ~fn:popd_word ~doc:"( -- ) Pop and change to directory from stack"
;;
