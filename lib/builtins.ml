open Core
open Lwt
open Types

(* ========== Error Message Helpers ========== *)

let stack_underflow op = failwith (sprintf "Stack underflow: %s" op)
let requires_type op typ = failwith (sprintf "%s: requires %s" op typ)
let div_by_zero op = failwith (sprintf "%s: division by zero" op)

(* ========== Stack Manipulation ========== *)
let dup = function
  | [] -> stack_underflow "dup"
  | x :: rest -> return (x :: x :: rest)
;;

let swap = function
  | [] | [ _ ] -> stack_underflow "swap"
  | x :: y :: rest -> return (y :: x :: rest)
;;

let drop = function
  | [] -> stack_underflow "drop"
  | _ :: rest -> return rest
;;

let clear _ = return []
;;

let over = function
  | [] | [ _ ] -> stack_underflow "over"
  | x :: y :: rest -> return (y :: x :: y :: rest)
;;

let rot = function
  | [] | [ _ ] | [ _; _ ] -> stack_underflow "rot"
  | x :: y :: z :: rest -> return (z :: x :: y :: rest)
;;

(* Print top of stack *)
let dot = function
  | [] -> stack_underflow "."
  | String s :: rest ->
    Lwt_io.printl s >>= fun () ->
    Lwt_io.flush Lwt_io.stdout >>= fun () ->
    return rest
  | Output s :: rest ->
    Lwt_io.printl s >>= fun () ->
    Lwt_io.flush Lwt_io.stdout >>= fun () ->
    return rest
  | Int i :: rest ->
    Lwt_io.printlf "%d" i >>= fun () ->
    Lwt_io.flush Lwt_io.stdout >>= fun () ->
    return rest
;;

(* Type without newline *)
let type_word = function
  | [] -> stack_underflow "type"
  | String s :: rest ->
    Lwt_io.printf "%s" s >>= fun () ->
    Lwt_io.flush Lwt_io.stdout >>= fun () ->
    return rest
  | Output s :: rest ->
    Lwt_io.printf "%s" s >>= fun () ->
    Lwt_io.flush Lwt_io.stdout >>= fun () ->
    return rest
  | Int i :: rest ->
    Lwt_io.printf "%d" i >>= fun () ->
    Lwt_io.flush Lwt_io.stdout >>= fun () ->
    return rest
;;

(* Stack display *)
let dot_s stack =
  Lwt_io.printf "<%d> " (List.length stack) >>= fun () ->
  Lwt_list.iter_s (function
    | String s -> Lwt_io.printf "\"%s\" " s
    | Output s -> Lwt_io.printf "«%s» " s
    | Int i -> Lwt_io.printf "%d " i) stack >>= fun () ->
  Lwt_io.printl "" >>= fun () ->
  Lwt_io.flush Lwt_io.stdout >>= fun () ->
  return stack
;;

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

(* Convert String to Output (fake pipe) *)
let to_output = function
  | [] -> stack_underflow ">output"
  | String s :: rest -> return (Output s :: rest)
  | Output _ :: _ as stack -> return stack  (* Already an output *)
  | Int _ :: _ -> requires_type ">output" "string"
;;

(* Convert Output to String *)
let to_string = function
  | [] -> stack_underflow ">string"
  | Output s :: rest -> return (String s :: rest)
  | Int i :: rest -> return (String (Int.to_string i) :: rest)
  | String _ :: _ as stack -> return stack  (* Already a string *)
;;

(* Convert any value to String (for use as command argument) *)
let to_arg = function
  | [] -> stack_underflow ">arg"
  | String _ :: _ as stack -> return stack  (* Already a string *)
  | Int i :: rest -> return (String (Int.to_string i) :: rest)
  | Output s :: rest -> return (String s :: rest)
;;

(* ========== File Redirection ========== *)

(* Write Output to file: Output String -> *)
let write_file state =
  match state.Types.stack with
  | [] | [_] -> stack_underflow ">file"
  | String filename :: Output content :: rest ->
      Lwt.catch
        (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.output ~flags:[Core_unix.O_WRONLY; Core_unix.O_CREAT; Core_unix.O_TRUNC] filename (fun oc ->
            Lwt_io.write oc content) >>= fun () ->
          state.last_exit_code <- 0;
          state.stack <- rest;
          return ())
        (fun exn ->
          state.last_exit_code <- 1;
          failwith (sprintf ">file: %s" (Exn.to_string exn)))
  | String _ :: String _ :: _ -> requires_type ">file" "output (not string)"
  | String _ :: Int _ :: _ -> requires_type ">file" "output (not int)"
  | _ -> requires_type ">file" "filename and output"
;;

(* Append Output to file: Output String -> *)
let append_file state =
  match state.Types.stack with
  | [] | [_] -> stack_underflow ">>file"
  | String filename :: Output content :: rest ->
      Lwt.catch
        (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.output ~flags:[Core_unix.O_WRONLY; Core_unix.O_CREAT; Core_unix.O_APPEND] filename (fun oc ->
            Lwt_io.write oc content) >>= fun () ->
          state.last_exit_code <- 0;
          state.stack <- rest;
          return ())
        (fun exn ->
          state.last_exit_code <- 1;
          failwith (sprintf ">>file: %s" (Exn.to_string exn)))
  | String _ :: String _ :: _ -> requires_type ">>file" "output (not string)"
  | String _ :: Int _ :: _ -> requires_type ">>file" "output (not int)"
  | _ -> requires_type ">>file" "filename and output"
;;

(* ========== Environment Variable Builtins ========== *)

(* Get environment variable: key -> value *)
let getenv_word = function
  | [] -> stack_underflow "getenv"
  | String key :: rest ->
      (match Sys.getenv key with
       | Some value -> return (String value :: rest)
       | None -> return (String "" :: rest))
  | Output _ :: _ | Int _ :: _ ->
      requires_type "getenv" "string"
;;

(* Set environment variable: value key -> *)
let setenv_word = function
  | [] | [_] -> stack_underflow "setenv"
  | String key :: String value :: rest ->
      Core_unix.putenv ~key ~data:value;
      return rest
  | _ -> requires_type "setenv" "two strings (value key)"
;;

(* Unset environment variable: key -> *)
let unsetenv_word = function
  | [] -> stack_underflow "unsetenv"
  | String key :: rest ->
      Core_unix.unsetenv key;
      return rest
  | _ -> requires_type "unsetenv" "string"
;;

(* Append to colon-separated environment variable: value key -> *)
let env_append = function
  | [] | [_] -> stack_underflow "env-append"
  | String key :: String value :: rest ->
      let new_value =
        match Sys.getenv key with
        | Some existing -> existing ^ ":" ^ value
        | None -> value
      in
      Core_unix.putenv ~key ~data:new_value;
      return rest
  | _ -> requires_type "env-append" "two strings (value key)"
;;

(* Prepend to colon-separated environment variable: value key -> *)
let env_prepend = function
  | [] | [_] -> stack_underflow "env-prepend"
  | String key :: String value :: rest ->
      let new_value =
        match Sys.getenv key with
        | Some existing -> value ^ ":" ^ existing
        | None -> value
      in
      Core_unix.putenv ~key ~data:new_value;
      return rest
  | _ -> requires_type "env-prepend" "two strings (value key)"
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
  | [] -> stack_underflow "cd"
  | String path :: rest ->
      let expanded_path = expand_tilde path in
      (try
        Core_unix.chdir expanded_path;
        state.stack <- rest;
        return ()
       with
       | Core_unix.Unix_error (err, _, _) ->
           failwith (sprintf "cd: %s" (Core_unix.Error.message err)))
  | _ -> requires_type "cd" "string"
;;

(* Push current directory and change to new one: path -> *)
let pushd_word state =
  match state.Types.stack with
  | [] -> stack_underflow "pushd"
  | String path :: rest ->
      let current = Core_unix.getcwd () in
      let expanded_path = expand_tilde path in
      (try
        Core_unix.chdir expanded_path;
        state.dir_stack <- current :: state.dir_stack;
        state.stack <- rest;
        return ()
       with
       | Core_unix.Unix_error (err, _, _) ->
           failwith (sprintf "pushd: %s" (Core_unix.Error.message err)))
  | _ -> requires_type "pushd" "string"
;;

(* Pop directory from stack and change to it *)
let popd_word state =
  match state.Types.dir_stack with
  | [] -> failwith "popd: directory stack empty"
  | dir :: rest ->
      (try
        Core_unix.chdir dir;
        state.dir_stack <- rest;
        return ()
       with
       | Core_unix.Unix_error (err, _, _) ->
           failwith (sprintf "popd: %s" (Core_unix.Error.message err)))
;;

(* ========== Arithmetic Operators ========== *)

(* Addition: a b -> a+b *)
let add = function
  | [] | [_] -> stack_underflow "+"
  | Int b :: Int a :: rest -> return (Int (a + b) :: rest)
  | _ -> requires_type "+" "two integers"
;;

(* Subtraction: a b -> a-b *)
let sub = function
  | [] | [_] -> stack_underflow "-"
  | Int b :: Int a :: rest -> return (Int (a - b) :: rest)
  | _ -> requires_type "-" "two integers"
;;

(* Multiplication: a b -> a*b *)
let mul = function
  | [] | [_] -> stack_underflow "*"
  | Int b :: Int a :: rest -> return (Int (a * b) :: rest)
  | _ -> requires_type "*" "two integers"
;;

(* Division: a b -> a/b *)
let div = function
  | [] | [_] -> stack_underflow "/"
  | Int 0 :: _ :: _ -> div_by_zero "/"
  | Int b :: Int a :: rest -> return (Int (a / b) :: rest)
  | _ -> requires_type "/" "two integers"
;;

(* Modulo: a b -> a mod b *)
let mod_op = function
  | [] | [_] -> stack_underflow "mod"
  | Int 0 :: _ :: _ -> div_by_zero "mod"
  | Int b :: Int a :: rest -> return (Int (a mod b) :: rest)
  | _ -> requires_type "mod" "two integers"
;;

(* Division and modulo: a b -> quotient remainder *)
let divmod = function
  | [] | [_] -> stack_underflow "/mod"
  | Int 0 :: _ :: _ -> div_by_zero "/mod"
  | Int b :: Int a :: rest ->
      let quot = a / b in
      let rem = a mod b in
      return (Int rem :: Int quot :: rest)
  | _ -> requires_type "/mod" "two integers"
;;

(* Multiply then divide: a b c -> (a*b)/c *)
let muldiv = function
  | [] | [_] | [_; _] -> stack_underflow "*/"
  | Int 0 :: _ :: _ :: _ -> div_by_zero "*/"
  | Int c :: Int b :: Int a :: rest ->
      return (Int ((a * b) / c) :: rest)
  | _ -> requires_type "*/" "three integers"
;;

(* ========== Loop Index Words ========== *)

(* Get current loop index: -> i *)
let loop_i state =
  match state.Types.loop_stack with
  | [] -> failwith "i: not inside a loop"
  | loop_info :: _ ->
      (match loop_info.do_index with
       | Some idx -> state.stack <- Int idx :: state.stack; return ()
       | None -> failwith "i: loop index not available")
;;

(* Get outer loop index (for nested loops): -> j *)
let loop_j state =
  match state.Types.loop_stack with
  | [] -> failwith "j: not inside a nested loop"
  | [ _ ] -> failwith "j: not inside a nested loop"
  | _ :: outer_loop :: _ ->
      (match outer_loop.do_index with
       | Some idx -> state.stack <- Int idx :: state.stack; return ()
       | None -> failwith "j: loop index not available")
;;

(* ========== Comparison Operators ========== *)

(* Equality: a b -> 0 or 1 *)
let eq = function
  | [] | [_] -> stack_underflow "="
  | Int b :: Int a :: rest -> return (Int (if a = b then 1 else 0) :: rest)
  | String b :: String a :: rest -> return (Int (if String.equal a b then 1 else 0) :: rest)
  | _ -> requires_type "=" "two values of the same type"
;;

(* Greater than: a b -> 0 or 1 *)
let gt = function
  | [] | [_] -> stack_underflow ">"
  | Int b :: Int a :: rest -> return (Int (if a > b then 1 else 0) :: rest)
  | _ -> requires_type ">" "two integers"
;;

(* Less than: a b -> 0 or 1 *)
let lt = function
  | [] | [_] -> stack_underflow "<"
  | Int b :: Int a :: rest -> return (Int (if a < b then 1 else 0) :: rest)
  | _ -> requires_type "<" "two integers"
;;

(* Greater than or equal: a b -> 0 or 1 *)
let gte = function
  | [] | [_] -> stack_underflow ">="
  | Int b :: Int a :: rest -> return (Int (if a >= b then 1 else 0) :: rest)
  | _ -> requires_type ">=" "two integers"
;;

(* Less than or equal: a b -> 0 or 1 *)
let lte = function
  | [] | [_] -> stack_underflow "<="
  | Int b :: Int a :: rest -> return (Int (if a <= b then 1 else 0) :: rest)
  | _ -> requires_type "<=" "two integers"
;;

(* Not equal: a b -> 0 or 1 *)
let neq = function
  | [] | [_] -> stack_underflow "<>"
  | Int b :: Int a :: rest -> return (Int (if a <> b then 1 else 0) :: rest)
  | String b :: String a :: rest -> return (Int (if not (String.equal a b) then 1 else 0) :: rest)
  | _ -> requires_type "<>" "two values of the same type"
;;

(* ========== String Operations ========== *)

(* Concatenate two strings: a b -> a+b *)
let concat = function
  | [] | [_] -> stack_underflow "concat"
  | String b :: String a :: rest -> return (String (a ^ b) :: rest)
  | _ -> requires_type "concat" "two strings"
;;

(* Conditional string helpers for prompts *)

(* ?prefix: prepend separator only if string is non-empty *)
(* ( str sep -- prefixed-str ) *)
let prefix_if_nonempty = function
  | [] | [_] -> stack_underflow "?prefix"
  | String sep :: String str :: rest ->
      if String.is_empty str then
        return (String str :: rest)
      else
        return (String (sep ^ str) :: rest)
  | _ -> requires_type "?prefix" "two strings"
;;

(* ?suffix: append separator only if string is non-empty *)
(* ( str sep -- suffixed-str ) *)
let suffix_if_nonempty = function
  | [] | [_] -> stack_underflow "?suffix"
  | String sep :: String str :: rest ->
      if String.is_empty str then
        return (String str :: rest)
      else
        return (String (str ^ sep) :: rest)
  | _ -> requires_type "?suffix" "two strings"
;;

(* ?wrap: wrap with prefix and suffix only if string is non-empty *)
(* ( str prefix suffix -- wrapped-str ) *)
let wrap_if_nonempty = function
  | [] | [_] | [_; _] -> stack_underflow "?wrap"
  | String suffix :: String prefix :: String str :: rest ->
      if String.is_empty str then
        return (String str :: rest)
      else
        return (String (prefix ^ str ^ suffix) :: rest)
  | _ -> requires_type "?wrap" "three strings"
;;

(* ========== Help System ========== *)

(* List all words in the dictionary *)
let words_word state =
  let words = Hashtbl.keys state.dict |> List.sort ~compare:String.compare in
  let rec print_words = function
    | [] -> Lwt_io.printl ""
    | w :: rest ->
        Lwt_io.printf "%s " w >>= fun () ->
        print_words rest
  in
  print_words words >>= fun () ->
  return ()
;;

(* Show definition of a word - takes word name from next token *)
let see_word state word_name =
  match Hashtbl.find state.dict word_name with
  | Some (Types.Builtin (_, Some doc)) ->
      Lwt_io.printlf "%s: %s" word_name doc >>= fun () ->
      return ()
  | Some (Types.Builtin (_, None)) ->
      Lwt_io.printlf "%s is a builtin function" word_name >>= fun () ->
      return ()
  | Some (Types.StateBuiltin (_, Some doc)) ->
      Lwt_io.printlf "%s: %s" word_name doc >>= fun () ->
      return ()
  | Some (Types.StateBuiltin (_, None)) ->
      Lwt_io.printlf "%s is a builtin state function" word_name >>= fun () ->
      return ()
  | Some (Types.Defined tokens) ->
      Lwt_io.printf ": %s " word_name >>= fun () ->
      Lwt_list.iter_s (fun t -> Lwt_io.printf "%s " t) tokens >>= fun () ->
      Lwt_io.printl ";" >>= fun () ->
      return ()
  | Some (Types.ShellCmd cmd) ->
      Lwt_io.printlf "%s is a shell command: %s" word_name cmd >>= fun () ->
      return ()
  | None ->
      (* Not in dictionary - check if it's an external command and show help *)
      let try_help_cmd cmd =
        let open Lwt.Infix in
        Lwt.catch
          (fun () ->
            let process = Lwt_process.open_process_in ("", [| "/bin/sh"; "-c"; cmd |]) in
            Lwt_io.read process#stdout >>= fun output ->
            process#close >>= fun _status ->
            (* Filter out error messages that indicate --help is not supported *)
            let is_error =
              String.is_substring output ~substring:"Illegal option" ||
              String.is_substring output ~substring:"invalid option" ||
              String.is_substring output ~substring:"unrecognized option" ||
              String.length output = 0
            in
            if is_error then
              Lwt.return None
            else
              Lwt.return (Some output))
          (fun _ -> Lwt.return None)
      in
      (* Try --help first, then man *)
      try_help_cmd (word_name ^ " --help 2>&1 | head -n 30") >>= fun help_result ->
      (match help_result with
       | Some output ->
           Lwt_io.printlf "%s --help:" word_name >>= fun () ->
           Lwt_io.print output >>= fun () ->
           return ()
       | None ->
           try_help_cmd ("man " ^ word_name ^ " 2>&1 | head -n 30") >>= fun man_result ->
           (match man_result with
            | Some output ->
                Lwt_io.printlf "man %s:" word_name >>= fun () ->
                Lwt_io.print output >>= fun () ->
                return ()
            | None ->
                Lwt_io.printlf "%s is not defined" word_name >>= fun () ->
                return ()))
;;

(* Show general help *)
let help_word _state =
  Lwt_io.printl "Forth Shell - Available Commands" >>= fun () ->
  Lwt_io.printl "" >>= fun () ->
  Lwt_io.printl "Stack Operations:" >>= fun () ->
  Lwt_io.printl "  dup swap drop over rot    - manipulate stack" >>= fun () ->
  Lwt_io.printl "  .s                        - show stack contents" >>= fun () ->
  Lwt_io.printl "" >>= fun () ->
  Lwt_io.printl "Printing:" >>= fun () ->
  Lwt_io.printl "  .                         - print top of stack" >>= fun () ->
  Lwt_io.printl "  type                      - print without newline" >>= fun () ->
  Lwt_io.printl "" >>= fun () ->
  Lwt_io.printl "Arithmetic:" >>= fun () ->
  Lwt_io.printl "  + - * / mod /mod */       - math operations" >>= fun () ->
  Lwt_io.printl "  = < > <= >= <>            - comparisons" >>= fun () ->
  Lwt_io.printl "" >>= fun () ->
  Lwt_io.printl "Control Flow:" >>= fun () ->
  Lwt_io.printl "  if ... then               - conditional" >>= fun () ->
  Lwt_io.printl "  if ... else ... then      - conditional with else" >>= fun () ->
  Lwt_io.printl "  begin ... until           - loop until condition" >>= fun () ->
  Lwt_io.printl "  begin ... while ... repeat - loop while condition" >>= fun () ->
  Lwt_io.printl "  start limit do ... loop   - counted loop" >>= fun () ->
  Lwt_io.printl "  i j                       - loop indices" >>= fun () ->
  Lwt_io.printl "" >>= fun () ->
  Lwt_io.printl "Word Definition:" >>= fun () ->
  Lwt_io.printl "  : name ... ;              - define new word" >>= fun () ->
  Lwt_io.printl "" >>= fun () ->
  Lwt_io.printl "Help System:" >>= fun () ->
  Lwt_io.printl "  words                     - list all words" >>= fun () ->
  Lwt_io.printl "  see <word>                - show word definition" >>= fun () ->
  Lwt_io.printl "  help                      - show this help" >>= fun () ->
  Lwt_io.printl "" >>= fun () ->
  Lwt_io.printl "Type 'words' to see all available commands" >>= fun () ->
  return ()
;;

(* Prompt helpers - for customizable prompt with $prompt *)

(* Format stack state like current prompt: [2], [:1], [1:1], or empty *)
let prompt_stack state =
  let count_inputs, count_outputs =
    List.fold_left state.stack ~init:(0, 0) ~f:(fun (ins, outs) v ->
      match v with
      | Types.String _ | Types.Int _ -> (ins + 1, outs)
      | Types.Output _ -> (ins, outs + 1))
  in
  let indicator =
    match count_inputs, count_outputs with
    | 0, 0 -> ""
    | n, 0 -> "[" ^ Int.to_string n ^ "]"
    | 0, n -> "[:" ^ Int.to_string n ^ "]"
    | n, m -> "[" ^ Int.to_string n ^ ":" ^ Int.to_string m ^ "]"
  in
  state.stack <- Types.String indicator :: state.stack;
  return ()
;;

(* Count of input items on stack *)
let prompt_in state =
  let count =
    List.count state.stack ~f:(function
      | Types.String _ | Types.Int _ -> true
      | Types.Output _ -> false)
  in
  state.stack <- Types.Int count :: state.stack;
  return ()
;;

(* Count of output items on stack *)
let prompt_out state =
  let count =
    List.count state.stack ~f:(function
      | Types.Output _ -> true
      | Types.String _ | Types.Int _ -> false)
  in
  state.stack <- Types.Int count :: state.stack;
  return ()
;;

(* Get current git branch *)
let prompt_gitbranch state =
  let branch =
    try
      let cmd = "git branch --show-current 2>/dev/null" in
      let ic = Core_unix.open_process_in cmd in
      let line = In_channel.input_line ic in
      let _ = Core_unix.close_process_in ic in
      match line with
      | Some s -> String.strip s
      | None -> ""
    with
    | _ -> ""
  in
  state.stack <- Types.String branch :: state.stack;
  return ()
;;

(* Get current working directory *)
let prompt_cwd state =
  let cwd = Core_unix.getcwd () in
  state.stack <- Types.String cwd :: state.stack;
  return ()
;;

(* Get basename of current directory *)
let prompt_basename state =
  let cwd = Core_unix.getcwd () in
  let basename = Filename.basename cwd in
  state.stack <- Types.String basename :: state.stack;
  return ()
;;

(* Get system hostname *)
let prompt_hostname state =
  let hostname = Core_unix.gethostname () in
  state.stack <- Types.String hostname :: state.stack;
  return ()
;;

(* Get current username *)
let prompt_username state =
  let username =
    try
      Core_unix.getlogin ()
    with
    | _ ->
        (match Sys.getenv "USER" with
         | Some u -> u
         | None -> "user")
  in
  state.stack <- Types.String username :: state.stack;
  return ()
;;

(* Get last exit code *)
let prompt_exitcode state =
  let code = Int.to_string state.last_exit_code in
  state.stack <- Types.String code :: state.stack;
  return ()
;;

(* Get current time HH:MM *)
let prompt_time _state =
  let open Core_unix in
  let tm = localtime (time ()) in
  let time_str = Printf.sprintf "%02d:%02d" tm.tm_hour tm.tm_min in
  _state.stack <- Types.String time_str :: _state.stack;
  return ()
;;

(* Register all builtins in a dictionary *)
let register_builtins dict =
  let reg name fn doc = Hashtbl.set dict ~key:name ~data:(Builtin (fn, Some doc)) in
  let reg_state name fn doc = Hashtbl.set dict ~key:name ~data:(Types.StateBuiltin (fn, Some doc)) in
  (* Stack operations *)
  reg "dup" dup "( a -- a a ) Duplicate top item";
  reg "swap" swap "( a b -- b a ) Swap top two items";
  reg "drop" drop "( a -- ) Remove top item";
  reg "clear" clear "( ... -- ) Clear entire stack";
  reg "over" over "( a b -- a b a ) Copy second item to top";
  reg "rot" rot "( a b c -- b c a ) Rotate top three items";
  (* Printing *)
  reg "." dot "( a -- ) Print and remove top item with newline";
  reg "type" type_word "( a -- ) Print and remove top item without newline";
  reg ".s" dot_s "( -- ) Display entire stack without modifying it";
  (* Execution *)
  reg_state "exec" exec_word "( args... cmd -- output ) Execute shell command with arguments";
  reg_state "?" exit_code_word "( -- code ) Push exit code of last command";
  (* Type conversions *)
  reg ">output" to_output "( string -- output ) Convert String to Output for piping";
  reg ">string" to_string "( output/int -- string ) Convert Output or Int to String";
  reg ">arg" to_arg "( a -- string ) Convert any value to String for command args";
  (* File redirection *)
  reg_state ">file" write_file "( filename output -- ) Write output to file";
  reg_state ">>file" append_file "( filename output -- ) Append output to file";
  (* Environment *)
  reg "getenv" getenv_word "( key -- value ) Get environment variable";
  reg "setenv" setenv_word "( key value -- ) Set environment variable";
  reg "unsetenv" unsetenv_word "( key -- ) Unset environment variable";
  reg "env-append" env_append "( key value -- ) Append to colon-separated env var";
  reg "env-prepend" env_prepend "( key value -- ) Prepend to colon-separated env var";
  reg "env" env_all "( -- vars... ) Push all environment variables";
  (* Directory navigation *)
  reg_state "cd" cd_word "( path -- ) Change directory";
  reg_state "pushd" pushd_word "( path -- ) Push current dir and change to path";
  reg_state "popd" popd_word "( -- ) Pop and change to directory from stack";
  (* Arithmetic *)
  reg "+" add "( a b -- a+b ) Add two numbers";
  reg "-" sub "( a b -- a-b ) Subtract b from a";
  reg "*" mul "( a b -- a*b ) Multiply two numbers";
  reg "/" div "( a b -- a/b ) Divide a by b";
  reg "mod" mod_op "( a b -- a%b ) Modulo (remainder of a/b)";
  reg "/mod" divmod "( a b -- quot rem ) Quotient and remainder";
  reg "*/" muldiv "( a b c -- (a*b)/c ) Multiply then divide";
  (* Comparison *)
  reg "=" eq "( a b -- flag ) Test equality (1 if equal, 0 if not)";
  reg ">" gt "( a b -- flag ) Test greater than (1 if a>b, 0 if not)";
  reg "<" lt "( a b -- flag ) Test less than (1 if a<b, 0 if not)";
  reg ">=" gte "( a b -- flag ) Test greater or equal (1 if a>=b, 0 if not)";
  reg "<=" lte "( a b -- flag ) Test less or equal (1 if a<=b, 0 if not)";
  reg "<>" neq "( a b -- flag ) Test not equal (1 if not equal, 0 if equal)";
  (* String operations *)
  reg "concat" concat "( a b -- a+b ) Concatenate two strings";
  reg "?prefix" prefix_if_nonempty "( str sep -- result ) Prepend separator if string non-empty";
  reg "?suffix" suffix_if_nonempty "( str sep -- result ) Append separator if string non-empty";
  reg "?wrap" wrap_if_nonempty "( str pre suf -- result ) Wrap with prefix/suffix if non-empty";
  (* Loop indices *)
  reg_state "i" loop_i "( -- index ) Push current loop index";
  reg_state "j" loop_j "( -- index ) Push outer loop index (nested loops)";
  (* Help system *)
  reg_state "words" words_word "List all available words";
  reg_state "help" help_word "Show comprehensive help information";
  (* Prompt helpers - for customizable $prompt *)
  reg_state "$stack" prompt_stack "( -- str ) Formatted stack state [n:m]";
  reg_state "$in" prompt_in "( -- n ) Count of input items on stack";
  reg_state "$out" prompt_out "( -- n ) Count of output items on stack";
  reg_state "$gitbranch" prompt_gitbranch "( -- str ) Current git branch or empty";
  reg_state "$cwd" prompt_cwd "( -- str ) Current working directory";
  reg_state "$basename" prompt_basename "( -- str ) Current directory name";
  reg_state "$hostname" prompt_hostname "( -- str ) System hostname";
  reg_state "$username" prompt_username "( -- str ) Current username";
  reg_state "$exitcode" prompt_exitcode "( -- str ) Last command exit code";
  reg_state "$time" prompt_time "( -- str ) Current time HH:MM"
;;
