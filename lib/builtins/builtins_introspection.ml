open Core
open Lwt
open Types

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
            let process = Lwt_process.open_process_in ("", [| Config.shell_executable; "-c"; cmd |]) in
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

(* ========== Prompt Helpers ========== *)

(* Format stack state like current prompt: [2], [:1], [1:1], or empty *)
let prompt_stack state =
  (* During prompt evaluation, use the saved original stack; otherwise use current stack *)
  let stack_to_inspect =
    match state.prompt_eval_original_stack with
    | Some original -> original
    | None -> state.stack
  in
  let count_inputs, count_outputs =
    List.fold_left stack_to_inspect ~init:(0, 0) ~f:(fun (ins, outs) v ->
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
  let stack_to_inspect =
    match state.prompt_eval_original_stack with
    | Some original -> original
    | None -> state.stack
  in
  let count =
    List.count stack_to_inspect ~f:(function
      | Types.String _ | Types.Int _ -> true
      | Types.Output _ -> false)
  in
  state.stack <- Types.Int count :: state.stack;
  return ()
;;

(* Count of output items on stack *)
let prompt_out state =
  let stack_to_inspect =
    match state.prompt_eval_original_stack with
    | Some original -> original
    | None -> state.stack
  in
  let count =
    List.count stack_to_inspect ~f:(function
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

(* ========== Registration ========== *)

let register dict =
  let reg_state = Builtins_base.register_state_builtin dict in
  (* Help system *)
  reg_state ~name:"words" ~fn:words_word ~doc:"List all available words";
  reg_state ~name:"help" ~fn:help_word ~doc:"Show comprehensive help information";
  (* Prompt helpers - for customizable $prompt *)
  reg_state ~name:"$stack" ~fn:prompt_stack ~doc:"( -- str ) Formatted stack state [n:m]";
  reg_state ~name:"$in" ~fn:prompt_in ~doc:"( -- n ) Count of input items on stack";
  reg_state ~name:"$out" ~fn:prompt_out ~doc:"( -- n ) Count of output items on stack";
  reg_state ~name:"$gitbranch" ~fn:prompt_gitbranch ~doc:"( -- str ) Current git branch or empty";
  reg_state ~name:"$cwd" ~fn:prompt_cwd ~doc:"( -- str ) Current working directory";
  reg_state ~name:"$basename" ~fn:prompt_basename ~doc:"( -- str ) Current directory name";
  reg_state ~name:"$hostname" ~fn:prompt_hostname ~doc:"( -- str ) System hostname";
  reg_state ~name:"$username" ~fn:prompt_username ~doc:"( -- str ) Current username";
  reg_state ~name:"$exitcode" ~fn:prompt_exitcode ~doc:"( -- str ) Last command exit code";
  reg_state ~name:"$time" ~fn:prompt_time ~doc:"( -- str ) Current time HH:MM"
;;
