open Core
open Lwt
open Types

(* Check if file exists and is executable *)
let is_executable path =
  match Core_unix.access path [`Exists; `Exec] with
  | Ok () -> true
  | Error _ -> false
;;

(* Find command in PATH, return absolute path if found *)
let find_in_path cmd =
  (* Handle absolute paths directly *)
  if String.is_prefix cmd ~prefix:"/" then
    if is_executable cmd then Some cmd else None
  (* Handle relative paths with / (e.g., ./script) *)
  else if String.contains cmd '/' then
    let abs_path = Filename.concat (Core_unix.getcwd ()) cmd in
    if is_executable abs_path then Some abs_path else None
  (* Search PATH *)
  else
    match Sys.getenv "PATH" with
    | None -> None
    | Some path_str ->
        let paths = String.split path_str ~on:':' in
        List.find_map paths ~f:(fun dir ->
          let full_path = Filename.concat dir cmd in
          if is_executable full_path then Some full_path else None)
;;

(* Check if a string contains glob characters *)
let has_glob_chars s =
  String.contains s '*' || String.contains s '?' || String.contains s '['
;;

(* Expand glob pattern to list of matching files *)
let expand_glob pattern =
  (* Split pattern into directory and filename parts *)
  let dir, file_pattern =
    match String.rsplit2 pattern ~on:'/' with
    | Some (d, f) -> d, f
    | None -> ".", pattern
  in

  (* Simple glob matching: * matches anything, ? matches one char *)
  let glob_to_regex pat =
    let escaped = Str.quote pat in
    let with_star = Str.global_replace (Str.regexp_string "\\*") ".*" escaped in
    let with_question = Str.global_replace (Str.regexp_string "\\?") "." with_star in
    Str.regexp ("^" ^ with_question ^ "$")
  in

  try
    let regex = glob_to_regex file_pattern in
    (* Read directory entries *)
    let dh = Core_unix.opendir dir in
    let rec read_entries acc =
      match Core_unix.readdir_opt dh with
      | None -> acc
      | Some entry -> read_entries (entry :: acc)
    in
    let entries = read_entries [] in
    Core_unix.closedir dh;
    (* Filter and sort matches *)
    let matches = List.filter entries ~f:(fun entry ->
      Str.string_match regex entry 0) in
    let sorted = List.sort matches ~compare:String.compare in
    (* Prepend directory if not current dir *)
    if String.equal dir "." then
      sorted
    else
      List.map sorted ~f:(fun f -> dir ^ "/" ^ f)
  with
  | Core_unix.Unix_error _ -> []
;;

(* Execute a list of tokens (for loops) - now returns Lwt promise *)
let rec execute_tokens state tokens =
  Lwt_list.iter_s (fun t -> eval_token state (t, false)) tokens

(* ========== Token Evaluation Handlers ========== *)

(* Handle each...then collection *)
and handle_each_collection state token is_quoted output_content body =
  if String.equal token "then" then (
    (* End of each...then - execute body for each line *)
    let lines = String.split_lines output_content in
    let body_tokens = List.rev body in
    state.collecting_each <- None;
    Lwt_list.iter_s
      (fun line ->
        (* Push line onto stack *)
        state.stack <- String line :: state.stack;
        (* Execute body tokens with preserved quote info *)
        Lwt_list.iter_s (fun (t, q) -> eval_token state (t, q)) body_tokens)
      lines)
  else (
    (* Add token to body with quote info *)
    state.collecting_each <- Some (output_content, (token, is_quoted) :: body);
    return ())

(* Handle loop body collection - delegates to Loop_execution module *)
and handle_loop_collection state token loop_type body depth =
  Loop_execution.handle_loop_collection execute_tokens state token loop_type body depth

(* Handle word definition collection *)
and handle_word_definition state token name =
  (* Building a word definition - just store tokens *)
  if String.equal token ";" then (
    (* End definition *)
    Hashtbl.set state.dict ~key:name ~data:(Defined (List.rev state.def_body));
    state.defining <- None;
    state.def_body <- [];
    return ())
  else (
    (* Add token to definition - store as unquoted *)
    state.def_body <- token :: state.def_body;
    return ())

(* Handle control flow skipping state *)
and handle_control_flow_skipping state token target depth =
  (* We're skipping tokens *)
  if String.equal token "if" then (
    (* Nested if - increase depth *)
    state.control_flow <- Types.Skipping { target; depth = depth + 1 };
    return ())
  else if String.equal token "then" then (
    (* Found then *)
    if depth = 0 then
      state.control_flow <- Types.Normal  (* Exit control flow *)
    else
      state.control_flow <- Types.Skipping { target; depth = depth - 1 };
    return ())
  else if String.equal token "else" then (
    (* Found else *)
    (match target, depth with
     | `Else, 0 ->
         (* Switch to executing the else branch *)
         state.control_flow <- Types.Normal
     | `Then, 0 ->
         (* Stay skipping, this else is at our level but we already executed if branch *)
         ()
     | _ -> ());  (* Nested else, ignore *)
    return ())
  else
    return ()

(* Handle control flow keywords (if/then/else/begin/do/each/:)
   Returns Some result if keyword was handled, None otherwise *)
and handle_control_flow_keywords state token =
  if String.equal token "if" then (
    (* if: pop condition from stack *)
    match state.stack with
    | [] -> failwith "if: stack underflow"
    | Int 0 :: rest ->
        (* Condition is false, skip to else or then *)
        state.stack <- rest;
        state.control_flow <- Types.Skipping { target = `Else; depth = 0 };
        Some (return ())
    | Int _ :: rest ->
        (* Condition is true, continue normally *)
        state.stack <- rest;
        state.control_flow <- Types.Normal;
        Some (return ())
    | _ -> failwith "if: requires integer on stack")
  else if String.equal token "else" then (
    (* else: we executed the if branch, now skip to then *)
    state.control_flow <- Types.Skipping { target = `Then; depth = 0 };
    Some (return ()))
  else if String.equal token "then" then (
    (* then: end of if/then (no else branch) *)
    state.control_flow <- Types.Normal;
    Some (return ()))
  else if String.equal token "begin" then (
    (* Start begin...until or begin...while...repeat loop *)
    (* We don't know which type yet, collect tokens *)
    state.collecting_loop <- Some (Types.BeginUntil, [], 0);
    Some (return ()))
  else if String.equal token "until" then
    (* until without begin *)
    failwith "until: no matching begin"
  else if String.equal token "repeat" then
    (* repeat without begin *)
    failwith "repeat: no matching begin"
  else if String.equal token "do" then (
    (* Start do...loop or do...+loop *)
    state.collecting_loop <- Some (Types.DoLoop, [], 0);
    Some (return ()))
  else if String.equal token "loop" then
    (* loop without do *)
    failwith "loop: no matching do"
  else if String.equal token "+loop" then
    (* +loop without do *)
    failwith "+loop: no matching do"
  else if String.equal token "each" then (
    (* Start each...then - pop Output from stack and start collecting *)
    match state.stack with
    | [] -> failwith "each: stack underflow"
    | Output content :: rest ->
        state.stack <- rest;
        state.collecting_each <- Some (content, []);
        Some (return ())
    | _ -> failwith "each: requires Output on stack")
  else if String.equal token ":" then (
    (* Start word definition - next token is the name *)
    state.defining <- Some "UNNAMED";
    Some (return ()))
  else if String.equal token "see" then (
    (* see word - next token is the word to inspect *)
    (* This is a placeholder - see will be handled specially in eval_line *)
    Some (return ()))
  else
    (* Not a control flow keyword, delegate to execution handler *)
    None

(* Handle token execution (integers, dictionary lookup, PATH lookup, globs) *)
and handle_token_execution state token is_quoted =
  if Tokenizer.is_int token then (
    (* Push integer *)
    state.stack <- Int (Int.of_string token) :: state.stack;
    return ())
  else (
    (* Look up in dictionary *)
    match Hashtbl.find state.dict token with
         | Some (Builtin (fn, _)) ->
             fn state.stack >>= fun new_stack ->
             state.stack <- new_stack;
             return ()
         | Some (StateBuiltin (fn, _)) ->
             fn state >>= fun () ->
             return ()
         | Some (Defined tokens) ->
             (* Execute defined words - treat as unquoted *)
             (* Catch Word_exit for early return from word definitions *)
             Lwt.catch
               (fun () -> Lwt_list.iter_s (fun t -> eval_token state (t, false)) tokens)
               (function
                 | Types.Word_exit -> return ()  (* Early exit, continue normally *)
                 | exn -> Lwt.fail exn)  (* Re-raise other exceptions *)
         | Some (ShellCmd cmd) ->
             (* Push command as string and exec *)
             state.stack <- String cmd :: state.stack;
             Builtins.exec_word state >>= fun () ->
             return ()
    | None ->
        (* Not in dictionary *)
        if is_quoted then (
          (* Quoted string - push as literal, no PATH lookup *)
          state.stack <- String token :: state.stack;
          return ())
        else (
          (* Unquoted - try PATH lookup *)
          match find_in_path token with
          | Some full_path ->
              (* Found in PATH, execute it *)
              state.stack <- String full_path :: state.stack;
              Builtins.exec_word state >>= fun () ->
              return ()
          | None ->
              (* Not found in PATH, push as string literal *)
              (* Expand globs if contains glob chars *)
              if has_glob_chars token then (
                let matches = expand_glob token in
                if not (List.is_empty matches) then
                  (* Push matches in order (they'll be reversed on stack) *)
                  List.iter matches ~f:(fun m -> state.stack <- String m :: state.stack);
                (* If no matches, push nothing *)
                return ())
              else (
                (* Not a glob pattern, push as literal *)
                state.stack <- String token :: state.stack;
                return ())))

(* Main eval_token function - dispatches to appropriate handler *)
and eval_token state (token, is_quoted) : unit Lwt.t =
  (* Check if we're collecting an each...then body *)
  match state.collecting_each with
  | Some (output_content, body) ->
      handle_each_collection state token is_quoted output_content body
  | None ->
      (* Check if we're collecting a loop body *)
      match state.collecting_loop with
      | Some (loop_type, body, depth) ->
          handle_loop_collection state token loop_type body depth
      | None ->
          (* Not collecting loop, check if we're defining a word *)
          match state.defining with
          | Some name ->
              handle_word_definition state token name
          | None ->
              (* Not defining, handle control flow *)
              match state.control_flow with
              | Types.Skipping { target; depth } ->
                  handle_control_flow_skipping state token target depth
              | Types.Normal ->
                  (* Not skipping, check for control flow keywords *)
                  match handle_control_flow_keywords state token with
                  | Some result -> result
                  | None -> handle_token_execution state token is_quoted

(* Evaluate a line of input *)
let eval_line state line =
  let tokens = Tokenizer.tokenize line in
  (* Handle special commands that read next token *)
  match tokens with
  | (":", _) :: (name, _) :: rest ->
    (* Word definition *)
    state.defining <- Some name;
    state.def_body <- [];
    Lwt_list.iter_s (eval_token state) rest
  | ("see", _) :: (word_name, _) :: _ ->
    (* see command - show word definition *)
    Builtins.see_word state word_name
  | ("see", _) :: [] ->
    (* see without argument *)
    Lwt_io.printl "Usage: see <word>" >>= fun () ->
    return ()
  | _ -> Lwt_list.iter_s (eval_token state) tokens
;;
