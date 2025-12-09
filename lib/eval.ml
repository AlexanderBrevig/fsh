open Core
open Lwt
open Types

(* Check if a string is an integer *)
let is_int s =
  try
    let _ = Int.of_string s in
    true
  with
  | _ -> false
;;

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

(* Tokenize with quote awareness *)
let tokenize line =
  let rec tokenize_helper pos in_quote current_token acc =
    if pos >= String.length line then
      (* End of line *)
      let final_acc =
        if String.is_empty current_token then acc
        else (current_token, in_quote) :: acc
      in
      List.rev final_acc
    else
      let c = line.[pos] in
      match c with
      | '"' when not in_quote ->
          (* Start quote *)
          let new_acc =
            if String.is_empty current_token then acc
            else (current_token, false) :: acc
          in
          tokenize_helper (pos + 1) true "" new_acc
      | '"' when in_quote ->
          (* End quote *)
          tokenize_helper (pos + 1) false "" ((current_token, true) :: acc)
      | (' ' | '\n' | '\r' | '\t') when not in_quote ->
          (* Whitespace outside quotes - token separator *)
          let new_acc =
            if String.is_empty current_token then acc
            else (current_token, false) :: acc
          in
          tokenize_helper (pos + 1) false "" new_acc
      | _ ->
          (* Regular character *)
          tokenize_helper (pos + 1) in_quote (current_token ^ String.make 1 c) acc
  in
  tokenize_helper 0 false "" []
;;

(* Execute a list of tokens (for loops) - now returns Lwt promise *)
let rec execute_tokens state tokens =
  Lwt_list.iter_s (fun t -> eval_token state (t, false)) tokens

(* Evaluate a single token with quote awareness - now returns Lwt promise *)
and eval_token state (token, is_quoted) : unit Lwt.t =
  (* Check if we're collecting an each...then body *)
  match state.collecting_each with
  | Some (output_content, body) ->
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
  | None ->
      (* Check if we're collecting a loop body *)
      match state.collecting_loop with
      | Some (loop_type, body, depth) ->
      (* We're inside a loop definition, collect tokens *)
      (match token, loop_type, depth with
       | "until", Types.BeginUntil, 0 ->
           (* End of begin...until loop (not nested) *)
           let loop_body = List.rev body in
           state.collecting_loop <- None;
           (* Execute loop body at least once, then check condition *)
           let rec loop_until () =
             execute_tokens state loop_body >>= fun () ->
             (* Check condition on stack *)
             match state.stack with
             | Int 0 :: rest ->
                 (* Condition false, continue looping *)
                 state.stack <- rest;
                 loop_until ()
             | Int _ :: rest ->
                 (* Condition true, exit loop *)
                 state.stack <- rest;
                 return ()
             | [] -> failwith "until: stack underflow (needs condition)"
             | _ :: _ -> failwith "until: requires integer condition"
           in
           loop_until ()
       | "until", Types.BeginUntil, _ ->
           (* Nested until, add to body and decrement depth *)
           state.collecting_loop <- Some (loop_type, token :: body, depth - 1);
           return ()
       | "while", Types.BeginUntil, 0 ->
           (* This is actually a begin...while...repeat loop, not begin...until *)
           state.collecting_loop <- Some (Types.BeginWhile, "while" :: body, 0);
           return ()
       | "while", Types.BeginWhile, _ ->
           (* In while mode, just add token *)
           state.collecting_loop <- Some (loop_type, "while" :: body, depth);
           return ()
       | "repeat", Types.BeginWhile, 0 ->
           (* End of begin...while...repeat loop (not nested) *)
           let all_tokens = List.rev body in
           (* Find where 'while' is to split the loop *)
           let rec find_while acc = function
             | [] -> failwith "repeat: no matching while"
             | "while" :: rest -> List.rev acc, rest
             | token :: rest -> find_while (token :: acc) rest
           in
           let before_while, after_while = find_while [] all_tokens in
           state.collecting_loop <- None;
           (* Execute: before_while (check condition) while after_while repeat *)
           let rec loop_while () =
             execute_tokens state before_while >>= fun () ->
             (* Check condition *)
             match state.stack with
             | Int 0 :: rest ->
                 (* Condition false, exit loop *)
                 state.stack <- rest;
                 return ()
             | Int _ :: rest ->
                 (* Condition true, execute body and repeat *)
                 state.stack <- rest;
                 execute_tokens state after_while >>= fun () ->
                 loop_while ()
             | [] -> failwith "while: stack underflow (needs condition)"
             | _ :: _ -> failwith "while: requires integer condition"
           in
           loop_while ()
       | "repeat", Types.BeginWhile, _ ->
           (* Nested repeat, add to body and decrement depth *)
           state.collecting_loop <- Some (loop_type, token :: body, depth - 1);
           return ()
       | "loop", (Types.DoLoop | Types.DoPlusLoop), 0 ->
           (* End of do...loop (not nested) *)
           let loop_body = List.rev body in
           state.collecting_loop <- None;
           (* Pop start and limit from stack *)
           (match state.stack with
            | Int limit :: Int start :: rest ->
                state.stack <- rest;
                (* Execute loop from start to limit-1 *)
                let rec loop_do idx =
                  if idx < limit then (
                    (* Push loop info onto loop stack *)
                    let loop_info = { Types.start_tokens = loop_body
                                    ; loop_type = Types.DoLoop
                                    ; do_start = Some start
                                    ; do_limit = Some limit
                                    ; do_index = Some idx
                                    } in
                    state.loop_stack <- loop_info :: state.loop_stack;
                    execute_tokens state loop_body >>= fun () ->
                    state.loop_stack <- List.tl_exn state.loop_stack;
                    loop_do (idx + 1))
                  else
                    return ()
                in
                loop_do start
            | _ -> failwith "do: stack underflow (needs start and limit)")
       | "loop", (Types.DoLoop | Types.DoPlusLoop), _ ->
           (* Nested loop, add to body and decrement depth *)
           state.collecting_loop <- Some (loop_type, token :: body, depth - 1);
           return ()
       | "+loop", Types.DoPlusLoop, 0 ->
           (* End of do...+loop with step (not nested) *)
           let loop_body = List.rev body in
           state.collecting_loop <- None;
           (* Pop start and limit from stack *)
           (match state.stack with
            | Int limit :: Int start :: rest ->
                state.stack <- rest;
                (* Execute loop with dynamic step *)
                let rec loop_do_step idx =
                  (* Check if we should continue (ascending or descending) *)
                  let should_continue =
                    if start < limit then idx < limit
                    else idx > limit
                  in
                  if should_continue then (
                    (* Push loop info onto loop stack *)
                    let loop_info = { Types.start_tokens = loop_body
                                    ; loop_type = Types.DoPlusLoop
                                    ; do_start = Some start
                                    ; do_limit = Some limit
                                    ; do_index = Some idx
                                    } in
                    state.loop_stack <- loop_info :: state.loop_stack;
                    execute_tokens state loop_body >>= fun () ->
                    state.loop_stack <- List.tl_exn state.loop_stack;
                    (* Get step from stack *)
                    match state.stack with
                    | Int step :: rest ->
                        state.stack <- rest;
                        loop_do_step (idx + step)
                    | _ -> failwith "+loop: stack underflow (needs step)")
                  else
                    return ()
                in
                loop_do_step start
            | _ -> failwith "do: stack underflow (needs start and limit)")
       | "+loop", Types.DoPlusLoop, _ ->
           (* Nested +loop, add to body and decrement depth *)
           state.collecting_loop <- Some (loop_type, token :: body, depth - 1);
           return ()
       | "+loop", Types.DoLoop, 0 ->
           (* This is actually a do...+loop, change type (not nested) *)
           let loop_body = List.rev body in
           state.collecting_loop <- None;
           (* Pop start and limit from stack *)
           (match state.stack with
            | Int limit :: Int start :: rest ->
                state.stack <- rest;
                (* Execute loop with dynamic step *)
                let rec loop_do_step idx =
                  (* Check if we should continue (ascending or descending) *)
                  let should_continue =
                    if start < limit then idx < limit
                    else idx > limit
                  in
                  if should_continue then (
                    (* Push loop info onto loop stack *)
                    let loop_info = { Types.start_tokens = loop_body
                                    ; loop_type = Types.DoPlusLoop
                                    ; do_start = Some start
                                    ; do_limit = Some limit
                                    ; do_index = Some idx
                                    } in
                    state.loop_stack <- loop_info :: state.loop_stack;
                    execute_tokens state loop_body >>= fun () ->
                    state.loop_stack <- List.tl_exn state.loop_stack;
                    (* Get step from stack *)
                    match state.stack with
                    | Int step :: rest ->
                        state.stack <- rest;
                        loop_do_step (idx + step)
                    | _ -> failwith "+loop: stack underflow (needs step)")
                  else
                    return ()
                in
                loop_do_step start
            | _ -> failwith "do: stack underflow (needs start and limit)")
       | "+loop", Types.DoLoop, _ ->
           (* Nested +loop in BeginUntil or other context, add to body and decrement depth *)
           state.collecting_loop <- Some (loop_type, token :: body, depth - 1);
           return ()
       | "begin", _, _ ->
           (* Nested begin in loop body, increment depth *)
           state.collecting_loop <- Some (loop_type, token :: body, depth + 1);
           return ()
       | "do", _, _ ->
           (* Nested do in loop body, increment depth *)
           state.collecting_loop <- Some (loop_type, token :: body, depth + 1);
           return ()
       | _, _, _ ->
           (* Regular token, add to body *)
           state.collecting_loop <- Some (loop_type, token :: body, depth);
           return ())
  | None ->
      (* Not collecting loop, check if we're defining a word first *)
      (match state.defining with
   | Some name ->
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
   | None ->
       (* Not defining, handle control flow keywords *)
       match state.control_flow with
   | Types.SkippingToElse depth | Types.SkippingToThen depth ->
       (* We're skipping tokens *)
       if String.equal token "if" then (
         (* Nested if - increase depth *)
         state.control_flow <- (match state.control_flow with
           | Types.SkippingToElse _ -> Types.SkippingToElse (depth + 1)
           | Types.SkippingToThen _ -> Types.SkippingToThen (depth + 1)
           | _ -> state.control_flow);
         return ())
       else if String.equal token "then" then (
         (* Found then *)
         if depth = 0 then
           state.control_flow <- Types.Normal  (* Exit control flow *)
         else
           state.control_flow <- (match state.control_flow with
             | Types.SkippingToElse _ -> Types.SkippingToElse (depth - 1)
             | Types.SkippingToThen _ -> Types.SkippingToThen (depth - 1)
             | _ -> state.control_flow);
         return ())
       else if String.equal token "else" then (
         (* Found else *)
         (match state.control_flow with
          | Types.SkippingToElse d when d = 0 ->
              (* Switch to executing the else branch *)
              state.control_flow <- Types.Normal
          | Types.SkippingToThen d when d = 0 ->
              (* Stay skipping, this else is at our level but we already executed if branch *)
              ()
          | _ -> ());  (* Nested else, ignore *)
         return ())
       else
         return ()
   | Types.Normal ->
       (* Not skipping, check for control flow keywords *)
       if String.equal token "if" then (
         (* if: pop condition from stack *)
         match state.stack with
         | [] -> failwith "if: stack underflow"
         | Int 0 :: rest ->
             (* Condition is false, skip to else or then *)
             state.stack <- rest;
             state.control_flow <- Types.SkippingToElse 0;
             return ()
         | Int _ :: rest ->
             (* Condition is true, continue normally *)
             state.stack <- rest;
             state.control_flow <- Types.Normal;
             return ()
         | _ -> failwith "if: requires integer on stack")
       else if String.equal token "else" then (
         (* else: we executed the if branch, now skip to then *)
         state.control_flow <- Types.SkippingToThen 0;
         return ())
       else if String.equal token "then" then (
         (* then: end of if/then (no else branch) *)
         state.control_flow <- Types.Normal;
         return ())
       else if String.equal token "begin" then (
         (* Start begin...until or begin...while...repeat loop *)
         (* We don't know which type yet, collect tokens *)
         state.collecting_loop <- Some (Types.BeginUntil, [], 0);
         return ())
       else if String.equal token "until" then
         (* until without begin *)
         failwith "until: no matching begin"
       else if String.equal token "repeat" then
         (* repeat without begin *)
         failwith "repeat: no matching begin"
       else if String.equal token "do" then (
         (* Start do...loop or do...+loop *)
         state.collecting_loop <- Some (Types.DoLoop, [], 0);
         return ())
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
             return ()
         | _ -> failwith "each: requires Output on stack")
       else if String.equal token ":" then (
         (* Start word definition - next token is the name *)
         state.defining <- Some "UNNAMED";
         return ())
       else if String.equal token "see" then (
         (* see word - next token is the word to inspect *)
         (* This is a placeholder - see will be handled specially in eval_line *)
         return ())
       else if is_int token then (
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
             Lwt_list.iter_s (fun t -> eval_token state (t, false)) tokens
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
                   (* Push matches in order (they'll be reversed on stack) *)
                   List.iter matches ~f:(fun m -> state.stack <- String m :: state.stack));
                 state.stack <- String token :: state.stack;
                 return ())))
;;

(* Evaluate a line of input *)
let eval_line state line =
  let tokens = tokenize line in
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
