open Core
open Lwt
open Types

(* Execute a list of tokens - forward declaration type *)
type token_executor = state -> string list -> unit Lwt.t

(* Execute begin...until loop *)
let execute_begin_until (execute_tokens : token_executor) state loop_body =
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
    | [] -> Lwt.fail (Failure "until: stack underflow (needs condition)")
    | _ :: _ -> Lwt.fail (Failure "until: requires integer condition")
  in
  loop_until ()
;;

(* Execute begin...while...repeat loop *)
let execute_begin_while (execute_tokens : token_executor) state before_while after_while =
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
    | [] -> Lwt.fail (Failure "while: stack underflow (needs condition)")
    | _ :: _ -> Lwt.fail (Failure "while: requires integer condition")
  in
  loop_while ()
;;

(* Execute do...loop (simple counted loop) *)
let execute_do_loop (execute_tokens : token_executor) state ~start ~limit ~loop_body =
  let rec loop_do idx =
    if idx < limit then (
      (* Push loop info onto loop stack *)
      let loop_info = Types.DoCountedLoop { body = loop_body; start; limit; current = idx } in
      state.loop_stack <- loop_info :: state.loop_stack;
      execute_tokens state loop_body >>= fun () ->
      state.loop_stack <- List.tl_exn state.loop_stack;
      loop_do (idx + 1))
    else
      return ()
  in
  loop_do start
;;

(* Execute do...+loop (counted loop with dynamic step) *)
let execute_do_plus_loop (execute_tokens : token_executor) state ~start ~limit ~loop_body =
  let rec loop_do_step idx =
    (* Check if we should continue (ascending or descending) *)
    let should_continue =
      if start < limit then idx < limit
      else idx > limit
    in
    if should_continue then (
      (* Push loop info onto loop stack *)
      let loop_info = Types.DoPlusCountedLoop { body = loop_body; start; limit; current = idx } in
      state.loop_stack <- loop_info :: state.loop_stack;
      execute_tokens state loop_body >>= fun () ->
      state.loop_stack <- (match state.loop_stack with
        | [] -> Errors.internal_error "loop stack underflow in +loop"
        | _ :: rest -> rest);
      (* Get step from stack *)
      match state.stack with
      | Int step :: rest ->
          state.stack <- rest;
          loop_do_step (idx + step)
      | _ -> Lwt.fail (Failure "+loop: stack underflow (needs step)"))
    else
      return ()
  in
  loop_do_step start
;;

(* Split begin...while...repeat body at 'while' keyword *)
let split_while_body all_tokens =
  let rec find_while acc = function
    | [] -> failwith "repeat: no matching while"
    | "while" :: rest -> List.rev acc, rest
    | token :: rest -> find_while (token :: acc) rest
  in
  find_while [] all_tokens
;;

(* Handle loop body collection and execution
   This function manages the collection of loop tokens and dispatches to
   the appropriate execution function when the loop is complete *)
let handle_loop_collection (execute_tokens : token_executor) state token loop_type body depth =
  match token, loop_type, depth with
  | "until", Types.BeginUntil, 0 ->
      (* End of begin...until loop (not nested) *)
      let loop_body = List.rev body in
      state.collecting_loop <- None;
      execute_begin_until execute_tokens state loop_body

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
      let before_while, after_while = split_while_body all_tokens in
      state.collecting_loop <- None;
      execute_begin_while execute_tokens state before_while after_while

  | "repeat", Types.BeginWhile, _ ->
      (* Nested repeat, add to body and decrement depth *)
      state.collecting_loop <- Some (loop_type, token :: body, depth - 1);
      return ()

  | "loop", (Types.DoLoop | Types.DoPlusLoop), 0 ->
      (* End of do...loop (not nested) *)
      let loop_body = List.rev body in
      state.collecting_loop <- None;
      (match state.stack with
       | Int limit :: Int start :: rest ->
           state.stack <- rest;
           execute_do_loop execute_tokens state ~start ~limit ~loop_body
       | _ -> failwith "do: stack underflow (needs start and limit)")

  | "loop", (Types.DoLoop | Types.DoPlusLoop), _ ->
      (* Nested loop, add to body and decrement depth *)
      state.collecting_loop <- Some (loop_type, token :: body, depth - 1);
      return ()

  | "+loop", Types.DoPlusLoop, 0 ->
      (* End of do...+loop with step (not nested) *)
      let loop_body = List.rev body in
      state.collecting_loop <- None;
      (match state.stack with
       | Int limit :: Int start :: rest ->
           state.stack <- rest;
           execute_do_plus_loop execute_tokens state ~start ~limit ~loop_body
       | _ -> failwith "do: stack underflow (needs start and limit)")

  | "+loop", Types.DoPlusLoop, _ ->
      (* Nested +loop, add to body and decrement depth *)
      state.collecting_loop <- Some (loop_type, token :: body, depth - 1);
      return ()

  | "+loop", Types.DoLoop, 0 ->
      (* This is actually a do...+loop, change type (not nested) *)
      let loop_body = List.rev body in
      state.collecting_loop <- None;
      (match state.stack with
       | Int limit :: Int start :: rest ->
           state.stack <- rest;
           execute_do_plus_loop execute_tokens state ~start ~limit ~loop_body
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
      return ()
;;
