open Lwt

(* ========== Stack Manipulation ========== *)

let dup = function
  | [] -> Errors.stack_underflow "dup"
  | x :: rest -> return (x :: x :: rest)
;;

let swap = function
  | [] | [ _ ] -> Errors.stack_underflow "swap"
  | x :: y :: rest -> return (y :: x :: rest)
;;

let drop = function
  | [] -> Errors.stack_underflow "drop"
  | _ :: rest -> return rest
;;

let clear _ = return []
;;

let over = function
  | [] | [ _ ] -> Errors.stack_underflow "over"
  | x :: y :: rest -> return (y :: x :: y :: rest)
;;

let rot = function
  | [] | [ _ ] | [ _; _ ] -> Errors.stack_underflow "rot"
  | x :: y :: z :: rest -> return (z :: x :: y :: rest)
;;

(* ========== Registration ========== *)

let register dict =
  let reg = Builtins_base.register_builtin dict in
  reg ~name:"dup" ~fn:dup ~doc:"( a -- a a ) Duplicate top item";
  reg ~name:"swap" ~fn:swap ~doc:"( a b -- b a ) Swap top two items";
  reg ~name:"drop" ~fn:drop ~doc:"( a -- ) Remove top item";
  reg ~name:"clear" ~fn:clear ~doc:"( ... -- ) Clear entire stack";
  reg ~name:"over" ~fn:over ~doc:"( a b -- a b a ) Copy second item to top";
  reg ~name:"rot" ~fn:rot ~doc:"( a b c -- b c a ) Rotate top three items"
;;
