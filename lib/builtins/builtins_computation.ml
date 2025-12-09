open Core
open Lwt
open Types

(* ========== Arithmetic Operators ========== *)

(* Addition: a b -> a+b *)
let add = function
  | [] | [_] -> Errors.stack_underflow "+"
  | Int b :: Int a :: rest -> return (Int (a + b) :: rest)
  | _ -> Errors.requires_type ~op:"+" ~typ:"two integers"
;;

(* Subtraction: a b -> a-b *)
let sub = function
  | [] | [_] -> Errors.stack_underflow "-"
  | Int b :: Int a :: rest -> return (Int (a - b) :: rest)
  | _ -> Errors.requires_type ~op:"-" ~typ:"two integers"
;;

(* Multiplication: a b -> a*b *)
let mul = function
  | [] | [_] -> Errors.stack_underflow "*"
  | Int b :: Int a :: rest -> return (Int (a * b) :: rest)
  | _ -> Errors.requires_type ~op:"*" ~typ:"two integers"
;;

(* Division: a b -> a/b *)
let div = function
  | [] | [_] -> Errors.stack_underflow "/"
  | Int 0 :: _ :: _ -> Errors.div_by_zero "/"
  | Int b :: Int a :: rest -> return (Int (a / b) :: rest)
  | _ -> Errors.requires_type ~op:"/" ~typ:"two integers"
;;

(* Modulo: a b -> a mod b *)
let mod_op = function
  | [] | [_] -> Errors.stack_underflow "mod"
  | Int 0 :: _ :: _ -> Errors.div_by_zero "mod"
  | Int b :: Int a :: rest -> return (Int (a mod b) :: rest)
  | _ -> Errors.requires_type ~op:"mod" ~typ:"two integers"
;;

(* Division and modulo: a b -> quotient remainder *)
let divmod = function
  | [] | [_] -> Errors.stack_underflow "/mod"
  | Int 0 :: _ :: _ -> Errors.div_by_zero "/mod"
  | Int b :: Int a :: rest ->
      let quot = a / b in
      let rem = a mod b in
      return (Int rem :: Int quot :: rest)
  | _ -> Errors.requires_type ~op:"/mod" ~typ:"two integers"
;;

(* Multiply then divide: a b c -> (a*b)/c *)
let muldiv = function
  | [] | [_] | [_; _] -> Errors.stack_underflow "*/"
  | Int 0 :: _ :: _ :: _ -> Errors.div_by_zero "*/"
  | Int c :: Int b :: Int a :: rest ->
      return (Int ((a * b) / c) :: rest)
  | _ -> Errors.requires_type ~op:"*/" ~typ:"three integers"
;;

(* ========== Comparison Operators ========== *)

(* Equality: a b -> 0 or 1 *)
let eq = function
  | [] | [_] -> Errors.stack_underflow "="
  | Int b :: Int a :: rest -> return (Int (if a = b then 1 else 0) :: rest)
  | String b :: String a :: rest -> return (Int (if String.equal a b then 1 else 0) :: rest)
  | _ -> Errors.requires_type ~op:"=" ~typ:"two values of the same type"
;;

(* Greater than: a b -> 0 or 1 *)
let gt = function
  | [] | [_] -> Errors.stack_underflow ">"
  | Int b :: Int a :: rest -> return (Int (if a > b then 1 else 0) :: rest)
  | _ -> Errors.requires_type ~op:">" ~typ:"two integers"
;;

(* Less than: a b -> 0 or 1 *)
let lt = function
  | [] | [_] -> Errors.stack_underflow "<"
  | Int b :: Int a :: rest -> return (Int (if a < b then 1 else 0) :: rest)
  | _ -> Errors.requires_type ~op:"<" ~typ:"two integers"
;;

(* Greater than or equal: a b -> 0 or 1 *)
let gte = function
  | [] | [_] -> Errors.stack_underflow ">="
  | Int b :: Int a :: rest -> return (Int (if a >= b then 1 else 0) :: rest)
  | _ -> Errors.requires_type ~op:">=" ~typ:"two integers"
;;

(* Less than or equal: a b -> 0 or 1 *)
let lte = function
  | [] | [_] -> Errors.stack_underflow "<="
  | Int b :: Int a :: rest -> return (Int (if a <= b then 1 else 0) :: rest)
  | _ -> Errors.requires_type ~op:"<=" ~typ:"two integers"
;;

(* Not equal: a b -> 0 or 1 *)
let neq = function
  | [] | [_] -> Errors.stack_underflow "<>"
  | Int b :: Int a :: rest -> return (Int (if a <> b then 1 else 0) :: rest)
  | String b :: String a :: rest -> return (Int (if not (String.equal a b) then 1 else 0) :: rest)
  | _ -> Errors.requires_type ~op:"<>" ~typ:"two values of the same type"
;;

(* ========== Boolean Logic Operators ========== *)

(* Boolean AND: a b -> (a and b) *)
(* 0 is false, non-zero is true; returns 1 for true, 0 for false *)
let bool_and = function
  | [] | [_] -> Errors.stack_underflow "and"
  | Int b :: Int a :: rest ->
      let result = if a <> 0 && b <> 0 then 1 else 0 in
      return (Int result :: rest)
  | _ -> Errors.requires_type ~op:"and" ~typ:"two integers"
;;

(* Boolean OR: a b -> (a or b) *)
let bool_or = function
  | [] | [_] -> Errors.stack_underflow "or"
  | Int b :: Int a :: rest ->
      let result = if a <> 0 || b <> 0 then 1 else 0 in
      return (Int result :: rest)
  | _ -> Errors.requires_type ~op:"or" ~typ:"two integers"
;;

(* Boolean NOT: a -> (not a) *)
let bool_not = function
  | [] -> Errors.stack_underflow "not"
  | Int a :: rest ->
      let result = if a = 0 then 1 else 0 in
      return (Int result :: rest)
  | _ -> Errors.requires_type ~op:"not" ~typ:"integer"
;;

(* Boolean XOR: a b -> (a xor b) *)
let bool_xor = function
  | [] | [_] -> Errors.stack_underflow "xor"
  | Int b :: Int a :: rest ->
      (* XOR: true when exactly one operand is true *)
      let result = match (a <> 0, b <> 0) with
        | (true, false) | (false, true) -> 1
        | _ -> 0
      in
      return (Int result :: rest)
  | _ -> Errors.requires_type ~op:"xor" ~typ:"two integers"
;;

(* ========== String Operations ========== *)

(* Concatenate two strings: a b -> a+b *)
let concat = function
  | [] | [_] -> Errors.stack_underflow "concat"
  | String b :: String a :: rest -> return (String (a ^ b) :: rest)
  | _ -> Errors.requires_type ~op:"concat" ~typ:"two strings"
;;

(* Conditional string helpers for prompts *)

(* ?prefix: prepend separator only if string is non-empty *)
(* ( str sep -- prefixed-str ) *)
let prefix_if_nonempty = function
  | [] | [_] -> Errors.stack_underflow "?prefix"
  | String sep :: String str :: rest ->
      if String.is_empty str then
        return (String str :: rest)
      else
        return (String (sep ^ str) :: rest)
  | _ -> Errors.requires_type ~op:"?prefix" ~typ:"two strings"
;;

(* ?suffix: append separator only if string is non-empty *)
(* ( str sep -- suffixed-str ) *)
let suffix_if_nonempty = function
  | [] | [_] -> Errors.stack_underflow "?suffix"
  | String sep :: String str :: rest ->
      if String.is_empty str then
        return (String str :: rest)
      else
        return (String (str ^ sep) :: rest)
  | _ -> Errors.requires_type ~op:"?suffix" ~typ:"two strings"
;;

(* ?wrap: wrap with prefix and suffix only if string is non-empty *)
(* ( str prefix suffix -- wrapped-str ) *)
let wrap_if_nonempty = function
  | [] | [_] | [_; _] -> Errors.stack_underflow "?wrap"
  | String suffix :: String prefix :: String str :: rest ->
      if String.is_empty str then
        return (String str :: rest)
      else
        return (String (prefix ^ str ^ suffix) :: rest)
  | _ -> Errors.requires_type ~op:"?wrap" ~typ:"three strings"
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

(* ========== Registration ========== *)

let register dict =
  let reg = Builtins_base.register_builtin dict in
  let reg_state = Builtins_base.register_state_builtin dict in
  (* Arithmetic *)
  reg ~name:"+" ~fn:add ~doc:"( a b -- a+b ) Add two numbers";
  reg ~name:"-" ~fn:sub ~doc:"( a b -- a-b ) Subtract b from a";
  reg ~name:"*" ~fn:mul ~doc:"( a b -- a*b ) Multiply two numbers";
  reg ~name:"/" ~fn:div ~doc:"( a b -- a/b ) Divide a by b";
  reg ~name:"mod" ~fn:mod_op ~doc:"( a b -- a%b ) Modulo (remainder of a/b)";
  reg ~name:"/mod" ~fn:divmod ~doc:"( a b -- quot rem ) Quotient and remainder";
  reg ~name:"*/" ~fn:muldiv ~doc:"( a b c -- (a*b)/c ) Multiply then divide";
  (* Comparison *)
  reg ~name:"=" ~fn:eq ~doc:"( a b -- flag ) Test equality (1 if equal, 0 if not)";
  reg ~name:">" ~fn:gt ~doc:"( a b -- flag ) Test greater than (1 if a>b, 0 if not)";
  reg ~name:"<" ~fn:lt ~doc:"( a b -- flag ) Test less than (1 if a<b, 0 if not)";
  reg ~name:">=" ~fn:gte ~doc:"( a b -- flag ) Test greater or equal (1 if a>=b, 0 if not)";
  reg ~name:"<=" ~fn:lte ~doc:"( a b -- flag ) Test less or equal (1 if a<=b, 0 if not)";
  reg ~name:"<>" ~fn:neq ~doc:"( a b -- flag ) Test not equal (1 if not equal, 0 if equal)";
  (* Boolean logic *)
  reg ~name:"and" ~fn:bool_and ~doc:"( a b -- flag ) Boolean AND (0=false, non-zero=true)";
  reg ~name:"or" ~fn:bool_or ~doc:"( a b -- flag ) Boolean OR (0=false, non-zero=true)";
  reg ~name:"not" ~fn:bool_not ~doc:"( a -- flag ) Boolean NOT (0=false, non-zero=true)";
  reg ~name:"xor" ~fn:bool_xor ~doc:"( a b -- flag ) Boolean XOR (0=false, non-zero=true)";
  (* String operations *)
  reg ~name:"concat" ~fn:concat ~doc:"( a b -- a+b ) Concatenate two strings";
  reg ~name:"?prefix" ~fn:prefix_if_nonempty ~doc:"( str sep -- result ) Prepend separator if string non-empty";
  reg ~name:"?suffix" ~fn:suffix_if_nonempty ~doc:"( str sep -- result ) Append separator if string non-empty";
  reg ~name:"?wrap" ~fn:wrap_if_nonempty ~doc:"( str pre suf -- result ) Wrap with prefix/suffix if non-empty";
  (* Loop indices *)
  reg_state ~name:"i" ~fn:loop_i ~doc:"( -- index ) Push current loop index";
  reg_state ~name:"j" ~fn:loop_j ~doc:"( -- index ) Push outer loop index (nested loops)"
;;
