open Core

(* ========== Keywords Module ==========
   Centralized keyword definitions for the Forth shell.
   All language keywords are defined here to ensure consistency
   across parsing, evaluation, and syntax highlighting.
*)

(* ========== Conditional Keywords ========== *)

let conditional = ["if"; "then"; "else"]

(* ========== Loop Keywords ========== *)

let loop_keywords = ["begin"; "until"; "while"; "repeat"; "do"; "loop"; "+loop"; "each"]

(* ========== Word Definition Keywords ========== *)

let definition = [":"; ";"]

(* ========== Introspection Keywords ========== *)

let introspection = ["see"]

(* ========== All Control Flow Keywords ========== *)

(* Keywords that affect control flow during evaluation *)
let control_flow = conditional @ loop_keywords @ definition

(* All language keywords (including non-control-flow ones) *)
let all = control_flow @ introspection

(* ========== Keyword Testing Functions ========== *)

let is_conditional token =
  List.mem conditional token ~equal:String.equal

let is_loop token =
  List.mem loop_keywords token ~equal:String.equal

let is_definition token =
  List.mem definition token ~equal:String.equal

let is_introspection token =
  List.mem introspection token ~equal:String.equal

let is_control_flow token =
  List.mem control_flow token ~equal:String.equal

let is_keyword token =
  List.mem all token ~equal:String.equal
