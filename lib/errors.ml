open Core

(* ========== Error Module ==========
   Centralized error message formatting for consistent error reporting
   across the codebase.
*)

(* ========== Stack Errors ========== *)

let stack_underflow op =
  failwith (sprintf "Stack underflow: %s" op)

let stack_underflow_msg op msg =
  failwith (sprintf "%s: stack underflow (%s)" op msg)

(* ========== Type Errors ========== *)

let requires_type ~op ~typ =
  failwith (sprintf "%s: requires %s" op typ)

let requires_int op =
  requires_type ~op ~typ:"integer"

let requires_string op =
  requires_type ~op ~typ:"string"

let requires_output op =
  requires_type ~op ~typ:"Output"

(* ========== Arithmetic Errors ========== *)

let div_by_zero op =
  failwith (sprintf "%s: division by zero" op)

(* ========== Control Flow Errors ========== *)

let unmatched_control_flow ~construct ~missing =
  failwith (sprintf "%s: no matching %s" construct missing)

let unmatched_begin op =
  unmatched_control_flow ~construct:op ~missing:"begin"

let unmatched_do op =
  unmatched_control_flow ~construct:op ~missing:"do"

let unmatched_while op =
  unmatched_control_flow ~construct:op ~missing:"while"

(* ========== System Errors ========== *)

let system_error ~op ~msg =
  failwith (sprintf "%s: %s" op msg)

let file_error ~op ~filename ~msg =
  failwith (sprintf "%s: %s: %s" op filename msg)

(* Wrap a Unix operation and convert Unix_error to a readable error message *)
let with_unix_error ~op f =
  try f () with
  | Core_unix.Unix_error (err, _, _) ->
      failwith (sprintf "%s: %s" op (Core_unix.Error.message err))

(* ========== General Errors ========== *)

let invalid_argument ~op ~msg =
  failwith (sprintf "%s: %s" op msg)

let internal_error msg =
  failwith (sprintf "Internal error: %s" msg)
