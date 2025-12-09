open Core

(* ========== Builtins Base Module ==========
   Common utilities for builtin word registration.
   Provides centralized registration helpers to reduce
   boilerplate across all builtin modules.
*)

(* Type alias for word dictionary *)
type dict = (string, Types.word) Hashtbl.t

(* ========== Registration Functions ========== *)

(* Register a regular builtin that operates on stack values *)
let register_builtin (dict : dict) ~name ~fn ~doc =
  Hashtbl.set dict ~key:name ~data:(Types.Builtin (fn, Some doc))

(* Register a state builtin that needs access to mutable state *)
let register_state_builtin (dict : dict) ~name ~fn ~doc =
  Hashtbl.set dict ~key:name ~data:(Types.StateBuiltin (fn, Some doc))

(* ========== Convenience Wrappers ========== *)

(* Create a registration context for a dictionary *)
module Register = struct
  type t = {
    dict : dict;
  }

  let create dict = { dict }

  let builtin t ~name ~fn ~doc =
    register_builtin t.dict ~name ~fn ~doc

  let state_builtin t ~name ~fn ~doc =
    register_state_builtin t.dict ~name ~fn ~doc
end
