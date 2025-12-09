(* Re-export submodules for direct access *)
module Stack = Builtins_stack
module Io = Builtins_io
module System = Builtins_system
module Computation = Builtins_computation
module Introspection = Builtins_introspection

(* Re-export functions used by eval.ml *)
let exec_word = System.exec_word
let see_word = Introspection.see_word

(* Register all builtins in the dictionary *)
let register_builtins dict =
  Stack.register dict;
  Io.register dict;
  System.register dict;
  Computation.register dict;
  Introspection.register dict
;;
