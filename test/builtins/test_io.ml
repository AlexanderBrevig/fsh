open Core
open Fsh

(* Helper to create a fresh state for each test *)
let fresh_state () =
  let state = Types.create_state () in
  Builtins.register_builtins state.dict;
  state
;;

(* ========== Printing Operations Tests ========== *)

let%expect_test "dot prints string with newline" =
  let state = fresh_state () in
  state.stack <- [ String "hello world" ];
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    hello world
    |}]
;;

let%expect_test "dot prints integer with newline" =
  let state = fresh_state () in
  state.stack <- [ Int 42 ];
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    42
    |}]
;;

let%expect_test "dot prints output with newline" =
  let state = fresh_state () in
  state.stack <- [ Output "command output" ];
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    command output
    |}]
;;

let%expect_test "type prints string without newline" =
  let state = fresh_state () in
  state.stack <- [ String "no newline" ];
  state.stack <- Lwt_main.run (Builtins.Io.type_word state.stack);
  [%expect {| no newline |}]
;;

let%expect_test "type prints integer without newline" =
  let state = fresh_state () in
  state.stack <- [ Int 99 ];
  state.stack <- Lwt_main.run (Builtins.Io.type_word state.stack);
  [%expect {| 99 |}]
;;

let%expect_test "dot_s displays empty stack" =
  let state = fresh_state () in
  state.stack <- [];
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <0>
    |}]
;;

let%expect_test "dot_s displays stack with mixed types" =
  let state = fresh_state () in
  state.stack <- [ Int 5; String "text"; Output "result" ];
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <3> 5 "text" «result»
    |}]
;;

(* ========== Type Conversion Tests ========== *)

let%expect_test "to_output converts string to output" =
  let state = fresh_state () in
  state.stack <- [ String "test data" ];
  state.stack <- Lwt_main.run (Builtins.Io.to_output state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> «test data»
    |}]
;;

let%expect_test "to_output with output is noop" =
  let state = fresh_state () in
  state.stack <- [ Output "already output" ];
  state.stack <- Lwt_main.run (Builtins.Io.to_output state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> «already output»
    |}]
;;

let%expect_test "to_string converts output to string" =
  let state = fresh_state () in
  state.stack <- [ Output "output data" ];
  state.stack <- Lwt_main.run (Builtins.Io.to_string state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "output data"
    |}]
;;

let%expect_test "to_string converts int to string" =
  let state = fresh_state () in
  state.stack <- [ Int 42 ];
  state.stack <- Lwt_main.run (Builtins.Io.to_string state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "42"
    |}]
;;

let%expect_test "to_arg converts int to string" =
  let state = fresh_state () in
  state.stack <- [ Int 123 ];
  state.stack <- Lwt_main.run (Builtins.Io.to_arg state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "123"
    |}]
;;

let%expect_test "to_arg converts output to string" =
  let state = fresh_state () in
  state.stack <- [ Output "some output" ];
  state.stack <- Lwt_main.run (Builtins.Io.to_arg state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "some output"
    |}]
;;

(* ========== Error Handling Tests ========== *)

let%expect_test "dot on empty stack fails" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    let state = fresh_state () in
    state.stack <- [];
    Lwt_main.run (Builtins.Io.dot state.stack));
  [%expect {| (Failure "Stack underflow: .") |}]
;;

let%expect_test "to_output with int fails" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    let state = fresh_state () in
    state.stack <- [ Int 42 ];
    Lwt_main.run (Builtins.Io.to_output state.stack));
  [%expect {| (Failure ">output: requires string") |}]
;;
