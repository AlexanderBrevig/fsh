open Core
open Fsh

(* Helper to create a fresh state for each test *)
let fresh_state () =
  let state = Types.create_state () in
  Builtins.register_builtins state.dict;
  state
;;

(* ========== Stack Operations Tests ========== *)

let%expect_test "dup duplicates top of stack" =
  let state = fresh_state () in
  state.stack <- [ String "hello" ];
  state.stack <- Lwt_main.run (Builtins.Stack.dup state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <2> "hello" "hello"
    |}]
;;

let%expect_test "dup with multiple items" =
  let state = fresh_state () in
  state.stack <- [ String "b"; String "a" ];
  state.stack <- Lwt_main.run (Builtins.Stack.dup state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {| <3> "b" "b" "a" |}]
;;

let%expect_test "swap exchanges top two items" =
  let state = fresh_state () in
  state.stack <- [ String "second"; String "first" ];
  state.stack <- Lwt_main.run (Builtins.Stack.swap state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <2> "first" "second"
    |}]
;;

let%expect_test "drop removes top item" =
  let state = fresh_state () in
  state.stack <- [ String "top"; String "bottom" ];
  state.stack <- Lwt_main.run (Builtins.Stack.drop state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "bottom"
    |}]
;;

let%expect_test "over copies second item to top" =
  let state = fresh_state () in
  state.stack <- [ String "b"; String "a" ];
  state.stack <- Lwt_main.run (Builtins.Stack.over state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <3> "a" "b" "a"
    |}]
;;

let%expect_test "rot rotates top three items" =
  let state = fresh_state () in
  state.stack <- [ String "c"; String "b"; String "a" ];
  state.stack <- Lwt_main.run (Builtins.Stack.rot state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <3> "a" "c" "b"
    |}]
;;

let%expect_test "clear empties the stack" =
  let state = fresh_state () in
  state.stack <- [ String "c"; String "b"; String "a" ];
  state.stack <- Lwt_main.run (Builtins.Stack.clear state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <0>
    |}]
;;

(* ========== Error Handling Tests ========== *)

let%expect_test "dup on empty stack fails" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    let state = fresh_state () in
    state.stack <- [];
    Lwt_main.run (Builtins.Stack.dup state.stack));
  [%expect {| (Failure "Stack underflow: dup") |}]
;;

let%expect_test "swap on single item fails" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    let state = fresh_state () in
    state.stack <- [ String "only one" ];
    Lwt_main.run (Builtins.Stack.swap state.stack));
  [%expect {| (Failure "Stack underflow: swap") |}]
;;

let%expect_test "drop on empty stack fails" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    let state = fresh_state () in
    state.stack <- [];
    Lwt_main.run (Builtins.Stack.drop state.stack));
  [%expect {| (Failure "Stack underflow: drop") |}]
;;

let%expect_test "over with insufficient items fails" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    let state = fresh_state () in
    state.stack <- [ String "only one" ];
    Lwt_main.run (Builtins.Stack.over state.stack));
  [%expect {| (Failure "Stack underflow: over") |}]
;;

let%expect_test "rot with insufficient items fails" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    let state = fresh_state () in
    state.stack <- [ String "b"; String "a" ];
    Lwt_main.run (Builtins.Stack.rot state.stack));
  [%expect {| (Failure "Stack underflow: rot") |}]
;;
