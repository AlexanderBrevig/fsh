open Core
open Fsh

(* Helper to create a fresh state for each test *)
let fresh_state () =
  let state = Types.create_state () in
  Builtins.register_builtins state.dict;
  state
;;

(* ========== Arithmetic Operator Tests ========== *)

let%expect_test "add two integers" =
  let state = fresh_state () in
  state.stack <- [ Int 7; Int 3 ];
  state.stack <- Lwt_main.run (Builtins.Computation.add state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 10
    |}]
;;

let%expect_test "subtract integers" =
  let state = fresh_state () in
  state.stack <- [ Int 5; Int 10 ];
  state.stack <- Lwt_main.run (Builtins.Computation.sub state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 5
    |}]
;;

let%expect_test "multiply integers" =
  let state = fresh_state () in
  state.stack <- [ Int 6; Int 7 ];
  state.stack <- Lwt_main.run (Builtins.Computation.mul state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 42
    |}]
;;

let%expect_test "divide integers" =
  let state = fresh_state () in
  state.stack <- [ Int 3; Int 15 ];
  state.stack <- Lwt_main.run (Builtins.Computation.div state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 5
    |}]
;;

let%expect_test "modulo operation" =
  let state = fresh_state () in
  state.stack <- [ Int 3; Int 10 ];
  state.stack <- Lwt_main.run (Builtins.Computation.mod_op state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "divmod returns quotient and remainder" =
  let state = fresh_state () in
  state.stack <- [ Int 3; Int 10 ];
  state.stack <- Lwt_main.run (Builtins.Computation.divmod state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <2> 1 3
    |}]
;;

let%expect_test "muldiv combines multiply and divide" =
  let state = fresh_state () in
  state.stack <- [ Int 4; Int 6; Int 2 ];
  state.stack <- Lwt_main.run (Builtins.Computation.muldiv state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 3
    |}]
;;

(* ========== Comparison Operator Tests ========== *)

let%expect_test "equality comparison - equal integers" =
  let state = fresh_state () in
  state.stack <- [ Int 5; Int 5 ];
  state.stack <- Lwt_main.run (Builtins.Computation.eq state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "equality comparison - unequal integers" =
  let state = fresh_state () in
  state.stack <- [ Int 7; Int 5 ];
  state.stack <- Lwt_main.run (Builtins.Computation.eq state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "greater than - true" =
  let state = fresh_state () in
  state.stack <- [ Int 3; Int 5 ];
  state.stack <- Lwt_main.run (Builtins.Computation.gt state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "greater than - false" =
  let state = fresh_state () in
  state.stack <- [ Int 5; Int 3 ];
  state.stack <- Lwt_main.run (Builtins.Computation.gt state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "less than - true" =
  let state = fresh_state () in
  state.stack <- [ Int 5; Int 3 ];
  state.stack <- Lwt_main.run (Builtins.Computation.lt state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "greater than or equal - equal" =
  let state = fresh_state () in
  state.stack <- [ Int 5; Int 5 ];
  state.stack <- Lwt_main.run (Builtins.Computation.gte state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "less than or equal - less" =
  let state = fresh_state () in
  state.stack <- [ Int 7; Int 3 ];
  state.stack <- Lwt_main.run (Builtins.Computation.lte state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "not equal - different integers" =
  let state = fresh_state () in
  state.stack <- [ Int 7; Int 5 ];
  state.stack <- Lwt_main.run (Builtins.Computation.neq state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "string equality - equal" =
  let state = fresh_state () in
  state.stack <- [ String "hello"; String "hello" ];
  state.stack <- Lwt_main.run (Builtins.Computation.eq state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "string inequality" =
  let state = fresh_state () in
  state.stack <- [ String "world"; String "hello" ];
  state.stack <- Lwt_main.run (Builtins.Computation.neq state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

(* ========== Boolean Logic Operator Tests ========== *)

let%expect_test "boolean and - both true" =
  let state = fresh_state () in
  state.stack <- [ Int 1; Int 1 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_and state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "boolean and - one false" =
  let state = fresh_state () in
  state.stack <- [ Int 0; Int 1 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_and state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "boolean or - one true" =
  let state = fresh_state () in
  state.stack <- [ Int 0; Int 1 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_or state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "boolean or - both false" =
  let state = fresh_state () in
  state.stack <- [ Int 0; Int 0 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_or state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "boolean not - false becomes true" =
  let state = fresh_state () in
  state.stack <- [ Int 0 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_not state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "boolean not - true becomes false" =
  let state = fresh_state () in
  state.stack <- [ Int 1 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_not state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "boolean xor - different values" =
  let state = fresh_state () in
  state.stack <- [ Int 0; Int 1 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_xor state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "boolean xor - same values" =
  let state = fresh_state () in
  state.stack <- [ Int 1; Int 1 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_xor state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

(* ========== String Operations Tests ========== *)

let%expect_test "concat two strings" =
  let state = fresh_state () in
  state.stack <- [ String "world"; String "hello " ];
  state.stack <- Lwt_main.run (Builtins.Computation.concat state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "hello world"
    |}]
;;

let%expect_test "prefix non-empty string" =
  let state = fresh_state () in
  state.stack <- [ String " "; String "test" ];
  state.stack <- Lwt_main.run (Builtins.Computation.prefix_if_nonempty state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> " test"
    |}]
;;

let%expect_test "prefix empty string does nothing" =
  let state = fresh_state () in
  state.stack <- [ String " "; String "" ];
  state.stack <- Lwt_main.run (Builtins.Computation.prefix_if_nonempty state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> ""
    |}]
;;

let%expect_test "suffix non-empty string" =
  let state = fresh_state () in
  state.stack <- [ String "!"; String "test" ];
  state.stack <- Lwt_main.run (Builtins.Computation.suffix_if_nonempty state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "test!"
    |}]
;;

let%expect_test "wrap non-empty string" =
  let state = fresh_state () in
  state.stack <- [ String "]"; String "["; String "content" ];
  state.stack <- Lwt_main.run (Builtins.Computation.wrap_if_nonempty state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "[content]"
    |}]
;;

(* ========== Error Handling Tests ========== *)

let%expect_test "division by zero fails" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    let state = fresh_state () in
    state.stack <- [ Int 0; Int 10 ];
    Lwt_main.run (Builtins.Computation.div state.stack));
  [%expect {| (Failure "/: division by zero") |}]
;;

let%expect_test "modulo by zero fails" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    let state = fresh_state () in
    state.stack <- [ Int 0; Int 10 ];
    Lwt_main.run (Builtins.Computation.mod_op state.stack));
  [%expect {| (Failure "mod: division by zero") |}]
;;
