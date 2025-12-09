open Core
open Fsh

(* Helper to create a fresh state for each test *)
let fresh_state () =
  let state = Types.create_state () in
  Builtins.register_builtins state.dict;
  state
;;

(* ========== Integration Tests ========== *)

let%expect_test "stack manipulation combo" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "a b c");
  Lwt_main.run (Eval.eval_line state "rot");
  Lwt_main.run (Eval.eval_line state "swap");
  Lwt_main.run (Eval.eval_line state "dup");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {| <4> "c" "c" "a" "b" |}]
;;

let%expect_test "eval line with builtin operations" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "hello dup swap drop");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "hello"
    |}]
;;

let%expect_test "integers with stack operations" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "10 20 30 swap drop");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {| <2> 30 10 |}]
;;

let%expect_test "type conversion chain" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "data >output >string");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "data"
    |}]
;;
