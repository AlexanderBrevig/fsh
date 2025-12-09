open Core
open Fsh

(* Helper to create a fresh state for each test *)
let fresh_state () =
  let state = Types.create_state () in
  Builtins.register_builtins state.dict;
  state
;;

(* ========== begin...until Loop Tests ========== *)

let%expect_test "simple begin...until counts to 5" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "0 begin 1 + dup 5 = until");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 5
    |}]
;;

let%expect_test "begin...until with printing" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "0 begin 1 + dup . dup 3 = until");
  state.stack <- Lwt_main.run (Builtins.Stack.drop state.stack);
  [%expect {|
    1
    2
    3
    |}]
;;

let%expect_test "begin...until executes at least once" =
  let state = fresh_state () in
  (* Even though condition starts true, loop should execute once *)
  Lwt_main.run (Eval.eval_line state "10 begin dup . 1 + dup 5 > until");
  state.stack <- Lwt_main.run (Builtins.Stack.drop state.stack);
  [%expect {|
    10
    |}]
;;

let%expect_test "begin...until with stack operations" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "1 begin 2 * dup 16 > until");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 32
    |}]
;;

(* ========== begin...while...repeat Loop Tests ========== *)

let%expect_test "simple begin...while...repeat counts down" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "5 begin dup 0 > while dup . 1 - repeat drop");
  [%expect {|
    5
    4
    3
    2
    1
    |}]
;;

let%expect_test "begin...while...repeat with false condition never enters" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "0 begin dup 0 > while dup . 1 - repeat");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "begin...while...repeat accumulates sum" =
  let state = fresh_state () in
  (* Sum from 1 to 5: 0 sum, 5 counter *)
  Lwt_main.run (Eval.eval_line state "0 5 begin dup 0 > while swap over + swap 1 - repeat drop");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 15
    |}]
;;

let%expect_test "begin...while...repeat with complex condition" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "1 begin dup 100 < while 2 * repeat");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 128
    |}]
;;

(* ========== do...loop Counted Loop Tests ========== *)

let%expect_test "simple do...loop prints 0 to 4" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "0 5 do i . loop");
  [%expect {|
    0
    1
    2
    3
    4
    |}]
;;

let%expect_test "do...loop with start and end" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "3 7 do i . loop");
  [%expect {|
    3
    4
    5
    6
    |}]
;;

let%expect_test "do...loop accumulates values" =
  let state = fresh_state () in
  (* Sum 1+2+3+4+5 *)
  Lwt_main.run (Eval.eval_line state "0 1 6 do i + loop");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 15
    |}]
;;

let%expect_test "do...loop with empty range does nothing" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "5 5 do i . loop");
  [%expect {|
    |}]
;;

let%expect_test "nested do...loop with i and j" =
  let state = fresh_state () in
  (* Print i*10 + j for 2x2 grid *)
  Lwt_main.run (Eval.eval_line state "0 2 do 0 2 do j 10 * i + . loop loop");
  [%expect {|
    0
    1
    10
    11
    |}]
;;

let%expect_test "do...loop with step using +loop" =
  let state = fresh_state () in
  (* Count by 2s from 0 to 10 *)
  Lwt_main.run (Eval.eval_line state "0 10 do i . 2 +loop");
  [%expect {|
    0
    2
    4
    6
    8
    |}]
;;

let%expect_test "do...loop counting down with negative step" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "10 0 do i . -2 +loop");
  [%expect {|
    10
    8
    6
    4
    2
    |}]
;;

(* ========== Loop Error Handling Tests ========== *)

open Lwt.Infix

let%expect_test "until without begin fails" =
  let state = fresh_state () in
  (try Lwt_main.run (Eval.eval_line state "until") with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    until: no matching begin
    |}]
;;

let%expect_test "repeat without begin fails" =
  let state = fresh_state () in
  (try Lwt_main.run (Eval.eval_line state "repeat") with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    repeat: no matching begin
    |}]
;;

let%expect_test "loop without do fails" =
  let state = fresh_state () in
  (try Lwt_main.run (Eval.eval_line state "loop") with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    loop: no matching do
    |}]
;;

let%expect_test "until without condition on stack fails" =
  let state = fresh_state () in
  (try Lwt_main.run (Eval.eval_line state "begin until") with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    until: stack underflow (needs condition)
    |}]
;;

let%expect_test "while without condition on stack fails" =
  let state = fresh_state () in
  (try Lwt_main.run (Eval.eval_line state "begin while repeat") with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    while: stack underflow (needs condition)
    |}]
;;

let%expect_test "do without two integers fails" =
  let state = fresh_state () in
  (try Lwt_main.run (Eval.eval_line state "5 do loop") with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    do: stack underflow (needs start and limit)
    |}]
;;

let%expect_test "i outside loop fails" =
  let state = fresh_state () in
  (try Lwt_main.run (Eval.eval_line state "i") with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    i: not inside a loop
    |}]
;;

let%expect_test "j outside nested loop fails" =
  let state = fresh_state () in
  (try Lwt_main.run (Eval.eval_line state "0 2 do j loop") with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    j: not inside a nested loop
    |}]
;;
