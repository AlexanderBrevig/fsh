open Core
open Fsh
open Lwt.Infix

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
  Out_channel.newline stdout;
  [%expect {|
    no newline
    |}]
;;

let%expect_test "type prints integer without newline" =
  let state = fresh_state () in
  state.stack <- [ Int 99 ];
  state.stack <- Lwt_main.run (Builtins.Io.type_word state.stack);
  Out_channel.newline stdout;
  [%expect {|
    99
    |}]
;;

let%expect_test ".s displays empty stack" =
  let state = fresh_state () in
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <0>
    |}]
;;

let%expect_test ".s displays mixed types" =
  let state = fresh_state () in
  state.stack <- [ Output "output"; Int 42; String "hello" ];
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {| <3> «output» 42 "hello" |}]
;;

(* ========== Type Conversion Tests ========== *)

let%expect_test ">output converts string to output" =
  let state = fresh_state () in
  state.stack <- [ String "test" ];
  state.stack <- Lwt_main.run (Builtins.Io.to_output state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {| <1> «test» |}]
;;

let%expect_test ">output with existing output is noop" =
  let state = fresh_state () in
  state.stack <- [ Output "already" ];
  state.stack <- Lwt_main.run (Builtins.Io.to_output state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {| <1> «already» |}]
;;

let%expect_test ">string converts output to string" =
  let state = fresh_state () in
  state.stack <- [ Output "test" ];
  state.stack <- Lwt_main.run (Builtins.Io.to_string state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "test"
    |}]
;;

let%expect_test ">string with existing string is noop" =
  let state = fresh_state () in
  state.stack <- [ String "already" ];
  state.stack <- Lwt_main.run (Builtins.Io.to_string state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "already"
    |}]
;;

let%expect_test ">string converts integer to string" =
  let state = fresh_state () in
  state.stack <- [ Int 42 ];
  state.stack <- Lwt_main.run (Builtins.Io.to_string state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "42"
    |}]
;;

let%expect_test ">arg converts integer to string" =
  let state = fresh_state () in
  state.stack <- [ Int 123 ];
  state.stack <- Lwt_main.run (Builtins.Io.to_arg state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "123"
    |}]
;;

let%expect_test ">arg converts output to string" =
  let state = fresh_state () in
  state.stack <- [ Output "output_data" ];
  state.stack <- Lwt_main.run (Builtins.Io.to_arg state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "output_data"
    |}]
;;

let%expect_test ">arg with existing string is noop" =
  let state = fresh_state () in
  state.stack <- [ String "already" ];
  state.stack <- Lwt_main.run (Builtins.Io.to_arg state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "already"
    |}]
;;

(* ========== Error Handling Tests ========== *)

let%expect_test "dup on empty stack fails" =
  let state = fresh_state () in
  (try state.stack <- Lwt_main.run (Builtins.Stack.dup state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    Stack underflow: dup
    |}]
;;

let%expect_test "swap on empty stack fails" =
  let state = fresh_state () in
  (try state.stack <- Lwt_main.run (Builtins.Stack.swap state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    Stack underflow: swap
    |}]
;;

let%expect_test "swap with single item fails" =
  let state = fresh_state () in
  state.stack <- [ String "only" ];
  (try state.stack <- Lwt_main.run (Builtins.Stack.swap state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    Stack underflow: swap
    |}]
;;

let%expect_test "drop on empty stack fails" =
  let state = fresh_state () in
  (try state.stack <- Lwt_main.run (Builtins.Stack.drop state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    Stack underflow: drop
    |}]
;;

let%expect_test "over on empty stack fails" =
  let state = fresh_state () in
  (try state.stack <- Lwt_main.run (Builtins.Stack.over state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    Stack underflow: over
    |}]
;;

let%expect_test "over with single item fails" =
  let state = fresh_state () in
  state.stack <- [ String "only" ];
  (try state.stack <- Lwt_main.run (Builtins.Stack.over state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    Stack underflow: over
    |}]
;;

let%expect_test "rot with insufficient items fails" =
  let state = fresh_state () in
  state.stack <- [ String "b"; String "a" ];
  (try state.stack <- Lwt_main.run (Builtins.Stack.rot state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    Stack underflow: rot
    |}]
;;

let%expect_test "dot on empty stack fails" =
  let state = fresh_state () in
  (try state.stack <- Lwt_main.run (Builtins.Io.dot state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    Stack underflow: .
    |}]
;;

let%expect_test "type on empty stack fails" =
  let state = fresh_state () in
  (try state.stack <- Lwt_main.run (Builtins.Io.type_word state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    Stack underflow: type
    |}]
;;

let%expect_test ">output on empty stack fails" =
  let state = fresh_state () in
  (try state.stack <- Lwt_main.run (Builtins.Io.to_output state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    Stack underflow: >output
    |}]
;;

let%expect_test ">output with integer fails" =
  let state = fresh_state () in
  state.stack <- [ Int 42 ];
  (try state.stack <- Lwt_main.run (Builtins.Io.to_output state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    >output: requires string
    |}]
;;

let%expect_test ">string on empty stack fails" =
  let state = fresh_state () in
  (try state.stack <- Lwt_main.run (Builtins.Io.to_string state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    Stack underflow: >string
    |}]
;;

(* ========== Environment Variable Tests ========== *)

let%expect_test "getenv reads environment variable" =
  let state = fresh_state () in
  Core_unix.putenv ~key:"TEST_VAR" ~data:"test_value";
  Lwt_main.run (Eval.eval_line state "TEST_VAR getenv");
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    test_value
    |}]
;;

let%expect_test "getenv with nonexistent variable returns empty" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "NONEXISTENT_VAR_12345 getenv");
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|

    |}]
;;

let%expect_test "setenv then getenv" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "my_value MY_VAR setenv");
  Lwt_main.run (Eval.eval_line state "MY_VAR getenv");
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {| my_value |}]
;;

let%expect_test "unsetenv removes variable" =
  let state = fresh_state () in
  Core_unix.putenv ~key:"TEMP_VAR" ~data:"temp";
  Lwt_main.run (Eval.eval_line state "TEMP_VAR unsetenv");
  Lwt_main.run (Eval.eval_line state "TEMP_VAR getenv");
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|

    |}]
;;

let%expect_test "env lists environment variables" =
  let state = fresh_state () in
  Core_unix.putenv ~key:"FSH_TEST" ~data:"present";
  Lwt_main.run (Eval.eval_line state "env");
  (* Check if FSH_TEST=present is in stack *)
  let has_test_var =
    List.exists state.stack ~f:(function
      | String s -> String.is_substring s ~substring:"FSH_TEST=present"
      | _ -> false)
  in
  print_endline (Bool.to_string has_test_var);
  [%expect {|
    true
    |}]
;;

let%expect_test "env-append adds to environment variable" =
  let state = fresh_state () in
  Core_unix.putenv ~key:"MY_PATH" ~data:"/usr/bin";
  Lwt_main.run (Eval.eval_line state "/opt/bin MY_PATH env-append");
  Lwt_main.run (Eval.eval_line state "MY_PATH getenv");
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {| /usr/bin:/opt/bin |}]
;;

let%expect_test "env-append creates new variable if not exists" =
  let state = fresh_state () in
  Core_unix.unsetenv "NEW_VAR_TEST";
  Lwt_main.run (Eval.eval_line state "/new/path NEW_VAR_TEST env-append");
  Lwt_main.run (Eval.eval_line state "NEW_VAR_TEST getenv");
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {| /new/path |}]
;;

let%expect_test "env-prepend adds to front of environment variable" =
  let state = fresh_state () in
  Core_unix.putenv ~key:"MY_PATH2" ~data:"/usr/bin";
  Lwt_main.run (Eval.eval_line state "/priority/bin MY_PATH2 env-prepend");
  Lwt_main.run (Eval.eval_line state "MY_PATH2 getenv");
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {| /priority/bin:/usr/bin |}]
;;

let%expect_test "env-prepend creates new variable if not exists" =
  let state = fresh_state () in
  Core_unix.unsetenv "NEW_VAR_TEST2";
  Lwt_main.run (Eval.eval_line state "/new/path NEW_VAR_TEST2 env-prepend");
  Lwt_main.run (Eval.eval_line state "NEW_VAR_TEST2 getenv");
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {| /new/path |}]
;;

let%expect_test "environment operations chain" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "/first MY_ENV setenv");
  Lwt_main.run (Eval.eval_line state "/second MY_ENV env-append");
  Lwt_main.run (Eval.eval_line state "/priority MY_ENV env-prepend");
  Lwt_main.run (Eval.eval_line state "MY_ENV getenv");
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {| /priority:/first:/second |}]
;;

(* ========== Arithmetic Operator Tests ========== *)

let%expect_test "addition" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "3 4 +");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 7
    |}]
;;

let%expect_test "subtraction" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "10 3 -");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 7
    |}]
;;

let%expect_test "multiplication" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "6 7 *");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 42
    |}]
;;

let%expect_test "division" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "20 4 /");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 5
    |}]
;;

let%expect_test "division with truncation" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "7 2 /");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 3
    |}]
;;

let%expect_test "modulo" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "7 3 mod");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "division and modulo" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "17 5 /mod");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <2> 2 3
    |}]
;;

let%expect_test "multiply then divide" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "3 4 2 */");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 6
    |}]
;;

let%expect_test "negative numbers in arithmetic" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "-5 3 +");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> -2
    |}]
;;

let%expect_test "chained arithmetic" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "2 3 + 4 *");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 20
    |}]
;;

(* ========== Comparison Operator Tests ========== *)

let%expect_test "equality true" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "5 5 =");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "equality false" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "3 7 =");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "string equality true" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "hello hello =");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "string equality false" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "hello world =");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "greater than true" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "10 5 >");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "greater than false" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "3 8 >");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "less than true" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "3 10 <");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "less than false" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "8 5 <");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

(* ========== Math Error Handling Tests ========== *)

let%expect_test "division by zero" =
  let state = fresh_state () in
  (try Lwt_main.run (Eval.eval_line state "5 0 /") with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    /: division by zero
    |}]
;;

let%expect_test "modulo by zero" =
  let state = fresh_state () in
  (try Lwt_main.run (Eval.eval_line state "5 0 mod") with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    mod: division by zero
    |}]
;;

let%expect_test "addition underflow" =
  let state = fresh_state () in
  state.stack <- [ Int 5 ];
  (try state.stack <- Lwt_main.run (Builtins.Computation.add state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    Stack underflow: +
    |}]
;;

let%expect_test "arithmetic with non-integers" =
  let state = fresh_state () in
  state.stack <- [ String "hello"; Int 5 ];
  (try state.stack <- Lwt_main.run (Builtins.Computation.add state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    +: requires two integers
    |}]
;;

(* ========== Directory Navigation Tests ========== *)

let%expect_test "cd changes directory" =
  let state = fresh_state () in
  let original = Core_unix.getcwd () in
  state.stack <- [ String "/tmp" ];
  Lwt_main.run (Builtins.System.cd_word state);
  let new_dir = Core_unix.getcwd () in
  print_endline (if String.equal new_dir "/tmp" then "success" else "failed");
  Core_unix.chdir original;  (* Restore *)
  [%expect {|
    success
    |}]
;;

let%expect_test "cd with tilde expansion" =
  let state = fresh_state () in
  let original = Core_unix.getcwd () in
  state.stack <- [ String "~" ];
  Lwt_main.run (Builtins.System.cd_word state);
  let new_dir = Core_unix.getcwd () in
  let home = Option.value (Sys.getenv "HOME") ~default:"" in
  print_endline (if String.equal new_dir home then "success" else "failed");
  Core_unix.chdir original;  (* Restore *)
  [%expect {|
    success
    |}]
;;

let%expect_test "cd removes path from stack" =
  let state = fresh_state () in
  let original = Core_unix.getcwd () in
  state.stack <- [ String "/tmp" ];
  Lwt_main.run (Builtins.System.cd_word state);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  Core_unix.chdir original;  (* Restore *)
  [%expect {|
    <0>
    |}]
;;

let%expect_test "cd with nonexistent directory" =
  let state = fresh_state () in
  state.stack <- [ String "/nonexistent_directory_xyz" ];
  (try Lwt_main.run (Builtins.System.cd_word state) with
   | Failure msg -> print_endline (if String.is_prefix msg ~prefix:"cd: " then "error caught" else msg));
  [%expect {|
    error caught
    |}]
;;

let%expect_test "cd with empty stack" =
  let state = fresh_state () in
  (try Lwt_main.run (Builtins.System.cd_word state) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    Stack underflow: cd
    |}]
;;

let%expect_test "cd with non-string" =
  let state = fresh_state () in
  state.stack <- [ Int 42 ];
  (try Lwt_main.run (Builtins.System.cd_word state) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    cd: requires string
    |}]
;;

let%expect_test "pushd saves directory and changes" =
  let state = fresh_state () in
  let original = Core_unix.getcwd () in
  state.stack <- [ String "/tmp" ];
  Lwt_main.run (Builtins.System.pushd_word state);
  let new_dir = Core_unix.getcwd () in
  print_endline (if String.equal new_dir "/tmp" then "changed" else "not changed");
  print_endline (if List.length state.dir_stack = 1 then "saved" else "not saved");
  Core_unix.chdir original;  (* Restore *)
  [%expect {|
    changed
    saved
    |}]
;;

let%expect_test "pushd then popd returns to original" =
  let state = fresh_state () in
  let original = Core_unix.getcwd () in
  state.stack <- [ String "/tmp" ];
  Lwt_main.run (Builtins.System.pushd_word state);
  Lwt_main.run (Builtins.System.popd_word state);
  let final_dir = Core_unix.getcwd () in
  print_endline (if String.equal final_dir original then "success" else "failed");
  [%expect {|
    success
    |}]
;;

let%expect_test "multiple pushd and popd" =
  let state = fresh_state () in
  let original = Core_unix.getcwd () in
  (* Push to /tmp *)
  state.stack <- [ String "/tmp" ];
  Lwt_main.run (Builtins.System.pushd_word state);
  (* Push to / *)
  state.stack <- [ String "/" ];
  Lwt_main.run (Builtins.System.pushd_word state);
  (* Now dir_stack should have [/tmp, original] *)
  print_endline (sprintf "stack depth: %d" (List.length state.dir_stack));
  (* Pop back to /tmp *)
  Lwt_main.run (Builtins.System.popd_word state);
  print_endline (if String.equal (Core_unix.getcwd ()) "/tmp" then "at /tmp" else "not at /tmp");
  (* Pop back to original *)
  Lwt_main.run (Builtins.System.popd_word state);
  print_endline (if String.equal (Core_unix.getcwd ()) original then "at original" else "not at original");
  [%expect {|
    stack depth: 2
    at /tmp
    at original
    |}]
;;

let%expect_test "popd with empty directory stack" =
  let state = fresh_state () in
  (try Lwt_main.run (Builtins.System.popd_word state) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    popd: directory stack empty
    |}]
;;

let%expect_test "pushd with empty stack" =
  let state = fresh_state () in
  (try Lwt_main.run (Builtins.System.pushd_word state) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    Stack underflow: pushd
    |}]
;;

let%expect_test "pushd with non-string" =
  let state = fresh_state () in
  state.stack <- [ Int 42 ];
  (try Lwt_main.run (Builtins.System.pushd_word state) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    pushd: requires string
    |}]
;;

(* ========== Exit Code Tests ========== *)

let%expect_test "? pushes exit code onto stack" =
  let state = fresh_state () in
  state.last_exit_code <- 42;
  Lwt_main.run (Builtins.System.exit_code_word state);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 42
    |}]
;;

let%expect_test "successful command sets exit code to 0" =
  let state = fresh_state () in
  state.stack <- [ String "true" ];
  Lwt_main.run (Builtins.exec_word state);
  Lwt_main.run (Builtins.System.exit_code_word state);
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    0
    |}]
;;

let%expect_test "failed command sets non-zero exit code" =
  let state = fresh_state () in
  state.stack <- [ String "false" ];
  Lwt_main.run (Builtins.exec_word state);
  Lwt_main.run (Builtins.System.exit_code_word state);
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    1
    |}]
;;

let%expect_test "exit code persists across operations" =
  let state = fresh_state () in
  (* Run false to set exit code to 1 *)
  state.stack <- [ String "false" ];
  Lwt_main.run (Builtins.exec_word state);
  (* Do some stack operations *)
  state.stack <- [ String "hello" ];
  state.stack <- Lwt_main.run (Builtins.Stack.dup state.stack);
  state.stack <- Lwt_main.run (Builtins.Stack.drop state.stack);
  (* Check exit code is still 1 *)
  Lwt_main.run (Builtins.System.exit_code_word state);
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    1
    |}]
;;

let%expect_test "multiple ? calls all return same exit code" =
  let state = fresh_state () in
  state.stack <- [ String "false" ];
  Lwt_main.run (Builtins.exec_word state);
  state.stack <- Lwt_main.run (Builtins.Stack.drop state.stack);  (* Drop output *)
  Lwt_main.run (Builtins.System.exit_code_word state);
  Lwt_main.run (Builtins.System.exit_code_word state);
  Lwt_main.run (Builtins.System.exit_code_word state);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <3> 1 1 1
    |}]
;;

let%expect_test "exit code updates with each command" =
  let state = fresh_state () in
  (* Run true (exit 0) *)
  state.stack <- [ String "true" ];
  Lwt_main.run (Builtins.exec_word state);
  print_endline (sprintf "After true: %d" state.last_exit_code);
  (* Run false (exit 1) *)
  state.stack <- [ String "false" ];
  Lwt_main.run (Builtins.exec_word state);
  print_endline (sprintf "After false: %d" state.last_exit_code);
  (* Run true again (exit 0) *)
  state.stack <- [ String "true" ];
  Lwt_main.run (Builtins.exec_word state);
  print_endline (sprintf "After true again: %d" state.last_exit_code);
  [%expect {|
    After true: 0
    After false: 1
    After true again: 0
    |}]
;;

let%expect_test "initial exit code is 0" =
  let state = fresh_state () in
  Lwt_main.run (Builtins.System.exit_code_word state);
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    0
    |}]
;;

(* ========== File Redirection Tests ========== *)

let%expect_test ">file writes output to file" =
  let state = fresh_state () in
  let tmpfile = Stdlib.Filename.temp_file "fsh_test" ".txt" in
  state.stack <- [ String tmpfile; Output "hello world\n" ];
  Lwt_main.run (Builtins.Io.write_file state);
  let content = In_channel.read_all tmpfile in
  print_endline content;
  Core_unix.unlink tmpfile;
  [%expect {|
    hello world
    |}]
;;

let%expect_test ">file empties the stack" =
  let state = fresh_state () in
  let tmpfile = Stdlib.Filename.temp_file "fsh_test" ".txt" in
  state.stack <- [ String tmpfile; Output "test" ];
  Lwt_main.run (Builtins.Io.write_file state);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  Core_unix.unlink tmpfile;
  [%expect {|
    <0>
    |}]
;;

let%expect_test ">file sets exit code 0 on success" =
  let state = fresh_state () in
  let tmpfile = Stdlib.Filename.temp_file "fsh_test" ".txt" in
  state.stack <- [ String tmpfile; Output "test" ];
  Lwt_main.run (Builtins.Io.write_file state);
  print_endline (sprintf "exit code: %d" state.last_exit_code);
  Core_unix.unlink tmpfile;
  [%expect {|
    exit code: 0
    |}]
;;

let%expect_test ">file with nonexistent directory sets exit code 1" =
  let state = fresh_state () in
  state.stack <- [ String "/nonexistent/dir/file.txt"; Output "test" ];
  (try Lwt_main.run (Builtins.Io.write_file state) with
   | Failure msg -> print_endline (if String.is_prefix msg ~prefix:">file: " then "error caught" else msg));
  print_endline (sprintf "exit code: %d" state.last_exit_code);
  [%expect {|
    error caught
    exit code: 1
    |}]
;;

let%expect_test ">file with empty stack" =
  let state = fresh_state () in
  (try Lwt_main.run (Builtins.Io.write_file state) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    Stack underflow: >file
    |}]
;;

let%expect_test ">file with wrong types (string instead of output)" =
  let state = fresh_state () in
  state.stack <- [ String "file.txt"; String "not an output" ];
  (try Lwt_main.run (Builtins.Io.write_file state) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    >file: requires output (not string)
    |}]
;;

let%expect_test ">>file appends to file" =
  let state = fresh_state () in
  let tmpfile = Stdlib.Filename.temp_file "fsh_test" ".txt" in
  (* First write *)
  state.stack <- [ String tmpfile; Output "first\n" ];
  Lwt_main.run (Builtins.Io.write_file state);
  (* Append *)
  state.stack <- [ String tmpfile; Output "second\n" ];
  Lwt_main.run (Builtins.Io.append_file state);
  (* Check contents *)
  let content = In_channel.read_all tmpfile in
  print_string content;
  Core_unix.unlink tmpfile;
  [%expect {|
    first
    second
    |}]
;;

let%expect_test ">>file multiple appends" =
  let state = fresh_state () in
  let tmpfile = Stdlib.Filename.temp_file "fsh_test" ".txt" in
  (* Write initial *)
  state.stack <- [ String tmpfile; Output "line1\n" ];
  Lwt_main.run (Builtins.Io.write_file state);
  (* Append multiple times *)
  state.stack <- [ String tmpfile; Output "line2\n" ];
  Lwt_main.run (Builtins.Io.append_file state);
  state.stack <- [ String tmpfile; Output "line3\n" ];
  Lwt_main.run (Builtins.Io.append_file state);
  (* Check contents *)
  let content = In_channel.read_all tmpfile in
  print_string content;
  Core_unix.unlink tmpfile;
  [%expect {|
    line1
    line2
    line3
    |}]
;;

let%expect_test ">>file with empty stack" =
  let state = fresh_state () in
  (try Lwt_main.run (Builtins.Io.append_file state) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    Stack underflow: >>file
    |}]
;;

let%expect_test ">>file with wrong types" =
  let state = fresh_state () in
  state.stack <- [ String "file.txt"; Int 42 ];
  (try Lwt_main.run (Builtins.Io.append_file state) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    >>file: requires output (not int)
    |}]
;;

let%expect_test ">file overwrites existing file" =
  let state = fresh_state () in
  let tmpfile = Stdlib.Filename.temp_file "fsh_test" ".txt" in
  (* First write *)
  state.stack <- [ String tmpfile; Output "original content\n" ];
  Lwt_main.run (Builtins.Io.write_file state);
  (* Overwrite *)
  state.stack <- [ String tmpfile; Output "new content\n" ];
  Lwt_main.run (Builtins.Io.write_file state);
  (* Check contents *)
  let content = In_channel.read_all tmpfile in
  print_endline content;
  Core_unix.unlink tmpfile;
  [%expect {|
    new content
    |}]
;;

(* ========== Additional Comparison Operator Tests ========== *)

let%expect_test ">= greater than or equal" =
  let state = fresh_state () in
  state.stack <- [ Int 3; Int 5 ];
  state.stack <- Lwt_main.run (Builtins.Computation.gte state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    1
    |}]
;;

let%expect_test ">= equal case" =
  let state = fresh_state () in
  state.stack <- [ Int 5; Int 5 ];
  state.stack <- Lwt_main.run (Builtins.Computation.gte state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    1
    |}]
;;

let%expect_test ">= less than case" =
  let state = fresh_state () in
  state.stack <- [ Int 7; Int 5 ];
  state.stack <- Lwt_main.run (Builtins.Computation.gte state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    0
    |}]
;;

let%expect_test "<= less than or equal" =
  let state = fresh_state () in
  state.stack <- [ Int 7; Int 5 ];
  state.stack <- Lwt_main.run (Builtins.Computation.lte state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    1
    |}]
;;

let%expect_test "<= equal case" =
  let state = fresh_state () in
  state.stack <- [ Int 5; Int 5 ];
  state.stack <- Lwt_main.run (Builtins.Computation.lte state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    1
    |}]
;;

let%expect_test "<= greater than case" =
  let state = fresh_state () in
  state.stack <- [ Int 3; Int 5 ];
  state.stack <- Lwt_main.run (Builtins.Computation.lte state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    0
    |}]
;;

let%expect_test "<> not equal with integers" =
  let state = fresh_state () in
  state.stack <- [ Int 3; Int 5 ];
  state.stack <- Lwt_main.run (Builtins.Computation.neq state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    1
    |}]
;;

let%expect_test "<> equal integers" =
  let state = fresh_state () in
  state.stack <- [ Int 5; Int 5 ];
  state.stack <- Lwt_main.run (Builtins.Computation.neq state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    0
    |}]
;;

let%expect_test "<> not equal with strings" =
  let state = fresh_state () in
  state.stack <- [ String "world"; String "hello" ];
  state.stack <- Lwt_main.run (Builtins.Computation.neq state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    1
    |}]
;;

let%expect_test "<> equal strings" =
  let state = fresh_state () in
  state.stack <- [ String "hello"; String "hello" ];
  state.stack <- Lwt_main.run (Builtins.Computation.neq state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    0
    |}]
;;

(* ========== Boolean Logic Operator Tests ========== *)

(* AND operator tests *)

let%expect_test "and: true and true" =
  let state = fresh_state () in
  state.stack <- [ Int 1; Int 1 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_and state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "and: true and false" =
  let state = fresh_state () in
  state.stack <- [ Int 0; Int 1 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_and state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "and: false and true" =
  let state = fresh_state () in
  state.stack <- [ Int 1; Int 0 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_and state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "and: false and false" =
  let state = fresh_state () in
  state.stack <- [ Int 0; Int 0 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_and state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "and: non-zero values as true" =
  let state = fresh_state () in
  state.stack <- [ Int 3; Int 5 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_and state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "and: negative and positive" =
  let state = fresh_state () in
  state.stack <- [ Int 2; Int (-3) ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_and state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "and: stack underflow" =
  let state = fresh_state () in
  state.stack <- [ Int 5 ];
  (try state.stack <- Lwt_main.run (Builtins.Computation.bool_and state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {| Stack underflow: and |}]
;;

let%expect_test "and: type error with string" =
  let state = fresh_state () in
  state.stack <- [ String "test"; Int 1 ];
  (try state.stack <- Lwt_main.run (Builtins.Computation.bool_and state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    and: requires two integers
    |}]
;;

(* OR operator tests *)

let%expect_test "or: true or true" =
  let state = fresh_state () in
  state.stack <- [ Int 1; Int 1 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_or state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "or: true or false" =
  let state = fresh_state () in
  state.stack <- [ Int 0; Int 1 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_or state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "or: false or true" =
  let state = fresh_state () in
  state.stack <- [ Int 1; Int 0 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_or state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "or: false or false" =
  let state = fresh_state () in
  state.stack <- [ Int 0; Int 0 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_or state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "or: non-zero values as true" =
  let state = fresh_state () in
  state.stack <- [ Int 0; Int 5 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_or state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "or: negative or zero" =
  let state = fresh_state () in
  state.stack <- [ Int 0; Int (-3) ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_or state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "or: stack underflow" =
  let state = fresh_state () in
  state.stack <- [ Int 5 ];
  (try state.stack <- Lwt_main.run (Builtins.Computation.bool_or state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {| Stack underflow: or |}]
;;

let%expect_test "or: type error with output" =
  let state = fresh_state () in
  state.stack <- [ Output "test"; Int 1 ];
  (try state.stack <- Lwt_main.run (Builtins.Computation.bool_or state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    or: requires two integers
    |}]
;;

(* NOT operator tests *)

let%expect_test "not: not true" =
  let state = fresh_state () in
  state.stack <- [ Int 1 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_not state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "not: not false" =
  let state = fresh_state () in
  state.stack <- [ Int 0 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_not state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "not: not positive number" =
  let state = fresh_state () in
  state.stack <- [ Int 5 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_not state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "not: not negative number" =
  let state = fresh_state () in
  state.stack <- [ Int (-3) ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_not state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "not: stack underflow" =
  let state = fresh_state () in
  state.stack <- [];
  (try state.stack <- Lwt_main.run (Builtins.Computation.bool_not state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {| Stack underflow: not |}]
;;

let%expect_test "not: type error with string" =
  let state = fresh_state () in
  state.stack <- [ String "hello" ];
  (try state.stack <- Lwt_main.run (Builtins.Computation.bool_not state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    not: requires integer
    |}]
;;

(* XOR operator tests *)

let%expect_test "xor: true xor true" =
  let state = fresh_state () in
  state.stack <- [ Int 1; Int 1 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_xor state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "xor: true xor false" =
  let state = fresh_state () in
  state.stack <- [ Int 0; Int 1 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_xor state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "xor: false xor true" =
  let state = fresh_state () in
  state.stack <- [ Int 1; Int 0 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_xor state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "xor: false xor false" =
  let state = fresh_state () in
  state.stack <- [ Int 0; Int 0 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_xor state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "xor: non-zero xor non-zero" =
  let state = fresh_state () in
  state.stack <- [ Int 7; Int 5 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_xor state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "xor: non-zero xor zero" =
  let state = fresh_state () in
  state.stack <- [ Int 0; Int 5 ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_xor state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "xor: negative xor positive" =
  let state = fresh_state () in
  state.stack <- [ Int 3; Int (-2) ];
  state.stack <- Lwt_main.run (Builtins.Computation.bool_xor state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "xor: stack underflow" =
  let state = fresh_state () in
  state.stack <- [ Int 5 ];
  (try state.stack <- Lwt_main.run (Builtins.Computation.bool_xor state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {| Stack underflow: xor |}]
;;

let%expect_test "xor: type error with mixed types" =
  let state = fresh_state () in
  state.stack <- [ Int 1; String "test" ];
  (try state.stack <- Lwt_main.run (Builtins.Computation.bool_xor state.stack) with
   | Failure msg -> Lwt_main.run (Lwt_io.printl msg >>= fun () -> Lwt_io.flush Lwt_io.stdout));
  [%expect {|
    xor: requires two integers
    |}]
;;

(* Integration tests using eval_line *)

let%expect_test "boolean logic: and via eval" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "1 1 and");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "boolean logic: or via eval" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "0 1 or");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "boolean logic: not via eval" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "0 not");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "boolean logic: xor via eval" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "1 0 xor");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "boolean logic: complex expression" =
  let state = fresh_state () in
  (* (5 > 3) and (10 < 20) *)
  Lwt_main.run (Eval.eval_line state "5 3 > 10 20 < and");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;

let%expect_test "boolean logic: complex with not" =
  let state = fresh_state () in
  (* not (5 < 3) *)
  Lwt_main.run (Eval.eval_line state "5 3 < not");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 1
    |}]
;;
