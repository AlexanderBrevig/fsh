open Core
open Fsh

(* Helper to create a fresh state for each test *)
let fresh_state () =
  let state = Types.create_state () in
  Builtins.register_builtins state.dict;
  state
;;

(* ========== Integer Parsing Tests ========== *)

let%expect_test "eval pushes integer literal" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "42");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 42
    |}]
;;

let%expect_test "eval pushes multiple integers" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "10 20 30");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <3> 30 20 10
    |}]
;;

let%expect_test "eval pushes negative integer" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "-123");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> -123
    |}]
;;

let%expect_test "eval handles mixed integers and strings" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "hello 42 world");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <3> "world" 42 "hello"
    |}]
;;

(* ========== Word Definition Tests ========== *)

let%expect_test "define and use simple word" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state ": double dup dup ;");
  Lwt_main.run (Eval.eval_line state "hello double");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <3> "hello" "hello" "hello"
    |}]
;;

let%expect_test "define word with multiple operations" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state ": swap2 swap swap ;");
  Lwt_main.run (Eval.eval_line state "a b c swap2");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {| <3> "c" "b" "a" |}]
;;

let%expect_test "redefine existing word" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state ": test 1 ;");
  Lwt_main.run (Eval.eval_line state "test");
  Lwt_main.run (Eval.eval_line state ": test 2 ;");
  Lwt_main.run (Eval.eval_line state "test");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <2> 2 1
    |}]
;;

let%expect_test "word definition with literals" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state ": greet hello world ;");
  Lwt_main.run (Eval.eval_line state "greet");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <2> "world" "hello"
    |}]
;;

let%expect_test "nested word definitions" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state ": dup2 dup dup ;");
  Lwt_main.run (Eval.eval_line state ": quad dup2 dup2 ;");
  Lwt_main.run (Eval.eval_line state "x quad");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {| <5> "x" "x" "x" "x" "x" |}]
;;

(* ========== Shell Command Execution Tests ========== *)

let%expect_test "shell command: hostname" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "hostname");
  (* Output will vary, just check we have an Output value *)
  let has_output =
    match state.stack with
    | [ Output _ ] -> true
    | _ -> false
  in
  print_endline (Bool.to_string has_output);
  [%expect {|
    true
    |}]
;;

let%expect_test "shell command: echo with arguments" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "test echo");
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {| |}]
;;

let%expect_test "shell command: echo with multiple arguments" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "world hello echo");
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    hello world
    |}]
;;

let%expect_test "unknown word pushes as string" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "unknown_word");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "unknown_word"
    |}]
;;

let%expect_test "command not in PATH becomes string literal" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "definitely_not_a_real_command_xyz");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "definitely_not_a_real_command_xyz"
    |}]
;;

let%expect_test "shell command with integer as argument via >arg" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "42 >arg echo");
  (* >arg converts Int to String, then echo receives "42" *)
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    42
    |}]
;;

let%expect_test "shell command with integer as argument via >string" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "3 >string echo");
  (* >string now also converts Int to String *)
  state.stack <- Lwt_main.run (Builtins.Io.dot state.stack);
  [%expect {|
    3
    |}]
;;

(* ========== Glob Expansion Tests ========== *)

let%expect_test "glob expansion with *.ml" =
  let state = fresh_state () in
  (* Create temp directory with test files *)
  let tmpdir = Stdlib.Filename.temp_file "fsh_test" "" in
  Core_unix.unlink tmpdir;
  Core_unix.mkdir tmpdir ~perm:0o755;
  Out_channel.write_all (tmpdir ^ "/file1.ml") ~data:"";
  Out_channel.write_all (tmpdir ^ "/file2.ml") ~data:"";
  Out_channel.write_all (tmpdir ^ "/file3.txt") ~data:"";
  (* Change to temp directory *)
  let original_dir = Core_unix.getcwd () in
  Core_unix.chdir tmpdir;
  (* Test glob expansion *)
  Lwt_main.run (Eval.eval_line state "*.ml");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  (* Cleanup *)
  Core_unix.chdir original_dir;
  Core_unix.unlink (tmpdir ^ "/file1.ml");
  Core_unix.unlink (tmpdir ^ "/file2.ml");
  Core_unix.unlink (tmpdir ^ "/file3.txt");
  Core_unix.rmdir tmpdir;
  [%expect {|
    <2> "file2.ml" "file1.ml"
    |}]
;;

let%expect_test "quoted glob does not expand" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state {|"*.ml"|});
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "*.ml"
    |}]
;;

let%expect_test "glob with no matches pushes nothing" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "*.nonexistent_extension_xyz");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <0>
    |}]
;;

let%expect_test "glob in subdirectory" =
  let state = fresh_state () in
  (* Create temp directory structure *)
  let tmpdir = Stdlib.Filename.temp_file "fsh_test" "" in
  Core_unix.unlink tmpdir;
  Core_unix.mkdir tmpdir ~perm:0o755;
  let subdir = tmpdir ^ "/subdir" in
  Core_unix.mkdir subdir ~perm:0o755;
  Out_channel.write_all (subdir ^ "/test1.txt") ~data:"";
  Out_channel.write_all (subdir ^ "/test2.txt") ~data:"";
  (* Test glob with path *)
  let original_dir = Core_unix.getcwd () in
  Core_unix.chdir tmpdir;
  Lwt_main.run (Eval.eval_line state "subdir/*.txt");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  (* Cleanup *)
  Core_unix.chdir original_dir;
  Core_unix.unlink (subdir ^ "/test1.txt");
  Core_unix.unlink (subdir ^ "/test2.txt");
  Core_unix.rmdir subdir;
  Core_unix.rmdir tmpdir;
  [%expect {|
    <2> "subdir/test2.txt" "subdir/test1.txt"
    |}]
;;

let%expect_test "glob with question mark" =
  let state = fresh_state () in
  (* Create temp directory with test files *)
  let tmpdir = Stdlib.Filename.temp_file "fsh_test" "" in
  Core_unix.unlink tmpdir;
  Core_unix.mkdir tmpdir ~perm:0o755;
  Out_channel.write_all (tmpdir ^ "/file1.txt") ~data:"";
  Out_channel.write_all (tmpdir ^ "/file2.txt") ~data:"";
  Out_channel.write_all (tmpdir ^ "/file10.txt") ~data:"";
  let original_dir = Core_unix.getcwd () in
  Core_unix.chdir tmpdir;
  (* Test ? matches single character *)
  Lwt_main.run (Eval.eval_line state "file?.txt");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  (* Cleanup *)
  Core_unix.chdir original_dir;
  Core_unix.unlink (tmpdir ^ "/file1.txt");
  Core_unix.unlink (tmpdir ^ "/file2.txt");
  Core_unix.unlink (tmpdir ^ "/file10.txt");
  Core_unix.rmdir tmpdir;
  [%expect {|
    <2> "file2.txt" "file1.txt"
    |}]
;;

let%expect_test "quoted string with spaces" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state {|"hello world"|});
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "hello world"
    |}]
;;

(* ========== Control Flow Tests ========== *)

let%expect_test "if then with true condition" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "1 if 42 then");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 42
    |}]
;;

let%expect_test "if then with false condition" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "0 if 42 then");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <0>
    |}]
;;

let%expect_test "if else then with true condition" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "1 if 42 else 99 then");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 42
    |}]
;;

let%expect_test "if else then with false condition" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "0 if 42 else 99 then");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 99
    |}]
;;

let%expect_test "nested if statements" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "1 if 1 if 42 then then");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 42
    |}]
;;

let%expect_test "nested if with false outer" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "0 if 1 if 42 then then");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <0>
    |}]
;;

let%expect_test "comparison with if" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "5 3 > if greater then");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "greater"
    |}]
;;

let%expect_test "comparison with if else" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "3 5 > if greater else smaller then");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "smaller"
    |}]
;;

let%expect_test "multiple statements in if branch" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "1 if 1 2 3 then");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <3> 3 2 1
    |}]
;;

let%expect_test "multiple statements in else branch" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state "0 if 1 2 else 3 4 then");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <2> 4 3
    |}]
;;

let%expect_test "if in word definition" =
  let state = fresh_state () in
  Lwt_main.run (Eval.eval_line state ": abs dup 0 < if 0 swap - then ;");
  Lwt_main.run (Eval.eval_line state "-5 abs");
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 5
    |}]
;;
