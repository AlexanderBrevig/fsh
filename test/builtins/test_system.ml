open Core
open Fsh

(* Helper to create a fresh state for each test *)
let fresh_state () =
  let state = Types.create_state () in
  Builtins.register_builtins state.dict;
  state
;;

(* ========== Environment Variable Tests ========== *)

let%expect_test "getenv retrieves existing variable" =
  Core_unix.putenv ~key:"TEST_VAR" ~data:"test_value";
  let state = fresh_state () in
  state.stack <- [ String "TEST_VAR" ];
  state.stack <- Lwt_main.run (Builtins.System.getenv_word state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> "test_value"
    |}]
;;

let%expect_test "getenv with nonexistent variable returns empty string" =
  let state = fresh_state () in
  state.stack <- [ String "NONEXISTENT_VAR_12345" ];
  state.stack <- Lwt_main.run (Builtins.System.getenv_word state.stack);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> ""
    |}]
;;

let%expect_test "setenv sets environment variable" =
  let state = fresh_state () in
  state.stack <- [ String "NEW_VAR"; String "new_value" ];
  state.stack <- Lwt_main.run (Builtins.System.setenv_word state.stack);
  (match Sys.getenv "NEW_VAR" with
   | Some v -> print_endline v
   | None -> print_endline "not set");
  [%expect {| new_value |}]
;;

let%expect_test "unsetenv removes environment variable" =
  Core_unix.putenv ~key:"TO_REMOVE" ~data:"value";
  let state = fresh_state () in
  state.stack <- [ String "TO_REMOVE" ];
  state.stack <- Lwt_main.run (Builtins.System.unsetenv_word state.stack);
  (match Sys.getenv "TO_REMOVE" with
   | Some _ -> print_endline "still set"
   | None -> print_endline "removed");
  [%expect {| removed |}]
;;

let%expect_test "env_append appends to existing variable" =
  Core_unix.putenv ~key:"PATH_VAR" ~data:"/first";
  let state = fresh_state () in
  state.stack <- [ String "PATH_VAR"; String "/second" ];
  state.stack <- Lwt_main.run (Builtins.System.env_append state.stack);
  (match Sys.getenv "PATH_VAR" with
   | Some v -> print_endline v
   | None -> print_endline "not set");
  [%expect {| /first:/second |}]
;;

let%expect_test "env_prepend prepends to existing variable" =
  Core_unix.putenv ~key:"PATH_VAR2" ~data:"/second";
  let state = fresh_state () in
  state.stack <- [ String "PATH_VAR2"; String "/first" ];
  state.stack <- Lwt_main.run (Builtins.System.env_prepend state.stack);
  (match Sys.getenv "PATH_VAR2" with
   | Some v -> print_endline v
   | None -> print_endline "not set");
  [%expect {| /first:/second |}]
;;

(* ========== Directory Navigation Tests ========== *)

let%expect_test "cd changes directory" =
  let original = Core_unix.getcwd () in
  let state = fresh_state () in
  state.stack <- [ String "/tmp" ];
  Lwt_main.run (Builtins.System.cd_word state);
  let new_dir = Core_unix.getcwd () in
  Core_unix.chdir original;
  print_endline new_dir;
  [%expect {| /tmp |}]
;;

let%expect_test "pushd saves and changes directory" =
  let original = Core_unix.getcwd () in
  let state = fresh_state () in
  state.stack <- [ String "/tmp" ];
  Lwt_main.run (Builtins.System.pushd_word state);
  let new_dir = Core_unix.getcwd () in
  let saved_dir = List.hd_exn state.dir_stack in
  Core_unix.chdir original;
  Printf.printf "new: %s, saved matches original: %b\n" new_dir (String.equal saved_dir original);
  [%expect {| new: /tmp, saved matches original: true |}]
;;

let%expect_test "popd restores directory" =
  let original = Core_unix.getcwd () in
  let state = fresh_state () in
  state.stack <- [ String "/tmp" ];
  Lwt_main.run (Builtins.System.pushd_word state);
  Lwt_main.run (Builtins.System.popd_word state);
  let restored = Core_unix.getcwd () in
  print_endline (if String.equal original restored then "restored" else "not restored");
  [%expect {| restored |}]
;;

(* ========== Exit Code Tests ========== *)

let%expect_test "exit code defaults to 0" =
  let state = fresh_state () in
  Lwt_main.run (Builtins.System.exit_code_word state);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <1> 0
    |}]
;;

let%expect_test "exit code captures command failure" =
  let state = fresh_state () in
  state.stack <- [ String "false" ];
  Lwt_main.run (Builtins.System.exec_word state);
  Lwt_main.run (Builtins.System.exit_code_word state);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <2> 1 «»
    |}]
;;

let%expect_test "exit code captures command success" =
  let state = fresh_state () in
  state.stack <- [ String "true" ];
  Lwt_main.run (Builtins.System.exec_word state);
  Lwt_main.run (Builtins.System.exit_code_word state);
  state.stack <- Lwt_main.run (Builtins.Io.dot_s state.stack);
  [%expect {|
    <2> 0 «»
    |}]
;;
