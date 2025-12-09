open Core

(* ========== Configuration Module ==========
   Centralized configuration constants for the Forth shell.
   All hardcoded paths, version strings, and system defaults
   are defined here for easy maintenance and modification.
*)

(* ========== Version Information ========== *)

let version = "0.2"
let version_string = sprintf "Forth Shell v%s" version

(* ========== File Paths (relative to HOME) ========== *)

(* History file *)
let history_file_name = ".fsh_history"

(* Configuration file *)
let rc_file_name = ".fshrc"

(* ========== System Commands ========== *)

(* Shell used for executing commands *)
let shell_executable = "/bin/sh"

(* ========== Path Resolution Functions ========== *)

module Paths = struct
  (* Get the home directory *)
  let home_dir () = Sys.getenv "HOME"

  (* Get absolute path to history file *)
  let history () =
    match home_dir () with
    | Some home -> home ^ "/" ^ history_file_name
    | None -> history_file_name

  (* Get absolute path to rc file *)
  let rc () =
    match home_dir () with
    | Some home -> home ^ "/" ^ rc_file_name
    | None -> rc_file_name

  (* Get the base name (without directory path) for display *)
  let rc_basename () =
    match home_dir () with
    | Some _ -> "~/" ^ rc_file_name
    | None -> rc_file_name
end
