open Core
open Lwt
open Types

(* ========== Printing ========== *)

(* Print top of stack *)
let dot = function
  | [] -> Errors.stack_underflow "."
  | String s :: rest ->
    Lwt_io.printl s >>= fun () ->
    Lwt_io.flush Lwt_io.stdout >>= fun () ->
    return rest
  | Output s :: rest ->
    Lwt_io.printl s >>= fun () ->
    Lwt_io.flush Lwt_io.stdout >>= fun () ->
    return rest
  | Int i :: rest ->
    Lwt_io.printlf "%d" i >>= fun () ->
    Lwt_io.flush Lwt_io.stdout >>= fun () ->
    return rest
;;

(* Type without newline *)
let type_word = function
  | [] -> Errors.stack_underflow "type"
  | String s :: rest ->
    Lwt_io.printf "%s" s >>= fun () ->
    Lwt_io.flush Lwt_io.stdout >>= fun () ->
    return rest
  | Output s :: rest ->
    Lwt_io.printf "%s" s >>= fun () ->
    Lwt_io.flush Lwt_io.stdout >>= fun () ->
    return rest
  | Int i :: rest ->
    Lwt_io.printf "%d" i >>= fun () ->
    Lwt_io.flush Lwt_io.stdout >>= fun () ->
    return rest
;;

(* Stack display *)
let dot_s stack =
  Lwt_io.printf "<%d> " (List.length stack) >>= fun () ->
  Lwt_list.iter_s (function
    | String s -> Lwt_io.printf "\"%s\" " s
    | Output s -> Lwt_io.printf "«%s» " s
    | Int i -> Lwt_io.printf "%d " i) stack >>= fun () ->
  Lwt_io.printl "" >>= fun () ->
  Lwt_io.flush Lwt_io.stdout >>= fun () ->
  return stack
;;

(* ========== Type Conversions ========== *)

(* Convert String to Output (fake pipe) *)
let to_output = function
  | [] -> Errors.stack_underflow ">output"
  | String s :: rest -> return (Output s :: rest)
  | Output _ :: _ as stack -> return stack  (* Already an output *)
  | Int _ :: _ -> Errors.requires_type ~op:">output" ~typ:"string"
;;

(* Convert Output to String *)
let to_string = function
  | [] -> Errors.stack_underflow ">string"
  | Output s :: rest -> return (String s :: rest)
  | Int i :: rest -> return (String (Int.to_string i) :: rest)
  | String _ :: _ as stack -> return stack  (* Already a string *)
;;

(* Convert any value to String (for use as command argument) *)
let to_arg = function
  | [] -> Errors.stack_underflow ">arg"
  | String _ :: _ as stack -> return stack  (* Already a string *)
  | Int i :: rest -> return (String (Int.to_string i) :: rest)
  | Output s :: rest -> return (String s :: rest)
;;

(* ========== File Redirection ========== *)

(* Write Output to file: Output String -> *)
let write_file state =
  match state.Types.stack with
  | [] | [_] -> Errors.stack_underflow ">file"
  | String filename :: Output content :: rest ->
      Lwt.catch
        (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.output ~flags:[Core_unix.O_WRONLY; Core_unix.O_CREAT; Core_unix.O_TRUNC] filename (fun oc ->
            Lwt_io.write oc content) >>= fun () ->
          state.last_exit_code <- 0;
          state.stack <- rest;
          return ())
        (fun exn ->
          state.last_exit_code <- 1;
          failwith (sprintf ">file: %s" (Exn.to_string exn)))
  | String _ :: String _ :: _ -> Errors.requires_type ~op:">file" ~typ:"output (not string)"
  | String _ :: Int _ :: _ -> Errors.requires_type ~op:">file" ~typ:"output (not int)"
  | _ -> Errors.requires_type ~op:">file" ~typ:"filename and output"
;;

(* Append Output to file: Output String -> *)
let append_file state =
  match state.Types.stack with
  | [] | [_] -> Errors.stack_underflow ">>file"
  | String filename :: Output content :: rest ->
      Lwt.catch
        (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.output ~flags:[Core_unix.O_WRONLY; Core_unix.O_CREAT; Core_unix.O_APPEND] filename (fun oc ->
            Lwt_io.write oc content) >>= fun () ->
          state.last_exit_code <- 0;
          state.stack <- rest;
          return ())
        (fun exn ->
          state.last_exit_code <- 1;
          failwith (sprintf ">>file: %s" (Exn.to_string exn)))
  | String _ :: String _ :: _ -> Errors.requires_type ~op:">>file" ~typ:"output (not string)"
  | String _ :: Int _ :: _ -> Errors.requires_type ~op:">>file" ~typ:"output (not int)"
  | _ -> Errors.requires_type ~op:">>file" ~typ:"filename and output"
;;

(* ========== Registration ========== *)

let register dict =
  let reg = Builtins_base.register_builtin dict in
  let reg_state = Builtins_base.register_state_builtin dict in
  (* Printing *)
  reg ~name:"." ~fn:dot ~doc:"( a -- ) Print and remove top item with newline";
  reg ~name:"type" ~fn:type_word ~doc:"( a -- ) Print and remove top item without newline";
  reg ~name:".s" ~fn:dot_s ~doc:"( -- ) Display entire stack without modifying it";
  (* Type conversions *)
  reg ~name:">output" ~fn:to_output ~doc:"( string -- output ) Convert String to Output for piping";
  reg ~name:">string" ~fn:to_string ~doc:"( output/int -- string ) Convert Output or Int to String";
  reg ~name:">arg" ~fn:to_arg ~doc:"( a -- string ) Convert any value to String for command args";
  (* File redirection *)
  reg_state ~name:">file" ~fn:write_file ~doc:"( filename output -- ) Write output to file";
  reg_state ~name:">>file" ~fn:append_file ~doc:"( filename output -- ) Append output to file"
;;
