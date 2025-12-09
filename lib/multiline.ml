open Core
open Types

(* Check if input is incomplete and needs more lines *)
let is_incomplete state line =
  (* Check for unclosed quotes *)
  let has_unclosed_quote s =
    let count = ref 0 in
    String.iter s ~f:(fun c -> if Char.equal c '"' then count := !count + 1);
    !count mod 2 <> 0
  in

  (* Check for word definition in progress *)
  let defining_incomplete = Option.is_some state.defining in

  (* Check for loop collection in progress *)
  let loop_incomplete = Option.is_some state.collecting_loop in

  (* Check for unclosed control flow structures by counting keywords *)
  let count_control_flow s =
    (* Split on all whitespace characters *)
    let tokens =
      String.split_on_chars s ~on:[' '; '\n'; '\r'; '\t']
      |> List.map ~f:String.strip
      |> List.filter ~f:(fun s -> not (String.is_empty s))
    in
    let count_keyword kw = List.count tokens ~f:(String.equal kw) in
    let if_count = count_keyword "if" in
    let then_count = count_keyword "then" in
    let begin_count = count_keyword "begin" in
    let until_count = count_keyword "until" in
    let repeat_count = count_keyword "repeat" in
    let do_count = count_keyword "do" in
    let loop_count = count_keyword "loop" in
    let plus_loop_count = count_keyword "+loop" in
    let colon_count = count_keyword ":" in
    let semicolon_count = count_keyword ";" in

    (* Check if any structures are unclosed *)
    if_count > then_count
    || begin_count > (until_count + repeat_count)
    || do_count > (loop_count + plus_loop_count)
    || colon_count > semicolon_count
  in

  has_unclosed_quote line
  || defining_incomplete
  || loop_incomplete
  || count_control_flow line
;;

(* Get continuation prompt for incomplete input *)
let continuation_prompt state =
  if Option.is_some state.defining then
    "... : "
  else if Option.is_some state.collecting_loop then
    "... loop "
  else
    "... "
;;
