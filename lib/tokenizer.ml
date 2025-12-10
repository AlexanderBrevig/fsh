open Core

(* Token with quote status and optional position *)
type token_info =
  { text : string
  ; quoted : bool
  ; position : int
  }

(* Tokenize with quote awareness and position tracking
   - include_quotes: if true, include quote marks in quoted tokens (for display)
   - Returns list of (token, is_quoted, position) tuples *)
let tokenize_with_positions ?(include_quotes=false) line =
  let rec tokenize_helper pos in_quote current_token token_start acc =
    if pos >= String.length line then
      (* End of line *)
      let final_acc =
        if String.is_empty current_token then acc
        else { text = current_token; quoted = in_quote; position = token_start } :: acc
      in
      List.rev final_acc
    else
      let c = line.[pos] in
      match c with
      | '"' when not in_quote ->
          (* Start quote *)
          let new_acc =
            if String.is_empty current_token then acc
            else { text = current_token; quoted = false; position = token_start } :: acc
          in
          let start_char = if include_quotes then "\"" else "" in
          tokenize_helper (pos + 1) true start_char pos new_acc
      | '"' when in_quote ->
          (* End quote *)
          let final_token =
            if include_quotes then current_token ^ "\"" else current_token
          in
          tokenize_helper (pos + 1) false "" pos
            ({ text = final_token; quoted = true; position = token_start } :: acc)
      | (' ' | '\n' | '\r' | '\t') when not in_quote ->
          (* Whitespace outside quotes - token separator *)
          let new_acc =
            if String.is_empty current_token then acc
            else { text = current_token; quoted = false; position = token_start } :: acc
          in
          tokenize_helper (pos + 1) false "" pos new_acc
      | _ ->
          (* Regular character *)
          let new_token_start = if String.is_empty current_token then pos else token_start in
          tokenize_helper (pos + 1) in_quote (current_token ^ String.make 1 c) new_token_start acc
  in
  tokenize_helper 0 false "" 0 []
;;

(* Simple tokenize function that returns (token, is_quoted) tuples
   This is the interface used by eval.ml *)
let tokenize line =
  let tokens = tokenize_with_positions ~include_quotes:false line in
  List.map tokens ~f:(fun t -> (t.text, t.quoted))
;;

(* Check if a string is an integer *)
let is_int s =
  try
    let _ = Int.of_string s in
    true
  with
  | _ -> false
;;
