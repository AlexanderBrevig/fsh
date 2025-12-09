open Core
open Lwt
open LTerm_text
open Types

module S = React.S

(* Count inputs (String/Int) vs outputs (Output) on stack *)
let count_stack stack =
  List.fold stack ~init:(0, 0) ~f:(fun (inputs, outputs) item ->
    match item with
    | String _ | Int _ -> (inputs + 1, outputs)
    | Output _ -> (inputs, outputs + 1))
;;

(* Convert a value to string for prompt display *)
let value_to_string = function
  | String s -> s
  | Int n -> Int.to_string n
  | Output s -> s
;;

(* Evaluate custom $prompt if defined and cache in state *)
let update_custom_prompt state =
  let open Lwt.Infix in
  match Hashtbl.find state.dict "$prompt" with
  | None ->
      state.custom_prompt <- None;
      return ()
  | Some word ->
      (* Save original stack *)
      let original_stack = state.stack in
      state.stack <- [];

      (* Try to evaluate $prompt *)
      Lwt.catch
        (fun () ->
          (* Execute the word - it should build up the prompt on the stack *)
          (match word with
           | Builtin (fn, _) ->
               fn state.stack >>= fun new_stack ->
               state.stack <- new_stack;
               return ()
           | StateBuiltin (fn, _) ->
               fn state >>= fun () ->
               return ()
           | Defined tokens ->
               Lwt_list.iter_s (fun t -> Eval.eval_token state (t, false)) tokens
           | ShellCmd _ ->
               (* Shouldn't happen, but handle gracefully *)
               return ())
          >>= fun () ->

          (* Collect all values from stack and concatenate *)
          (* The final result should ideally be a single String after all concats *)
          let parts = List.rev_map state.stack ~f:value_to_string in
          let prompt_str = String.concat parts in

          (* Store in state and restore original stack *)
          state.custom_prompt <- Some prompt_str;
          state.stack <- original_stack;
          return ())
        (fun _exn ->
          (* On error, restore stack and clear custom prompt *)
          state.stack <- original_stack;
          state.custom_prompt <- None;
          return ())
;;

(* Create colorized prompt based on stack state or custom $prompt *)
let create_prompt state =
  match state.custom_prompt with
  | Some custom -> eval [S custom]
  | None ->
      (* Default prompt based on stack state *)
      let inputs, outputs = count_stack state.stack in
      let total = inputs + outputs in

      if total = 0 then
        eval [S"fsh"; B_fg LTerm_style.lcyan; S"> "; E_fg]
      else if outputs = 0 then
        eval [S"fsh"; B_fg LTerm_style.lyellow; S"["; S(Int.to_string inputs); S"]"; E_fg; B_fg LTerm_style.lcyan; S"> "; E_fg]
      else if inputs = 0 then
        eval [S"fsh"; B_fg LTerm_style.lgreen; S"[:"; S(Int.to_string outputs); S"]"; E_fg; B_fg LTerm_style.lcyan; S"> "; E_fg]
      else
        eval [S"fsh"; B_fg LTerm_style.lyellow; S"["; S(Int.to_string inputs); S":"; S(Int.to_string outputs); S"]"; E_fg; B_fg LTerm_style.lcyan; S"> "; E_fg]
;;

(* Create continuation prompt for multiline input *)
let create_continuation_prompt _state =
  eval [B_fg LTerm_style.white; S"... "; E_fg]
;;

(* History file path *)
let history_file =
  match Sys.getenv "HOME" with
  | Some home -> home ^ "/.fsh_history"
  | None -> ".fsh_history"
;;

(* Check if a string is an integer *)
let is_int s =
  try
    let _ = Int.of_string s in
    true
  with
  | _ -> false
;;

(* Control flow keywords *)
let control_flow_keywords =
  ["if"; "then"; "else"; "begin"; "until"; "while"; "repeat"; "do"; "loop"; "+loop"; ":"; ";"]
;;

(* Tokenize a string for syntax highlighting with position tracking *)
let tokenize_for_highlighting text =
  let s = Zed_string.to_utf8 text in
  let rec tokenize_helper pos in_quote current_token token_start acc =
    if pos >= String.length s then
      (* End of string *)
      let final_acc =
        if String.is_empty current_token then acc
        else (current_token, in_quote, token_start) :: acc
      in
      List.rev final_acc
    else
      let c = s.[pos] in
      match c with
      | '"' when not in_quote ->
          (* Start quote *)
          let new_acc =
            if String.is_empty current_token then acc
            else (current_token, false, token_start) :: acc
          in
          tokenize_helper (pos + 1) true "\"" pos new_acc
      | '"' when in_quote ->
          (* End quote *)
          let quoted_token = current_token ^ "\"" in
          tokenize_helper (pos + 1) false "" pos ((quoted_token, true, token_start) :: acc)
      | (' ' | '\t') when not in_quote ->
          (* Whitespace outside quotes *)
          let new_acc =
            if String.is_empty current_token then acc
            else (current_token, false, token_start) :: acc
          in
          tokenize_helper (pos + 1) false "" pos new_acc
      | _ ->
          (* Regular character *)
          let new_token_start = if String.is_empty current_token then pos else token_start in
          tokenize_helper (pos + 1) in_quote (current_token ^ String.make 1 c) new_token_start acc
  in
  tokenize_helper 0 false "" 0 []
;;

(* Get style for a token based on its type *)
let get_token_style state (token, is_quoted) =
  if is_quoted then
    (* Quoted strings are yellow *)
    LTerm_style.({ none with foreground = Some lyellow })
  else if List.mem control_flow_keywords token ~equal:String.equal then
    (* Control flow keywords are magenta *)
    LTerm_style.({ none with foreground = Some lmagenta })
  else if is_int token then
    (* Numbers are cyan *)
    LTerm_style.({ none with foreground = Some lcyan })
  else if Hashtbl.mem state.Types.dict token then
    (* Words in dictionary are green *)
    LTerm_style.({ none with foreground = Some lgreen })
  else
    (* Unknown words are default *)
    LTerm_style.none
;;

(* Load history from file *)
let load_history () =
  Lwt.catch
    (fun () ->
      Lwt_io.open_file ~mode:Lwt_io.Input history_file >>= fun ic ->
      let rec read_lines acc =
        Lwt.catch
          (fun () ->
            Lwt_io.read_line ic >>= fun line ->
            read_lines (Zed_string.of_utf8 line :: acc))
          (function
          | End_of_file -> return (List.rev acc)
          | e -> Lwt.fail e)
      in
      read_lines [] >>= fun lines ->
      Lwt_io.close ic >>= fun () ->
      return lines)
    (fun _ -> return [])
;;

(* Save history to file *)
let save_history history =
  Lwt.catch
    (fun () ->
      Lwt_io.open_file ~mode:Lwt_io.Output history_file >>= fun oc ->
      Lwt_list.iter_s
        (fun entry ->
          Lwt_io.write_line oc (Zed_string.to_utf8 entry))
        history >>= fun () ->
      Lwt_io.close oc)
    (fun _ -> return ())
;;

(* Custom read_line engine with syntax highlighting *)
class read_line ~term ~state = object(self)
  inherit LTerm_read_line.read_line ()
  inherit [Zed_string.t] LTerm_read_line.term term

  (* Show our custom prompt *)
  method! show_box = false

  (* Apply syntax highlighting to input *)
  method! stylise _last_draw =
    let edit = Zed_edit.edit self#context in
    let text = Zed_rope.to_string (Zed_edit.text edit) in
    let cursor_pos = Zed_edit.position self#context in
    let text_str = Zed_string.to_utf8 text in
    let tokens = tokenize_for_highlighting text in

    (* Build markup from tokens, preserving whitespace *)
    let rec build_markup acc last_end = function
      | [] ->
          (* Add any remaining text after last token *)
          if last_end < String.length text_str then
            acc @ [S (String.sub text_str ~pos:last_end ~len:(String.length text_str - last_end))]
          else
            acc
      | (token, is_quoted, pos) :: rest ->
          let style = get_token_style state (token, is_quoted) in
          let token_len = String.length token in
          (* Add any whitespace before this token *)
          let acc' =
            if pos > last_end then
              acc @ [S (String.sub text_str ~pos:last_end ~len:(pos - last_end))]
            else
              acc
          in
          (* Add markup directives for this token *)
          let markup =
            match style.LTerm_style.foreground with
            | Some color -> [B_fg color; S token; E_fg]
            | None -> [S token]
          in
          build_markup (acc' @ markup) (pos + token_len) rest
    in
    let markup = build_markup [] 0 tokens in
    let final_text = LTerm_text.eval markup in
    (final_text, cursor_pos)

  initializer
    (* Set the prompt *)
    self#set_prompt (S.const (create_prompt state))
end

(* Accumulated multiline input *)
let multiline_buffer = ref ""

(* Custom read_line for continuation prompts with syntax highlighting *)
class read_line_continuation ~term ~state = object(self)
  inherit LTerm_read_line.read_line ()
  inherit [Zed_string.t] LTerm_read_line.term term

  (* Show our custom prompt *)
  method! show_box = false

  (* Apply syntax highlighting to input *)
  method! stylise _last_draw =
    let edit = Zed_edit.edit self#context in
    let text = Zed_rope.to_string (Zed_edit.text edit) in
    let cursor_pos = Zed_edit.position self#context in
    let text_str = Zed_string.to_utf8 text in
    let tokens = tokenize_for_highlighting text in

    (* Build markup from tokens, preserving whitespace *)
    let rec build_markup acc last_end = function
      | [] ->
          (* Add any remaining text after last token *)
          if last_end < String.length text_str then
            acc @ [S (String.sub text_str ~pos:last_end ~len:(String.length text_str - last_end))]
          else
            acc
      | (token, is_quoted, pos) :: rest ->
          let style = get_token_style state (token, is_quoted) in
          let token_len = String.length token in
          (* Add any whitespace before this token *)
          let acc' =
            if pos > last_end then
              acc @ [S (String.sub text_str ~pos:last_end ~len:(pos - last_end))]
            else
              acc
          in
          (* Add markup directives for this token *)
          let markup =
            match style.LTerm_style.foreground with
            | Some color -> [B_fg color; S token; E_fg]
            | None -> [S token]
          in
          build_markup (acc' @ markup) (pos + token_len) rest
    in
    let markup = build_markup [] 0 tokens in
    let final_text = LTerm_text.eval markup in
    (final_text, cursor_pos)

  initializer
    (* Set the continuation prompt *)
    self#set_prompt (S.const (create_continuation_prompt state))
end

(* Read a line (or multiple lines) of input *)
let read_input term state =
  let rec read_lines acc is_first_line =
    Lwt.catch
      (fun () ->
        let rl =
          if is_first_line then
            new read_line ~term ~state
          else
            new read_line_continuation ~term ~state
        in
        rl#run >>= fun input ->
        (* Ensure terminal state is clean after line input *)
        LTerm.flush term >>= fun () ->
        let line = Zed_string.to_utf8 input in
        let combined = if String.is_empty acc then line else acc ^ "\n" ^ line in

        (* Check if we need more input *)
        if Multiline.is_incomplete state combined then
          read_lines combined false
        else
          return (Some combined))
      (function
      | LTerm_read_line.Interrupt -> return None  (* EOF or Ctrl-C *)
      | exn -> Lwt.fail exn)
  in
  read_lines "" true
;;

(* Terminal output function that works with lambda-term *)
let term_output = ref None

let set_term_output term =
  term_output := Some term

let term_print s =
  match !term_output with
  | Some term ->
      LTerm.fprint term s >>= fun () ->
      LTerm.flush term
  | None ->
      Lwt_io.print s >>= fun () ->
      Lwt_io.flush Lwt_io.stdout

let term_printl s =
  match !term_output with
  | Some term ->
      LTerm.fprintl term s >>= fun () ->
      LTerm.flush term
  | None ->
      Lwt_io.printl s >>= fun () ->
      Lwt_io.flush Lwt_io.stdout

(* Auto-type Output values: if top of stack is Output, print it but keep it on stack *)
let auto_type_output state =
  match state.stack with
  | Output s :: _ ->
      term_print s
  | _ -> return ()

(* Load and execute ~/.fshrc if it exists *)
let load_config_file state =
  let open Lwt.Infix in
  let config_path =
    match Sys.getenv "HOME" with
    | Some home -> home ^ "/.fshrc"
    | None -> ".fshrc"
  in
  Lwt.catch
    (fun () ->
      (* Check if file exists *)
      Lwt_unix.file_exists config_path >>= fun exists ->
      if exists then (
        (* Read and execute each line *)
        Lwt_io.with_file ~mode:Lwt_io.Input config_path (fun ic ->
          let rec loop () =
            Lwt.catch
              (fun () ->
                Lwt_io.read_line ic >>= fun line ->
                (* Skip empty lines and comments *)
                let trimmed = String.strip line in
                if String.is_empty trimmed || String.is_prefix trimmed ~prefix:"\\" then
                  loop ()
                else (
                  (* Execute line silently *)
                  Lwt.catch
                    (fun () -> Eval.eval_line state line)
                    (fun exn ->
                      (* Print error but continue *)
                      Lwt_io.printf "Warning in ~/.fshrc: %s\n" (Exn.to_string exn))
                  >>= fun () ->
                  loop ()))
              (function
                | End_of_file -> return ()
                | exn -> Lwt.fail exn)
          in
          loop ()))
      else
        return ())
    (fun _exn ->
      (* Silently ignore if config file can't be read *)
      return ())
;;

(* Simple non-TTY REPL loop *)
let run_simple () =
  let state = create_state () in
  Builtins.register_builtins state.dict;

  Lwt_main.run (
    (* Load config file *)
    load_config_file state >>= fun () ->

    Lwt_io.printl "Forth Shell v0.2 (simple mode)" >>= fun () ->
    Lwt_io.printl "Type 'exit' to quit, Ctrl-D for EOF" >>= fun () ->
    Lwt_io.printl "" >>= fun () ->

    let rec loop () =
      Lwt.catch
        (fun () ->
          Lwt_io.read_line Lwt_io.stdin >>= fun line ->
          if String.(strip line = "exit") || String.(strip line = "quit") then
            Lwt_io.printl "Goodbye!" >>= fun () ->
            return ()
          else (
            Lwt.catch
              (fun () ->
                Eval.eval_line state line >>= fun () ->
                (* Auto-type output if present *)
                auto_type_output state >>= fun () ->
                Lwt_io.printl "" >>= fun () ->
                Lwt_io.flush Lwt_io.stdout)
              (fun exn ->
                Lwt_io.printf "Error: %s\n" (Exn.to_string exn) >>= fun () ->
                Lwt_io.flush Lwt_io.stdout) >>= fun () ->
            loop ()))
        (function
          | End_of_file ->
              Lwt_io.printl "\nGoodbye!" >>= fun () ->
              return ()
          | exn -> Lwt.fail exn)
    in
    loop ()
  )

(* Main REPL loop with TTY support *)
let run_interactive () =
  let state = create_state () in
  Builtins.register_builtins state.dict;

  (* Initialize terminal *)
  Lwt_main.run (
    Lazy.force LTerm.stdout >>= fun term ->

    (* Set terminal for output *)
    set_term_output term;

    (* Load config file *)
    load_config_file state >>= fun () ->

    (* Print welcome message *)
    LTerm.fprintl term "Forth Shell v0.2 (lambda-term)" >>= fun () ->
    LTerm.fprintl term "Type 'exit' to quit, Ctrl-D for EOF" >>= fun () ->
    LTerm.fprintl term "" >>= fun () ->

    (* REPL loop *)
    let rec loop () =
      (* Update custom prompt before each input *)
      update_custom_prompt state >>= fun () ->
      read_input term state >>= function
      | None ->
          (* EOF or exit *)
          Lwt_io.printf "\nGoodbye!\n" >>= fun () ->
          return ()
      | Some line ->
          if String.(strip line = "exit") || String.(strip line = "quit") then
            Lwt_io.printf "Goodbye!\n" >>= fun () ->
            return ()
          else (

            (* Evaluate the line *)
            Lwt.catch
              (fun () ->
                (* Eval is now Lwt-based *)
                Eval.eval_line state line >>= fun () ->
                (* Auto-type output if present *)
                auto_type_output state >>= fun () ->
                (* Flush all output after evaluation *)
                Lwt_io.flush Lwt_io.stdout >>= fun () ->
                Lwt_io.flush Lwt_io.stderr >>= fun () ->
                (* Ensure we're on a new line before the next prompt *)
                Lwt_io.write Lwt_io.stdout "\n" >>= fun () ->
                Lwt_io.flush Lwt_io.stdout >>= fun () ->
                LTerm.flush term)
              (fun exn ->
                Lwt_io.printf "Error: %s\n" (Exn.to_string exn) >>= fun () ->
                Lwt_io.flush Lwt_io.stdout >>= fun () ->
                LTerm.flush term) >>= fun () ->
            loop ())
    in
    loop ()
  )

(* Main entry point - detect TTY and choose appropriate mode *)
let run () =
  (* Check if stdin is a TTY *)
  if Core_unix.isatty Core_unix.stdin then
    run_interactive ()
  else
    run_simple ()
;;
