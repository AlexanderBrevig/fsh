open Core

(* Exception for early exit from word definitions *)
exception Word_exit

(* Core value types on the stack *)
type value =
  | String of string
  | Int of int
  | Output of string  (* Output from a shell command, piped to next command *)
[@@deriving sexp, compare]

(* Stack is just a list of values *)
type stack = value list [@@deriving sexp]

(* Control flow target for skipping *)
type skip_target = [ `Else | `Then ]

(* Control flow state for if/then/else *)
type control_flow_state =
  | Normal
  | Skipping of { target : skip_target; depth : int }  (* Skip until target, track nesting depth *)

(* Loop type for identifying loop variants during collection *)
type loop_type =
  | BeginUntil    (* begin ... until *)
  | BeginWhile    (* begin ... while ... repeat *)
  | DoLoop        (* do ... loop *)
  | DoPlusLoop    (* do ... +loop *)

(* Loop state for tracking active loops - uses variant for type safety *)
type loop_info =
  | BeginUntilLoop of { body : string list }
  | BeginWhileLoop of { body : string list }
  | DoCountedLoop of { body : string list; start : int; limit : int; current : int }
  | DoPlusCountedLoop of { body : string list; start : int; limit : int; current : int }

(* Word types and state are mutually recursive *)
type word =
  | Builtin of (stack -> stack Lwt.t) * string option      (* Built-in function with optional doc *)
  | StateBuiltin of (state -> unit Lwt.t) * string option  (* Built-in with state access and optional doc *)
  | Defined of string list                                  (* User-defined word (list of tokens) *)
  | ShellCmd of string                                      (* External shell command *)

(* Dictionary maps word names to their definitions *)
and dictionary = (string, word) Hashtbl.t

(* Interpreter state *)
and state =
  { mutable stack : stack
  ; dict : dictionary
  ; mutable defining : string option  (* Currently defining a word *)
  ; mutable def_body : string list    (* Body of word being defined *)
  ; mutable dir_stack : string list   (* Directory stack for pushd/popd *)
  ; mutable last_exit_code : int      (* Exit code of last command *)
  ; mutable control_flow : control_flow_state  (* Control flow state for if/then/else *)
  ; mutable loop_stack : loop_info list  (* Stack of active loops for nesting *)
  ; mutable collecting_loop : (loop_type * string list * int) option  (* Collecting loop body with nesting depth *)
  ; mutable collecting_each : (string * (string * bool) list) option  (* (output_content, body_with_quotes) for each...then *)
  ; mutable custom_prompt : string option  (* Cached custom prompt from $prompt evaluation *)
  ; mutable prompt_eval_original_stack : stack option  (* Original stack during prompt evaluation for $stack/$in/$out *)
  }

let create_state () =
  { stack = []
  ; dict = Hashtbl.create (module String)
  ; defining = None
  ; def_body = []
  ; dir_stack = []
  ; last_exit_code = 0
  ; control_flow = Normal
  ; loop_stack = []
  ; collecting_loop = None
  ; collecting_each = None
  ; custom_prompt = None
  ; prompt_eval_original_stack = None
  }
;;
