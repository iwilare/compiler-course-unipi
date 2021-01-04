exception Syntax_error   of string
exception Lexing_error   of string
exception Semantic_error of string
exception Codegen_error  of string

(* Transforms a list of options into an option of list.
   Inspired by Haskell's `sequence` :: Monad m => t (m a) -> m (t a)
   function defined in the Traversable typeclass. *)
let rec sequence : 'a option list -> 'a list option = function
  | [] -> Some([])
  | (Some(x)) :: xs ->
    (match sequence xs with
      | Some(ls) -> Some(x :: ls)
      | None     -> None)
  | None :: _ -> None

let get_lexing_position lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  (line_number, column)

let raise_syntax_error lexbuf msg =
  let pos = get_lexing_position lexbuf in
  let m = Printf.sprintf "%d:%d: %s" (fst pos) (snd pos) msg in
  raise (Syntax_error m)

let raise_lexer_error lexbuf msg =
  let pos = get_lexing_position lexbuf in
  let m = Printf.sprintf "%d:%d: %s" (fst pos) (snd pos) msg in
  raise (Lexing_error m)

let raise_semantic_error (startp, endp) msg =
  let start_line_number = startp.Lexing.pos_lnum in
  let start_column_number = startp.Lexing.pos_cnum  - startp.Lexing.pos_bol + 1 in
  let end_line_number = endp.Lexing.pos_lnum in
  let end_column_number = endp.Lexing.pos_cnum  - endp.Lexing.pos_bol + 1 in
  let line = if start_line_number = end_line_number then
      string_of_int start_line_number
    else
      Printf.sprintf "%d-%d" start_line_number end_line_number
  in
  let column = if start_column_number = end_column_number then
      string_of_int start_column_number
    else
      Printf.sprintf "%d-%d" start_column_number end_column_number
  in
  let log = Printf.sprintf "%s:%s: %s" line column msg in
  raise (Semantic_error log)

let raise_codegen_error msg = raise (Codegen_error msg)
