module I = Parser.MenhirInterpreter

(* Use the incremental API to provide a simple yet effective and precise
   error signalling within the parser. This basically reimplements and
   slightly overrides the loop already provided within the
   Menhir API using the Parser.MenhirInterpreter.loop_handle helper. *)

let fail lexbuf checkpoint =
  Util.raise_syntax_error lexbuf @@ "syntax error at lexeme \"" ^ Lexing.lexeme lexbuf ^ "\"."

let loop lexbuf result =
  let supplier = I.lexer_lexbuf_to_supplier Scanner.token lexbuf in
  I.loop_handle Fun.id (fail lexbuf) supplier result

let parse lexbuf = loop lexbuf (Parser.Incremental.program lexbuf.lex_curr_p)
