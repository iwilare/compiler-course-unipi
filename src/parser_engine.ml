module I = Parser.MenhirInterpreter

let fail lexbuf checkpoint =
  Util.raise_syntax_error lexbuf @@ "syntax error at lexeme \"" ^ Lexing.lexeme lexbuf ^ "\"."

let loop lexbuf result =
  let supplier = I.lexer_lexbuf_to_supplier Scanner.token lexbuf in
  I.loop_handle Fun.id (fail lexbuf) supplier result

let parse lexbuf = loop lexbuf (Parser.Incremental.program lexbuf.lex_curr_p)
