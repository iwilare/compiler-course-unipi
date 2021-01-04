{
open Parser

let create_hashtable init =
  let tbl = Hashtbl.create (List.length init) in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

let keyword_table =
    create_hashtable [
        ("true",   TRUE);
        ("false",  FALSE);
        ("if",     IF);
        ("return", RETURN);
        ("else",   ELSE);
        ("for",    FOR);
        ("while",  WHILE);
        ("do",     DO);
        ("int",    INT);
        ("char",   CHAR);
        ("void",   VOID);
        ("float",  FLOAT);
        ("bool",   BOOL);
        ("NULL",   NULL);
    ]
}

let letter     = ['a'-'z' 'A'-'Z']
let digit      = ['0'-'9']
let identifier = (letter | '_') (letter | digit | '_')*

let integer    = digit digit*
let mantissa   = '.' digit*
let exponent   = ['e' 'E'] ['-' '+']? digit+
let float      = digit* mantissa? exponent?

let whitespace = [' ' '\t']+
let newline    = ['\r' '\n']

rule token = parse
    | newline    { Lexing.new_line lexbuf; token lexbuf }
    | whitespace { token lexbuf                         }
    | "//"       { line_comment lexbuf                  }
    | "/*"       { block_comment 0 lexbuf               }

    | "("        { LPAREN  }
    | ")"        { RPAREN  }
    | "["        { LSQUARE }
    | "]"        { RSQUARE }
    | "{"        { LCURLY  }
    | "}"        { RCURLY  }

    | ","        { COMMA        }
    | ";"        { SEMICOLON    }
    | "="        { ASSIGNMENT   }

    | "!"        { NOT          }
    | "&"        { ADDRESS_OF   }

    | "||"       { OR           }
    | "&&"       { AND          }
    | "=="       { EQ           }
    | "!="       { NEQ          }
    | ">"        { GT           }
    | "<"        { LT           }
    | ">="       { GE           }
    | "<="       { LE           }

    | "++"       { INCREMENT    }
    | "--"       { DECREMENT    }

    | "+="       { ASSIGNMENT_PLUS   }
    | "-="       { ASSIGNMENT_MINUS  }
    | "*="       { ASSIGNMENT_TIMES  }
    | "/="       { ASSIGNMENT_DIVIDE }
    | "%="       { ASSIGNMENT_MOD    }

    | "+"        { PLUS         }
    | "-"        { MINUS        }
    | "*"        { TIMES        } (* Or pointer dereference*)
    | "/"        { DIVIDE       }
    | "%"        { MOD          }

    | identifier as id { try Hashtbl.find keyword_table id with Not_found -> ID(id) }
    | "'"              { CHARACTER(character_literal lexbuf) }
    | "\""             { STRING(string_literal lexbuf) }
    | integer as iv    { INTEGER(int_of_string iv)     }
    | float   as fv    { FLOATING(float_of_string fv)  }
    | eof              { EOF                           }
    | _ as c           { Util.raise_lexer_error lexbuf ("Illegal character \'" ^ Char.escaped c ^ "\'.") }
and line_comment = parse
    | newline { Lexing.new_line lexbuf; token lexbuf }
    | _       { line_comment lexbuf }
and block_comment level = parse
    | "*/"    { if level = 0 then token lexbuf else block_comment (level-1) lexbuf }
    | "/*"    { block_comment (level+1) lexbuf                                     }
    | newline { Lexing.new_line lexbuf; block_comment level lexbuf                 }
    | _       { block_comment level lexbuf                                         }
    | eof     { Util.raise_lexer_error lexbuf "Unclosed comment, found EOF."       }
and character_literal = parse
    | '\n' | eof { Util.raise_lexer_error lexbuf "Unclosed character literal." }
    | ""         { let c = character lexbuf in (character_end lexbuf; c) }
and character_end = parse
    | '\''       { }
    | _ as c     { Util.raise_lexer_error lexbuf ("Invalid character \'" ^ Char.escaped c ^ "\' in character literal, expected end.") }
and string_literal = parse
    | '\"'       { "" }
    | '\n' | eof { Util.raise_lexer_error lexbuf "Unclosed string literal." }
    | ""         { let c = character lexbuf in String.make 1 c ^ string_literal lexbuf }
and character = parse
    | "\\n"  { '\n' }
    | "\\t"  { '\t' }
    | "\\r"  { '\r' }
    | "\\\\" { '\\' }
    | "\\'"  { '\'' }
    | "\\\"" { '\"' }
    | _ as c { c    }
