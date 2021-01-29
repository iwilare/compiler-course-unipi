{
(* Importing Parser is required because the tokens need to
   be declared there as a common internal dependency. *)
open Parser

(* Maintain a list of the keywords along with their corresponding token
   inside a hash table, in order to simplify the scanner logic. *)

let keyword_table =
  Util.create_hashtable [
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
    ("struct", STRUCT);
  ]
}

(* Identifiers and numbers *)

let letter     = ['a'-'z' 'A'-'Z']
let digit      = ['0'-'9']
let identifier = (letter | '_') (letter | digit | '_')*

(* Numerical definitions for int and float *)

let integer    = digit digit*
let mantissa   = '.' digit*
let exponent   = ['e' 'E'] ['-' '+']? digit+
let float      = digit* mantissa? exponent?

(* Whitespace and newline definitions *)

let whitespace = [' ' '\t']+
let newline    = '\n' | '\n' '\r' | '\r' '\n' | '\r'

(* Main token definition *)

rule token = parse

    (* Whitespace and comment start *)

    | newline    { Lexing.new_line lexbuf; token lexbuf }
    | whitespace { token lexbuf                         }
    | "//"       { line_comment lexbuf                  }
    | "/*"       { block_comment 0 lexbuf               }

    (* Parentheses *)

    | "("        { LPAREN  }
    | ")"        { RPAREN  }
    | "["        { LSQUARE }
    | "]"        { RSQUARE }
    | "{"        { LCURLY  }
    | "}"        { RCURLY  }

    (* Syntactic elements *)

    | ","        { COMMA        }
    | ";"        { SEMICOLON    }
    | "="        { ASSIGNMENT   }

    (* Unary operators *)

    | "!"        { NOT          }
    | "&"        { ADDRESS_OF   }

    (* Binary operators *)

    | "+"        { PLUS         }
    | "-"        { MINUS        }
    | "*"        { TIMES        } (* Or pointer dereference*)
    | "/"        { DIVIDE       }
    | "%"        { MOD          }
    | "||"       { OR           }
    | "&&"       { AND          }
    | "=="       { EQ           }
    | "!="       { NEQ          }
    | ">"        { GT           }
    | "<"        { LT           }
    | ">="       { GE           }
    | "<="       { LE           }

    (* Pre/post increments and decrements *)

    | "++"       { INCREMENT    }
    | "--"       { DECREMENT    }

    (* Abbrevation for assignment operators *)

    | "+="       { ASSIGNMENT_PLUS   }
    | "-="       { ASSIGNMENT_MINUS  }
    | "*="       { ASSIGNMENT_TIMES  }
    | "/="       { ASSIGNMENT_DIVIDE }
    | "%="       { ASSIGNMENT_MOD    }

    (* Struct member access *)

    | "."        { DOT   }

    (* Atomic information-carrying token elements *)

    | identifier as id { try Hashtbl.find keyword_table id with Not_found -> ID(id) }
    | "'"              { CHARACTER(character_literal lexbuf) }
    | "\""             { STRING(string_literal lexbuf) }
    | integer as iv    { INTEGER(int_of_string iv)     }
    | float   as fv    { FLOATING(float_of_string fv)  }

    (* End of File and invalid character error-catching *)

    | eof              { EOF                           }
    | _ as c           { Util.raise_lexer_error lexbuf ("Illegal character \'" ^ Char.escaped c ^ "\'.") }

(* Line and block comments skipping procedures *)

and line_comment = parse
    | newline { Lexing.new_line lexbuf; token lexbuf }
    | _       { line_comment lexbuf }
and block_comment level = parse
    | "*/"    { if level = 0 then token lexbuf else block_comment (level-1) lexbuf }
    | "/*"    { block_comment (level+1) lexbuf                                     }
    | newline { Lexing.new_line lexbuf; block_comment level lexbuf                 }
    | _       { block_comment level lexbuf                                         }
    | eof     { Util.raise_lexer_error lexbuf "Unclosed comment, found EOF."       }

(* String literals tokenizer *)

and string_literal = parse
    | '\"'       { "" }
    | '\n' | eof { Util.raise_lexer_error lexbuf "Unclosed string literal." }
    | ""         { let c = character lexbuf in String.make 1 c ^ string_literal lexbuf }

(* Character literals tokenizer *)

and character_literal = parse
    | '\n' | eof { Util.raise_lexer_error lexbuf "Unclosed character literal." }
    | ""         { let c = character lexbuf in (character_end lexbuf; c) }
and character_end = parse
    | '\''       { }
    | _ as c     { Util.raise_lexer_error lexbuf ("Invalid character \'" ^ Char.escaped c ^ "\' in character literal, expected end.") }

(* Helper procedure scanning a single character literal, either standalone or in a string *)

and character = parse
    | "\\n"  { '\n'        }
    | "\\t"  { '\t'        }
    | "\\r"  { '\r'        }
    | "\\\\" { '\\'        }
    | "\\'"  { '\''        }
    | "\\\"" { '\"'        }
    | "\\0"  { Char.chr(0) }
    | _ as c { c           }
