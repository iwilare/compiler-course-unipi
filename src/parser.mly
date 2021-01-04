/*
* MicroC Parser specification
*/

%{
open Ast
%}

/* Tokens declarations */

%token EOF

%token TRUE "true" FALSE "false"

%token IF "if" ELSE "else" FOR "for" WHILE "while" RETURN "return" DO "do"

%token INT "int" CHAR "char" BOOL "bool" FLOAT "float" VOID "void"

%token NULL "NULL"

%token LPAREN "(" RPAREN ")"

%token LCURLY "{" RCURLY "}"

%token LSQUARE "[" RSQUARE "]"

%token SEMICOLON ";"

%token ASSIGNMENT "="

%token COMMA ","

%token ADDRESS_OF "&"

%token NOT "!"

%token OR "||" AND "&&"

%token EQ "==" NEQ "!=" GT ">" LT "<" GE ">=" LE "<="

%token INCREMENT "++" DECREMENT "--"

%token ASSIGNMENT_PLUS "+=" ASSIGNMENT_MINUS "-="
%token ASSIGNMENT_TIMES "*=" ASSIGNMENT_DIVIDE "/="
%token ASSIGNMENT_MOD "%="

%token PLUS "+" MINUS "-" TIMES "*" DIVIDE "/" MOD "%"

/* Tokens with lexical content */

%token <string> ID
%token <int>    INTEGER
%token <char>   CHARACTER
%token <float>  FLOATING
%token <string> STRING

/* Precedence and associativity specification */

%right    "=" "+=" "-=" "*=" "/=" "%="
%left     "||"
%left     "&&"
%left     "==" "!="
%nonassoc ">" "<" ">=" "<="
%left     "+" "-"
%left     "*" "/" "%"
%right    "!" "&"
%left     "++" "--"
%nonassoc "["

/* Resolve the dangling else problem by giving
   the branch without else a higher-priority than
   the one with the else. */

%nonassoc without_else
%nonassoc ELSE

/* Starting symbol */

%start <position program> program /* the parser returns a Ast.program value */

%%

/* A helper Menhir function to directly annotate expressions with their position */
%inline annotate(X):
   | x=X { x |> annotate_pos $loc }

/* Other useful Menhir functions to simplify parsing */
%inline parens(X):
   | x=delimited("(", X, ")") { x }
%inline parentify(X):
   | x=delimited("(", X, ")") { x }
   | x=X                      { x }

/* Grammar specification */

/* Menhir expression that have been split into underscore-less and
   underscored parts are simply to introduce the position annotation
   in an easy way, using the "annotate" helper function. */

program:
  | ds=list(topdecl) EOF { Prog(ds) }

topdecl: x=annotate(topdecl_) { x }
topdecl_:
  | f=fundecl     { Fundecl(f) }
  | v=vardecl ";" { Vardecl(fst v, snd v) }

/* Simply apply the function provided to vardesc to the inner type. */
vardecl:
  | t=typ v=vardesc { v t }

/* We discovered here a somewhat elegant approach to defer as much as possible
   the definition of the innermost concrete type, which is provided by
   vardecl and not by vardesc. For this reason, vardesc returns
   a function which only successively expects the innermost type.
   The types here simply accumulate on the inner type, thus creating
   a sort of left-recursion approach that, in terms of precedence and structure,
   actually mirrors the C definition of arrays and pointers. */
vardesc:
  | "(" v=vardesc ")"                   { v                       }
  | id=ID                               { fun t -> (t, id)        }
  | "*" v=vardesc                       { fun t -> v (TypP t)     }
  | v=vardesc "[" i=option(INTEGER) "]" { fun t -> v (TypA(t, i)) }

typ:
  | "int"   { TypI }
  | "bool"  { TypB }
  | "char"  { TypC }
  | "float" { TypF }
  | "void"  { TypV }

fundecl:
  | t=typ i=ID "(" p=separated_list(",", vardecl) ")" b=block
    { {typ=t; fname=i; formals=p; body=b } }

block: x=annotate(block_) { x }
block_:
  | "{" s=list(stmtordec) "}" { Block(s) }

stmtordec: x=annotate(stmtordec_) { x }
stmtordec_:
  | s=stmt        { Stmt(s)           }
  | v=vardecl ";" { Dec(fst v, snd v) }

stmt:
  | x=annotate(stmt_) { x }
  | b=block           { b }
stmt_:
  | "return" e=option(expr) ";"
    { Return(e) }
  | e=expr ";"
    { Expr(e) }
  | "while" "(" e=expr ")" b=stmt
    { While(e, b) }
  | "do" b=stmt "while" "(" e=expr ")"
    /* DoWhile is considered as a first-class AST node and it is NOT desugared. */
    { DoWhile(b, e) }
  | "for" "(" x=option(expr) ";" y=option(expr) ";" z=option(expr) ")" b=stmt
    /* Desugar the for statement, while also keeping the correct
         position annotation of the various sub-expressions.
         The desugaring is intuitively described as follows:

         for ( x ; y ; z )
            b
         ------------------------------------------------->
         {
           x ;         // if not provided: empty statement
           while ( y ) // if not provided: "true"
               {
                 b ;
                 y ;   // if not provided: empty statement
               }
         }
      */
    { let expr_to_stmt  e = annotate_pos e.ann @@ Expr(e) in
      let stmt_in_block e = annotate_pos e.ann @@ Stmt(e) in
      let expr_in_block e = stmt_in_block (expr_to_stmt e) in
      let init = Option.to_list @@ Option.map expr_in_block x in
      let default_condition = annotate_pos $loc @@ BLiteral(true) in
      let cond = Option.value y ~default:default_condition in
      let incr = Option.to_list @@ Option.map expr_in_block z in
        Block(init
            @ [stmt_in_block       @@
               annotate_pos $loc  @@ While(cond,
               annotate_pos b.ann @@ Block(stmt_in_block b :: incr))]) }
  | "if" "(" b=expr ")" e1=stmt "else" e2=stmt
    { If(b, e1, e2) }
  | "if" "(" b=expr ")" e=stmt %prec without_else
    /* Apply the if-without-else precedence and use an empty block as
       missing else statement. */
    { If(b, e, annotate_pos dummy_pos @@ Block([])) }

%inline OP:
  | "+"  { Add   }
  | "-"  { Sub   }
  | "*"  { Mul   }
  | "/"  { Div   }
  | "%"  { Mod   }
  | "==" { Eq    }
  | "!=" { Neq   }
  | "<"  { Lt    }
  | ">"  { Gt    }
  | "<=" { Le    }
  | ">=" { Ge    }
  | "&&" { And   }
  | "||" { Or    }

%inline UOP:
  | "!"  { Not }
  | "-"  { Neg }

%inline ASSIGNMENT_OP:
  | "+=" { Add }
  | "-=" { Sub }
  | "*=" { Mul }
  | "/=" { Div }
  | "%=" { Mod }

expr:
  | r=rexpr            { r }
  | l=lexpr            { Access(l) |> annotate_pos $loc }

rexpr:
  | x=aexpr            { x }
  | x=annotate(rexpr_) { x }
rexpr_:
  | f=ID "(" a=separated_list(",", expr) ")" { Call(f, a) }
  | l=lexpr "=" r=expr
    { Assign(l, r) }
  | l=lexpr op=ASSIGNMENT_OP r=expr
    /* AssignmentOps are considered as a first-class AST node and are NOT desugared. */
    { AssignOp(l, op, r) }
  | uop=UOP e=expr        { UnaryOp(uop, e)      }
  | e1=expr op=OP e2=expr { BinaryOp(op, e1, e2) }

aexpr:
  | x=parens(rexpr)    { x }
  | x=annotate(aexpr_) { x }
aexpr_:
  | i=INTEGER   { ILiteral(i)     }
  | c=CHARACTER { CLiteral(c)     }
  | f=FLOATING  { FLiteral(f)     }
  | s=STRING    { SLiteral(s)     }
  | "true"      { BLiteral(true)  }
  | "false"     { BLiteral(false) }
  | "NULL"      { Null            }
  | "&" a=lexpr { Addr(a)         }

lexpr: x=annotate(parentify(lexpr_)) { x }
lexpr_:
  /* Increments are considered as first-class AST nodes and are NOT desugared. */
  | "++" a=lexpr           { AccIncr(a, Pre,  Incr)                     }
  | "--" a=lexpr           { AccIncr(a, Pre,  Decr)                     }
  | a=lexpr "++"           { AccIncr(a, Post, Incr)                     }
  | a=lexpr "--"           { AccIncr(a, Post, Decr)                     }
  | i=ID                   { AccVar(i)                                  }
  | "*" a=aexpr            { AccDeref(a)                                }
  | "*" a=lexpr            { AccDeref(Access(a) |> annotate_pos a.ann)  }
  | v=lexpr "[" i=expr "]" { AccIndex(v, i)                             }
