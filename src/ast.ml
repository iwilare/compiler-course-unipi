(* Generic AST definition *)

(* Define an annotated element as a node type 'n and a generic annotation 'a *)
type ('n, 'a) annotated = { node : 'n; ann : 'a }

(* Helper function to create annotated nodes *)
let annotate a v = { ann = a; node = v }

(* Getter for annotations *)
let annotation e = e.ann

(* Getter for nodes *)
let node e = e.node

(* Helper function to preserve annotations *)
let lift_annotation f = fun e -> annotate e.ann (f e)

(* MicroC AST representation *)

type identifier = string

let show_identifier s : string = s

type binop =
  | Add | Sub | Mul | Div | Mod
  | Lt  | Le  | Gt  | Ge  | Eq  | Neq
  | And | Or  | Comma

type uop = Neg | Not

type typ =
  | TypI                                        (* Type int                    *)
  | TypB                                        (* Type bool                   *)
  | TypC                                        (* Type char                   *)
  | TypF                                        (* Type float                  *)
  | TypA of typ * int option                    (* Array type                  *)
  | TypP of typ                                 (* Pointer type                *)
  | TypV                                        (* Type void                   *)
  | TypS of identifier                          (* Struct type                 *)

type prepost = Pre | Post                       (* Pre or post             ++p or p++ *)

type incrdecr = Incr | Decr                     (* Increment or decrement  p++ or p-- *)

type 't expr = ('t expr_node, 't) annotated
and 't expr_node =
  | Null                                        (* NULL literal                                              *)
  | Access    of 't access                      (* x      or *p    or  a[e]                                  *)
  | Assign    of 't access * 't expr            (* x=e    or *p=e  or  a[e]=e                                *)
  | AssignOp  of 't access * binop * 't expr    (* x = x <op> e                                              *)
  | Addr      of 't access                      (* &x     or &*p   or  &a[e]                                 *)
  | ILiteral  of int                            (* Integer literal                                           *)
  | CLiteral  of char                           (* Char literal                                              *)
  | BLiteral  of bool                           (* Bool literal                                              *)
  | FLiteral  of float                          (* Float literal                                             *)
  | SLiteral  of string                         (* String literal                                            *)
  | UnaryOp   of uop * 't expr                  (* Unary primitive operator                                  *)
  | BinaryOp  of binop * 't expr * 't expr      (* Binary primitive operator                                 *)
  | Call      of identifier * ('t expr) list    (* Function call f(...)                                      *)
  | Increment of 't access * prepost * incrdecr (* Variable pre/post inc/decrement  ++p or p++ or --p or p-- *)

and 't access = ('t access_node, 't) annotated
and 't access_node =
  | AccVar    of identifier                     (* Variable access       x    *)
  | AccDeref  of 't expr                        (* Pointer dereferencing *p   *)
  | AccIndex  of 't access * 't expr            (* Array indexing        a[e] *)
  | AccStruct of 't access * identifier         (* Struct member access  a.v  *)

and 't stmt =  ('t stmt_node, 't) annotated
and 't stmt_node =
  | If      of 't expr * 't stmt * 't stmt     (* Conditional                 *)
  | While   of 't expr * 't stmt               (* While loop                  *)
  | DoWhile of 't stmt * 't expr               (* Do-while loop               *)
  | Expr    of 't expr                         (* Expression statement   e;   *)
  | Return  of 't expr option                  (* Return statement            *)
  | Block   of 't stmtordec list               (* Block: grouping and scope   *)

and 't stmtordec = ('t stmtordec_node, 't) annotated
and 't stmtordec_node =
  | Dec  of typ * identifier * 't expr option  (* (Initialized) local variable declaration *)
  | Stmt of 't stmt                            (* A statement                              *)

type 't fundecl = {
  typ     : typ;
  fname   : string;
  formals : (typ * identifier) list;
  body    : 't stmt;
}

type 't topdecl = ('t topdecl_node, 't) annotated
and 't topdecl_node =
  | Fundecl    of 't fundecl                           (* Function declaration                      *)
  | Vardecl    of typ * identifier * 't expr option    (* (Initialized) global variable declaration *)
  | Structdecl of identifier * (typ * identifier) list (* Struct declaration                        *)

type 't program = Prog of 't topdecl list

(* Pretty-printing functions *)

let show_binop = function
  | Add   -> "+"
  | Sub   -> "-"
  | Mul   -> "*"
  | Div   -> "/"
  | Mod   -> "%"
  | Lt    -> "<"
  | Le    -> "<="
  | Gt    -> ">"
  | Ge    -> ">="
  | Eq    -> "=="
  | Neq   -> "!="
  | And   -> "&&"
  | Or    -> "||"
  | Comma -> ","

let show_uop = function
  | Neg -> "-"
  | Not -> "!"

let rec show_typ = function
  | TypI       -> "int"
  | TypB       -> "bool"
  | TypC       -> "char"
  | TypF       -> "float"
  | TypA(t, s) -> show_typ t ^ "[" ^ (Option.fold ~none:"" ~some:string_of_int s) ^ "]"
  | TypP(t)    -> "*" ^ show_typ t
  | TypV       -> "void"
  | TypS(id)   -> "struct " ^ show_identifier id

let show_typ_list list = "(" ^ String.concat ", " (List.map show_typ list) ^ ")"

let show_prepost p i_or_d e  = match p with
  | Pre  -> "(" ^ i_or_d ^ e ^ ")"
  | Post -> "(" ^ e ^ i_or_d ^ ")"

let show_incrdec i = match i with
  | Incr -> "++"
  | Decr -> "--"

let indent n = String.make (4 * n) ' '

let show_vardecl ?indent:(i=0) (t, id) = indent i ^ show_typ t ^ " " ^ show_identifier id

let rec show_expr e = match e.node with
  | Null                  -> "NULL"
  | Access(a)             -> show_access a
  | Assign(a, e)          -> "(" ^ show_access a ^ " = " ^ show_expr e ^ ")"
  | AssignOp(a, binop, e) -> "(" ^ show_access a ^ " " ^ show_binop binop ^ "= " ^ show_expr e ^ ")"
  | Addr(a)               -> "(" ^ "&" ^ show_access a ^ ")"
  | ILiteral(i)           -> string_of_int i
  | CLiteral(c)           -> Char.escaped c
  | BLiteral(b)           -> string_of_bool b
  | FLiteral(f)           -> string_of_float f
  | SLiteral(s)           -> "\"" ^ String.escaped s ^ "\""
  | UnaryOp(uop, e)       -> "(" ^ show_uop uop ^ " " ^ show_expr e ^ ")"
  | BinaryOp(binop, a, b) -> "(" ^ show_expr a ^ " " ^ show_binop binop ^ " " ^ show_expr b ^ ")"
  | Call(id, args)        -> show_identifier id ^ "(" ^ String.concat ", " (List.map show_expr args) ^ ")"
  | Increment(a, p, i)    -> show_prepost p (show_incrdec i) (show_access a)
and show_access a = match a.node with
  | AccVar(id)            -> show_identifier id
  | AccDeref(e)           -> "(" ^ "*" ^ show_expr e ^ ")"
  | AccIndex(a, i)        -> "(" ^ show_access a ^ "[" ^ show_expr i ^ "]" ^ ")"
  | AccStruct(a, m)       -> "(" ^ show_access a ^ "." ^ show_identifier m ^ ")"

let rec show_stmt ?indent:(i=0) s = match s.node with
  | If(e, s1, s2)   -> indent i ^ "if" ^ "(" ^ show_expr e ^ ")" ^ "\n"
                     ^ show_stmt ~indent:(i+1) s1
                     ^ indent i ^ "else" ^ "\n"
                     ^ show_stmt ~indent:(i+1) s2
  | While(e, b)     -> indent i ^ "while" ^ "(" ^ show_expr e ^ ")" ^ "\n"
                     ^ show_stmt ~indent:(i+1) b
  | DoWhile(b, e)   -> indent i ^ "do" ^ "\n"
                     ^ show_stmt ~indent:(i+1) b
                     ^ indent i ^ "while" ^ "(" ^ show_expr e ^ ")" ^ "\n"
  | Expr(e)         -> indent i ^ show_expr e ^ ";" ^ "\n"
  | Return(None)    -> indent i ^ "return" ^ ";" ^ "\n"
  | Return(Some(e)) -> indent i ^ "return" ^ " " ^ show_expr e ^ ";" ^ "\n"
  | Block(b)        -> indent i ^ "{" ^ "\n"
                     ^ String.concat "" (List.map (show_stmtordec ~indent:(i+1)) b)
                     ^ indent i ^ "}" ^ "\n"
and show_stmtordec ?indent:(i=0) s = match s.node with
  | Dec(t, id, None)    -> show_vardecl ~indent:i (t, id) ^ ";" ^ "\n"
  | Dec(t, id, Some(e)) -> show_vardecl ~indent:i (t, id) ^ "=" ^ show_expr(e) ^ ";" ^ "\n"
  | Stmt(s)             -> show_stmt ~indent:i s

let show_fundecl f =
  show_typ f.typ ^ " " ^ f.fname ^ "(" ^ String.concat ", " (List.map show_vardecl f.formals) ^ ")\n" ^
  show_stmt f.body

let show_struct n s =
  "struct " ^ show_identifier n ^ " {\n" ^
    (s |> List.map (show_vardecl ~indent:1)
       |> List.map (fun s -> s ^ ";\n")
       |> String.concat "")
  ^ "}" ^ "\n"

let show_topdecl t = match t.node with
  | Fundecl(f)             -> show_fundecl f
  | Vardecl(t, i, None)    -> show_vardecl (t, i) ^ ";" ^ "\n"
  | Vardecl(t, i, Some(v)) -> show_vardecl (t, i) ^ "=" ^ show_expr v ^ ";" ^ "\n"
  | Structdecl(n, s) -> show_struct n s

let show_program (Prog(topdecls)) = String.concat "\n" @@ List.map show_topdecl topdecls
