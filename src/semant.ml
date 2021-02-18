open Ast
open Position

(* The type and name of the main entry point *)
let main_name = "main"

(* Information for functions contained in the symbol table.
   Each function can be either builtin or declared with a fundecl. *)
type fun_info =
  | Builtin of typ * typ list
  | Fundef of position fundecl

(* Information for variables contained in the symbol table.
   Each variable is identified with its type and where it
   has been declared. *)
type var_info = position * typ

(* Information for structs contained in the symbol table.
   Each struct is a namespace of field variables. *)
type struct_info = var_info Symbol_table.t

(* The main symbol table carried around during semantic checking,
   contains the namespaces for functions, variables and struct types. *)
type sym = { fun_sym    : fun_info    Symbol_table.t
           ; var_sym    : var_info    Symbol_table.t
           ; struct_sym : struct_info Symbol_table.t
           }

(* The type of the semantic annotation output during semantic checking.
   At the end of semantic checking, we will return a fully explicitly typed AST
   that serves as "evidence"/proof that semantic checking has been performed. *)
type semantic = typ

(**
  Determine whether a type has a completely defined size, known at
  compilation time. In C only complete types can be elements of arrays and structs.
  @param t the type to check
  @returns true iff the type is complete *)
let rec is_complete_type t = match t with
  | TypA(t, Some(_)) -> is_complete_type t (* Recursively check that the array element is complete *)
  | TypA(t, None)    -> false              (* Unbounded arrays have unknown size *)
  | _                -> true               (* Else, basic types and pointers have fixed size *)

(**
  Check that a type is valid:
    - Array size must be greater than 1
    - Array element types must have size known at runtime
  @param loc the location to report the error to if an error occurs
  @param t the type to check
  @throws Semantic_error if an error occurs
*)
let check_type struct_sym loc t = match t with
  | TypA(t, Some(i)) when i < 1 ->
    Util.raise_semantic_error loc @@ "Array size must be greater-than-zero, found " ^ string_of_int(i) ^ "."
  | TypA(t, _) when not (is_complete_type t) ->
    Util.raise_semantic_error loc @@ "Invalid incomplete type " ^ show_typ(t) ^ " as array element."
  | TypS(s) ->
    (match Symbol_table.lookup s struct_sym with
      | Some(t) -> ()
      | None -> Util.raise_semantic_error loc @@ "Undefined struct " ^ show_identifier(s) ^ ".")
  | _ -> ()

(**
  Check that the type returned by a function is valid.
  For now we only allow statically allocated elements whose lifetime is
  known/can be copied around freely without worry.
  @param loc the location to report the error to if an error occurs
  @param t the type to check
  @throws Semantic_error if an error occurs
 *)
let check_function_return_type struct_sym loc t = match t with
  | TypP(_) -> Util.raise_semantic_error loc "Invalid function return type, cannot be pointer."
  | TypA(_) -> Util.raise_semantic_error loc "Invalid function return type, cannot be array."
  | _       -> check_type struct_sym loc t

(**
  Check that the type used in a variable is valid.
  Void and NULL variables are not allowed.
  @param loc the location to report the error to if an error occurs
  @param t the type to check
  @throws Semantic_error if an error occurs
 *)
let check_var_type struct_sym loc t = match t with
  | TypV       -> Util.raise_semantic_error loc "Variable cannot be defined with type void."
  | TypP(TypV) -> Util.raise_semantic_error loc "Invalid reserved NULL type."
  | _          -> check_type struct_sym loc t

(**
  Check that the type argument given a function is valid.
  Furthermore, return the type that the function actually
  requires, called "implementation type": in the case of arrays,
  this corresponds to converting them to a unbounded array, thus
  discarding the first size. This is necessary in order to preserve
  the pass-by-reference semantics of arrays and not return copies of arrays.
  Under the hood, this is also what Clang does when dealing with sized
  arrays in function arguments.
  @param loc the location to report the error to if an error occurs
  @param t the type to check
  @return the implementation type for the function argument
  @throws Semantic_error if an error occurs
 *)
let check_function_argument_type struct_sym loc t =
  check_type struct_sym loc t;
  match t with
  (* Functions that accept sized arrays effectively receive
     just a pointer to the array, ignoring the size. Furthermore,
     it is interesting to note that in the C language no type-matching
     is actually required for the size of arrays when passing them
     to functions; as a concrete example, check test-string11.mc *)
  | TypA(t, Some(_)) -> TypA(t, None)
  | TypP(TypV) -> Util.raise_semantic_error loc "Invalid reserved NULL type."
  | t                -> t

(**
  Helper function incapsulating the implementation type of strings.
  Strings are implemented as a sized array of characters that includes
  the string length along with the zero-terminator.
  @param the string for which the type must be given
  @return the corresponding implementation typ
 *)
let string_type s = (TypA(TypC, Some(String.length s + 1)))

(**
  Unify a provided type with an expected one.
  This function is essential to manage and
  introduce casting and type polymorphic logic.
  @param expected the expected type
  @param provided the type that has been provided to match with the former
  @returns None if unification failed, Some(t) with the most specific type t
          (for now, since the type system is essentially non-polymorphic,
           it is always the expected one)
 *)
let rec unify_type expected provided =
  match (expected, provided) with
  (* Unify NULL with any pointer type and return the most specific. *)
  | (TypP t, TypP TypV) -> Some(TypP t)
  | (TypP TypV, TypP t) -> Some(TypP t)
  (* If an array with a certain size is required, only that size can be provided. *)
  | (TypA(t, Some(s)), TypA(t', Some(s'))) when s = s' -> Option.map (fun t -> TypA(t, Some(s))) (unify_type t t')
  (* However, if the required size is not specified, an array of any size can be provided. *)
  | (TypA(t, None), TypA(t', _)) -> Option.map (fun t -> TypA(t, None)) (unify_type t t')
  (* Check recursive unification for the pointer type. *)
  | (TypP a, TypP b) -> Option.map (fun t -> TypP(t)) (unify_type a b)
  (* Else, check for structural type equality. *)
  | (a, b) when a = b -> Some(a)
  (* Unification failed. *)
  | _ -> None

(**
  Unify a provided AST expression with the expected type,
  while setting the type annotation of the provided expression to
  the unification result. This is especially useful with NULL expressions,
  so that we can statically know their type later in the codegen phase.
  @param expected the expected type
  @param provided the type that has been provided to match with the former
  @returns None if unification failed, Some(t) with the re-annotated AST expression
*)
let unify expected provided =
  Option.map (fun t -> {provided with ann = t}) @@
    unify_type expected provided.ann

(**
  Helper function that unifies two types together, setting each
  other's annotations reciprocally. This can be useful when dealing
  with pointer expressions, e.g.: NULL == a && a == NULL.
  @param a the first  expression to unify
  @param a the second expression to unify
  @returns None if unification failed, Some(a, b) with two re-annotated AST expression
*)
let unify2 a b =
  match (unify_type a.ann b.ann, unify_type b.ann a.ann) with
    | (Some(uab), Some(uba)) -> Some({a with ann = uba}, {b with ann = uab})
    | (_, _) -> None

(**
  Function that incapsulates the checking logic for unary operators.
  @param loc the location to report the error to if an error occurs
  @param uop the unary operator
  @param a the type of the expression given to the operator
  @returns the resulting type of the operator
  @throws Semantic_error if an error occurs
*)
let check_unary_type loc uop a =
  match (uop, a) with
  | (Neg, (TypI | TypF)) -> a
  | (Not, TypB)          -> TypB
  | _ -> Util.raise_semantic_error loc @@ "Invalid type " ^ show_typ(a) ^ " with unary operator " ^ show_uop(uop) ^ "."

(**
  Function that incapsulates the checking logic for binary operators.
  This function returns a unified version of its parameters, which can for
  example happen in the case of pointer equality.
  @param loc the location to report the error to if an error occurs
  @param uop the binary operator
  @param a the left expression given to the operator
  @param b the right expression given to the operator
  @returns the resulting type of the operator, with the two possibly unified operands
  @throws Semantic_error if an error occurs
*)
let check_binary_type loc op a b =
  match (op, a.ann, b.ann) with
  | ((Add | Sub | Mul | Div | Mod),  TypI, TypI) -> (a, b, TypI)
  | ((Add | Sub | Mul | Div),        TypF, TypF) -> (a, b, TypF)
  | ((Eq | Neq | Lt | Le | Gt | Ge), TypI, TypI) -> (a, b, TypB)
  | ((Eq | Neq | Lt | Le | Gt | Ge), TypF, TypF) -> (a, b, TypB)
  (* Pointer equality can be performed if the two expressions can be unified *)
  | ((Neq | Eq), _, _) ->
    (match unify2 a b with
      | Some(au, ub) -> (au, ub, TypB)
      | None -> Util.raise_semantic_error loc @@ "Invalid pointer equality " ^ show_typ(a.ann) ^ " and " ^ show_typ(b.ann) ^ " with binary operator " ^ show_binop(op) ^ ".")
  | ((And | Or),                     TypB, TypB) -> (a, b, TypB)
  | (Comma,                          _,    _)    -> (a, b, b.ann)
  | _ -> Util.raise_semantic_error loc @@ "Invalid types " ^ show_typ(a.ann) ^ " and " ^ show_typ(b.ann) ^ " with binary operator " ^ show_binop(op) ^ "."

(**
  Semantically check an access expression.
  @param sym the current symbol table
  @param a the access expression
  @returns the semantically annotated AST for this expression
  @throws Semantic_error if an error occurs
*)
let rec check_access sym a =
  match a.node with
  | AccVar(id) ->
    (match Symbol_table.lookup id sym.var_sym with
     | None -> Util.raise_semantic_error a.ann @@ "Undefined variable \"" ^ id ^ "\"."
     | Some(_, typ) -> annotate typ @@ AccVar(id)) (* Annotate the variable with its looked-up type *)
  | AccDeref(e) ->
    let et = check_expr sym e in
    (match et.ann with
     | TypP(TypV) -> Util.raise_semantic_error a.ann @@ "Cannot directly dereference NULL type."
     | TypP(t)    -> annotate t @@ AccDeref(et)
     | t          -> Util.raise_semantic_error a.ann @@ "Cannot dereference non-pointer type exprssion with type " ^ show_typ(t) ^ ".")
  | AccIndex(a, e) ->
    let et = check_expr sym e in
    (match et.ann with
     | TypI ->
       let at = check_access sym a in
       (match at.ann with
        | TypA(t, _) -> annotate t @@ AccIndex(at,et)
        | t -> Util.raise_semantic_error a.ann @@ "Invalid indexing with non-array type " ^ show_typ(t) ^ ".")
     | t -> Util.raise_semantic_error a.ann @@ "Invalid indexing with non-integer index " ^ show_typ(t) ^ ".")
  | AccStruct(a, m) ->
    let at = check_access sym a in
    (match at.ann with
     | TypS(sname) ->
       let at = check_access sym a in
       (match Symbol_table.lookup sname sym.struct_sym with
        | Some(fields) ->
          (match Symbol_table.lookup m fields with
            | Some(_, typ) -> annotate typ @@ AccStruct(at, m)
            | None -> Util.raise_semantic_error a.ann @@ "Undefined field \"" ^ m ^ "\" in expression with type \"" ^ show_typ(at.ann) ^ "\".")
        | None -> Util.raise_semantic_error a.ann @@ "Undefined struct type \"" ^ show_identifier(sname) ^ "\".")
     | t -> Util.raise_semantic_error a.ann @@ "Invalid member access with non-struct type " ^ show_typ(t) ^ ".")

(**
  Semantically check an lvalue access expression.
  The only non-trivial check here performed is the fact that standard arrays
  cannot be reassigned. String literals are not an exception to this rule,
  and for simplicity they can only be assigned at the global top-level.
  @param sym the current symbol table
  @param a the access expression
  @returns the semantically annotated AST for this expression
  @throws Semantic_error if an error occurs
*)
and check_lvalue sym a =
  let at = check_access sym a in
  match at.ann with
  | TypA(_, _)       -> Util.raise_semantic_error a.ann "Cannot reassign array value."
  | _                -> at

(**
  Semantically check an expr AST expression.
  @param sym the current symbol table
  @param a the expr expression
  @returns the semantically annotated AST for this expression
  @throws Semantic_error if an error occurs
*)
and check_expr sym e =
  let loc = e.ann in
  match e.node with
  (* We use here the void* type as placeholder for the (forall a. *a) type.
     We assume that pointers to void are not allowed
     (since they cannot be casted and be made useful),
     but this placeholder can be easily replaced with a
     dummy type that specifically indicates the polymorphic NULL type.
     Note that we cannot use just use TypV, as it might be
     the return type of a function. This later simplifies
     the type checking and type compatibility. *)
  | Null               -> annotate (TypP(TypV)) Null
  | ILiteral(e)        -> annotate TypI         (ILiteral(e))
  | CLiteral(e)        -> annotate TypC         (CLiteral(e))
  | BLiteral(e)        -> annotate TypB         (BLiteral(e))
  | FLiteral(e)        -> annotate TypF         (FLiteral(e))
  | SLiteral(e)        -> (* Return the corresponding implementation type of strings for the given literal. *)
                          annotate (string_type e) (SLiteral(e))
  | Access(a)          -> (* Simply reapply the same annotation to the access expression. *)
                          lift_annotation (fun a -> Access(a)) @@ check_access sym a
  | Addr(a)            -> let at = check_access sym a in
                          annotate (TypP(at.ann)) (Addr(at))
  | Assign(l, r)       -> let tl = check_lvalue sym l in
                          let tr = check_expr sym r   in
                          (match unify tl.ann tr with
                            | Some(utr) -> annotate tl.ann (Assign(tl, utr))
                            | None -> Util.raise_semantic_error r.ann @@ "Trying to assign a value of type " ^ show_typ(tr.ann) ^ " to an lvalue with type " ^ show_typ(tl.ann) ^ ".")
  | AssignOp(l, op, r) -> (* Utility function to JUST simplify validity checking.
                              The expression is NOT desugared here. *)
                          let desugar_assign_op l op r =
                            annotate l.ann @@
                            Assign(l, annotate l.ann @@
                                      BinaryOp(op, annotate l.ann @@
                                                  Access(l), r)) in
                          let desugared = desugar_assign_op l op r in
                          let checked_desugared = check_expr sym desugared in
                          annotate checked_desugared.ann (AssignOp(check_access sym l, op, check_expr sym r))
  | UnaryOp(uop, e)    -> let et = check_expr sym e in
                          let return_type = check_unary_type loc uop et.ann in
                          annotate return_type (UnaryOp(uop, et))
  | BinaryOp(op, a, b) -> let at = check_expr sym a in
                          let bt = check_expr sym b in
                          let (atu, btu, return_type) = check_binary_type loc op at bt in
                          annotate return_type (BinaryOp(op, atu, btu))
  | Call(f, values)    -> let args = List.map (check_expr sym) values in
                          let (return_type, unified_args) = check_function_call loc sym f args in
                          annotate return_type (Call(f, unified_args))
  | Increment(a, p, i) ->
    let at = check_access sym a in
    (* For simplicity, we only allow post/pre increment and decrement operations
       on integer and float types, since no pointer arithmetic is yet implemented *)
    (match at.ann with
     | (TypI | TypF) as t -> annotate t @@ Increment(at, p, i)
     | t -> Util.raise_semantic_error a.ann @@ "Invalid post/pre-increment/decrement expression with type " ^ show_typ(t) ^ ".")

(**
  Helper function to semantically check arguments in a function call.
  @param loc the location to report the error to if an error occurs
  @param sym the current symbol table
  @param f the fundef record containing the function information
  @param args the list of argument expression used in the call
  @returns the return type of the call and the list of unified types for the arguments
  @throws Semantic_error if an error occurs
*)
and check_function_call loc sym f args =
  let args_types = List.map annotation args in
  (* Check and unify the arguments; this process is the same for both defined and builtin functions *)
  let check_function_args return_type formal_types =
    if List.length args == List.length formal_types then
      let unify_args param arg =
        match unify param arg with
        | Some(unified_arg) -> unified_arg
        | _ -> Util.raise_semantic_error loc @@ "Invalid arguments in function call. Expected types " ^ show_typ_list(formal_types) ^ " but got " ^ show_typ_list(args_types) ^ "." in
      (* Return the function type and the list of unified arguments *)
      (return_type, List.map2 unify_args formal_types args)
    else
      Util.raise_semantic_error loc @@ "Invalid number of arguments in function call. Expected types " ^ show_typ_list(formal_types) ^ " but got " ^ show_typ_list(args_types) ^ "."
  in
  match Symbol_table.lookup f sym.fun_sym with
  | None -> Util.raise_semantic_error loc @@ "Undefined function \"" ^ f ^ "\" in function call."
  | Some(Fundef(fundecl))       -> check_function_args fundecl.typ (List.map fst fundecl.formals)
  | Some(Builtin(typ, formals)) -> check_function_args typ formals

(**
  Semantically check a variable declaration, independently of it being used
  as function parameter, or as global variable, or as variable in a block.
  @param loc the location to report the error to if an error occurs
  @param sym the current symbol table
  @throws Semantic_error if an error occurs
*)
let check_vardecl loc sym (t, id) =
  check_var_type sym.struct_sym loc t;
  try  Symbol_table.add_entry id (loc, t) sym.var_sym |> ignore
  with Symbol_table.DuplicateEntry ->
    Util.raise_semantic_error loc "Variable already declared in the same scope."

(**
  Encapsulates the logic of allowed global compile-time values.
  For simplicity, the expression is simply checked to be a constant
  while allowing compile-time constant calculations.
  @param loc the location to report any errors to
  @param sym the current symbol table
  @param e the expression being checked
  @return the semantically-checked expression given
  @throws Semantic_error if an error occurs
*)
let rec check_global_value loc sym e = match e.node with
  | SLiteral(_) | ILiteral(_) | CLiteral(_) | BLiteral(_) | FLiteral(_)
  | Null               -> check_expr sym e
  | UnaryOp(uop, e)    -> let et = check_global_value loc sym e in
                          let return_type = check_unary_type loc uop et.ann in
                          annotate return_type (UnaryOp(uop, et))
  | BinaryOp(op, a, b) -> let at = check_global_value loc sym a in
                          let bt = check_global_value loc sym b in
                          let (atu, btu, return_type) = check_binary_type loc op at bt in
                          annotate return_type (BinaryOp(op, atu, btu))
  | _ -> Util.raise_semantic_error e.ann "Invalid non-constant initialization element."

(**
  Semantically check a global variable declaration. This also encapsulates
  the fact that the value of a global variable must be compile-time constant.
  @returns the optionally type checked initial value of the variable
  @param loc the location to report the error to if an error occurs
  @param sym the current symbol table
  @return the constant-time value that initializes the variable
  @throws Semantic_error if an error occurs
*)
let check_global_vardecl loc sym (t, id) init =
  check_vardecl loc sym (t, id);
  (* Check that the global value has a compile-time defined size *)
  (if not (is_complete_type t) then
      Util.raise_semantic_error loc @@ "Invalid global variable \"" ^ id ^ "\" with compile-time incomplete type " ^ show_typ t ^ ".");
  (* Check that the initialization has the correct value for the static variable *)
  let check_init_type tv =
    (match unify t tv with
    | Some(utv) -> utv
    | None -> Util.raise_semantic_error loc @@ "Trying to initialize a variable with type " ^ show_typ(t) ^ " with a value of type " ^ show_typ(tv.ann) ^ ".") in
  Option.map (fun e -> check_init_type (check_global_value loc sym e)) init

(**
  Function that incapsulates the logic of checking the type of a boolean condition,
  for a given expression. It is used both in while (and desugared fors), and if statements.
  This is separated as an independent function to embody the logic of
  checking for truthiness, which could also be implemented as checking and
  considering integer and pointer expressions as valid in an if/while/for statement.
  @param loc the location to report the error to if an error occurs
  @param t the given type
  @returns the unification of the type with the required one
  @throws Semantic_error if an error occurs
*)
let check_condition loc t =
  match unify TypB t with
    | Some(ut) -> ut
    | None -> Util.raise_semantic_error loc @@ "Expected type bool in condition, but got " ^ show_typ(t.ann) ^ "."

(**
  Semantically check a statement.
  As a placeholder, the semantically annotated AST is annotated with the type void.
  @param rt the return type of the function where this statement is
  @param sym the current symbol table
  @param stmt the statement expression
  @returns the semantically annotated AST for this statement (void)
  @throws Semantic_error if an error occurs
*)
let rec check_stmt rt sym stmt = annotate TypV @@
  (match stmt.node with
  | If(e, s1, s2) ->
    let et  = e |> check_expr sym |> check_condition e.ann in
    let s1t = check_stmt  rt sym s1 in
    let s2t = check_stmt  rt sym s2 in
    If(et, s1t, s2t)
  | While(e, b) ->
    let et = e |> check_expr sym |> check_condition e.ann in
    let st = check_stmt  rt sym b in
    While(et, st)
  | DoWhile(b, e) ->
    let et = e |> check_expr sym |> check_condition e.ann in
    let st = check_stmt  rt sym b in
    DoWhile(st, et)
  | Expr(e) -> Expr(check_expr sym e)
  | Return(None) ->
    (match rt with
      | TypV -> Return(None)
      | _ -> Util.raise_semantic_error stmt.ann @@ "Function returns " ^ show_typ(rt) ^ ", but got empty return.")
  | Return(Some(e)) ->
    let et = check_expr sym e in
    (match unify rt et with
      | Some(uet) -> Return(Some(et))
      | None      -> Util.raise_semantic_error e.ann @@ "Function returns " ^ show_typ(rt) ^ ", but returned value is of type " ^ show_typ(et.ann) ^ ".")
  | Block(e) ->
    (* Create a new symbol table by adding a block scope *)
    let block_sym = {sym with var_sym = Symbol_table.begin_block sym.var_sym} in
    Block(List.map (check_stmtordecl rt block_sym) e))

(**
  Semantically check a statement or a declaration, contained inside a block.
  As a placeholder, the semantically annotated AST is annotated with the type void.
  @param ret_found a ref variable; contains true iff a return statement has been found in the current function
  @param rt the return type of the function where this statement is
  @param sym the current symbol table
  @param stmt the statement/delcaration expression
  @returns the semantically annotated AST for this statement (void)
  @throws Semantic_error if an error occurs
*)
and check_stmtordecl rt sym b = annotate TypV @@
  (match b.node with
  (* There is no initializer; simply check the variable declaration. *)
  | Dec(t, id, None) -> check_vardecl b.ann sym (t, id);
                        Dec(t, id, None)
  | Dec(t, id, Some(e)) ->
    check_vardecl b.ann sym (t, id);
    let et = check_expr sym e in
    (* Ensure that the declared variable type corresponds with the value assigned.
       Note that here we do not check that the assignment is a valid "semantic"
       assignment in the same way that it is checked in check_expr and Assign.
       For example, here we can introduce string literals, whereas if we desugared it
       into an assignment, we would get "Cannot reassign to array" because strings
       are just arrays. *)
    (match unify t et with
      | Some(uet) -> Dec(t, id, Some(uet))
      | None -> Util.raise_semantic_error b.ann @@ "Trying to initialize a local variable with type " ^ show_typ(t) ^ " with a value of type " ^ show_typ(et.ann) ^ ".")
  | Stmt(s) -> Stmt(check_stmt rt sym s))

(**
  Semantically check a function declaration.
  This function acts as starting point for the main check_stmt (and then check_expr) functions.
  Furthermore, transform the function arguments into their implementation types.
  @param loc the location to report the error to if an error occurs
  @param sym the current symbol table
  @param f the fundecl being currently checked
  @returns the semantically annotated fundecl
  @throws Semantic_error if an error occurs
*)
let check_fundecl loc sym f : typ fundecl =
  (* Convert the function arguments into their corresponding implementation types *)
  let runtime_formals =
     f.formals |> List.map (fun (t, i) -> (check_function_argument_type sym.struct_sym loc t, i)) in
  (* Redefine f as the correctly converted runtime fundecl *)
  let f = {f with formals = runtime_formals} in
  (* Check the return type as valid *)
  check_function_return_type sym.struct_sym loc f.typ;
  (* Pre-emptively insert the (incomplete) function definition inside the
     namespace for functions, in order to allow for recursive definitions *)
  let self_fun_sym =
    try Symbol_table.add_entry f.fname (Fundef(f)) sym.fun_sym
    with Symbol_table.DuplicateEntry ->
      Util.raise_semantic_error loc @@ "Function " ^ f.fname ^ " already declared." in
  (* Initialize a new scoping block inside the function *)
  let function_sym = { fun_sym    = self_fun_sym
                     ; var_sym    = Symbol_table.begin_block sym.var_sym
                     ; struct_sym = sym.struct_sym
                     } in
  (* Insert all parameters as variable declarations in the new namespace *)
  List.iter (check_vardecl loc function_sym) f.formals;
  let function_body = check_stmt f.typ function_sym f.body in
  {f with body = function_body}

(**
  Semantically check a field variable declaration in a struct.
  Variables inside structs are special because their type needs to
  have a compile-time known fixed size.
  @param loc the location to report the error to if an error occurs
  @param struct_sym the current struct symbol table
  @throws Semantic_error if an error occurs
*)
let check_field loc sym (t, id) =
  check_var_type sym.struct_sym loc t;
  (if not (is_complete_type t) then
      Util.raise_semantic_error loc @@ "Invalid struct member \"" ^ id ^ "\" with compile-time incomplete type " ^ show_typ t ^ ".");
  try  Symbol_table.add_entry id (loc, t) sym.var_sym |> ignore
  with Symbol_table.DuplicateEntry ->
    Util.raise_semantic_error loc "Variable already declared in the struct."

(**
  Semantically check a struct declaration.
  @param sym the current symbol table
  @param name the declared name of the struct
  @param s the list of types and variables being declared in the struct
  @throws Semantic_error if an error occurs
*)
let check_structdecl loc sym (name, s) =
  (* Declare a new symbol-table struct scope to check for variable duplication;
  this struct-local will be inserted as information for the struct in the symbol table. *)
  let struct_sym = {sym with var_sym = Symbol_table.empty_table () } in
  (* Check that all fields of the struct are all size-defined types and are not duplicated.*)
  List.iter (check_field loc struct_sym) s;
  try
    (* Insert the currently created struct scope in the symbol-table *)
    Symbol_table.add_entry name struct_sym.var_sym struct_sym.struct_sym |> ignore
  with Symbol_table.DuplicateEntry ->
    Util.raise_semantic_error loc @@ "Struct " ^ name ^ " already declared."

(**
  Semantically check a top-level declaration.
  @param sym the current symbol table
  @param topdecl the top-level declaration being checked
  @returns the semantically annotated AST for this declaration
  @throws Semantic_error if an error occurs
*)
let check_topdecl sym topdecl : typ topdecl = annotate TypV @@
  (match topdecl.node with
  | Fundecl(f)          -> Fundecl(check_fundecl topdecl.ann sym f)
  | Vardecl(t, i, init) -> let cinit = check_global_vardecl topdecl.ann sym (t, i) init in
                           Vardecl(t, i, cinit)
  | Structdecl(n, s)    -> check_structdecl topdecl.ann sym (n, s);
                           Structdecl(n, s))

(**
  Incapsulate the logic of checking for whole-program properties
  for the entire code, after the main program has been locally checked.
  In this case, the main function is required and it requires the correct signature.
  @param sym the current symbol table
  @param p the semantically annotated AST program
  @returns the same semantically annotated AST of the program
  @throws Semantic_error if an error occurs
*)
let check_program sym p =
  match Symbol_table.lookup main_name sym.fun_sym with
  (* Check for the correct signatures of the main procedure *)
  | Some(Fundef({typ = TypI; fname = main_name; formals = []})) -> p
  | Some(Fundef({typ = TypV; fname = main_name; formals = []})) -> p
  | Some(Fundef(f)) -> Util.raise_semantic_error f.body.ann "Invalid signature for main."
  | _               -> Util.raise_semantic_error dummy_pos "Function main not defined."

(**
  Incapsulate the initialization of the base function environment, so that
  it can be easily changed in the future. For now, simply define the
  standard builtin functions.
  @returns the symbol table representing the function namespace
*)
let builtin_functions_sym =
  let b = Symbol_table.empty_table () in
  let decl_builtin (n, r, a) =
    Symbol_table.add_entry n (Builtin(r, a)) b |> ignore in
  List.iter decl_builtin Builtins.builtin_functions;
  b

(**
  Check the position-annotated AST of the given program.
  @returns the semantically annotated and checked AST of the program
  @throws Semantic_error if a checking error occours
*)
let check ((Prog(topdecls)) : position program) : typ program =
  (* Define the initial symbol-table environment *)
  let global_sym = { fun_sym    = builtin_functions_sym
                   ; var_sym    = Symbol_table.empty_table ()
                   ; struct_sym = Symbol_table.empty_table ()
                   } in
  (Prog(List.map (check_topdecl global_sym) topdecls))
    (* Now that the program has been locally check, check global properties *)
    |> check_program global_sym
