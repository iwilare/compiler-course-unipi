open Ast
open Semant

(* A shorthand for referring to the Llvm library *)
module L = Llvm

(* The LLVM global context *)
let llcontext = L.global_context ()

(* Some useful standard LLVM IR types to use in the code generation *)
let int_type   = L.i32_type   llcontext
let bool_type  = L.i1_type    llcontext
let char_type  = L.i8_type    llcontext
let float_type = L.float_type llcontext
let void_type  = L.void_type  llcontext
let null_type  = L.pointer_type (L.i8_type llcontext)

(* Some useful global constants for the basic types *)
let llvm_zero = L.const_int   int_type   0
let llvm_one  = L.const_int   int_type   1
let llvm_onef = L.const_float float_type 1.0

let llvm_false = L.const_int bool_type 0
let llvm_true  = L.const_int bool_type 1

(**
  Function incapsulating the logic of implementing binary operators,
  associating each operator and the types of its operands with the
  LLVM instruction that constructs and implements it.
  @param the binary operator, and a tuple indicating the types of the given values
  @throws Codegen_error if the given operator and type combination is invalid.
          This should theoretically never happen, as the types given
          should have been semantically checked.
*)
let binary_operator = function
  (* Integer binary operators *)
  (* Signed comparisons are used, as there is currently no implementation for unsigned integers. *)
  | (Add,   (TypI, TypI)) -> L.build_add
  | (Sub,   (TypI, TypI)) -> L.build_sub
  | (Mul,   (TypI, TypI)) -> L.build_mul
  | (Div,   (TypI, TypI)) -> L.build_sdiv
  | (Mod,   (TypI, TypI)) -> L.build_srem
  | (Lt,    (TypI, TypI)) -> L.build_icmp L.Icmp.Slt
  | (Le,    (TypI, TypI)) -> L.build_icmp L.Icmp.Sle
  | (Gt,    (TypI, TypI)) -> L.build_icmp L.Icmp.Sgt
  | (Ge,    (TypI, TypI)) -> L.build_icmp L.Icmp.Sge
  | (Eq,    (TypI, TypI)) -> L.build_icmp L.Icmp.Eq
  | (Neq,   (TypI, TypI)) -> L.build_icmp L.Icmp.Ne
  (* Floating binary operators *)
  (* From the documentation we have the following instructions:
     O-variant, ordered comparisons:   yields true if both operands are not a NaN and the condition is true.
     U-variant, unordered comparisons: yields true if either operand is a NaN or the condition is true.
     For simplicity, we choose here the O-variant in order to provide an expected "avalanche" effect with NaN. *)
  | (Add,   (TypF, TypF)) -> L.build_fadd
  | (Sub,   (TypF, TypF)) -> L.build_fsub
  | (Mul,   (TypF, TypF)) -> L.build_fmul
  | (Div,   (TypF, TypF)) -> L.build_fdiv
  | (Lt,    (TypF, TypF)) -> L.build_fcmp L.Fcmp.Olt
  | (Le,    (TypF, TypF)) -> L.build_fcmp L.Fcmp.Ole
  | (Gt,    (TypF, TypF)) -> L.build_fcmp L.Fcmp.Ogt
  | (Ge,    (TypF, TypF)) -> L.build_fcmp L.Fcmp.Oge
  | (Eq,    (TypF, TypF)) -> L.build_fcmp L.Fcmp.Oeq
  | (Neq,   (TypF, TypF)) -> L.build_fcmp L.Fcmp.One
  | (Comma, (_,    _))    -> (fun a b _ _ -> b)
  | _ -> Util.raise_codegen_error "Invalid types with binary operator"

(**
  Function incapsulating the logic of implementing unary operators,
  associating each operator and the type of its operand with the
  LLVM instruction that constructs and implements it.
  @param the unary operator, and the types of the operand given
  @throws Codegen_error if the given operator and type combination is invalid.
          This should theoretically never happen, as the type and operator given
          should have been semantically checked.
*)
let unary_operator = function
  | (Neg, TypI) -> L.build_neg
  | (Neg, TypF) -> L.build_fneg
  | (Not, TypB) -> L.build_not
  | _ -> Util.raise_codegen_error "Invalid types with unary operator."

(**
  Function incapsulating the logic of implementing increment and
  decrement, associating each case and the type of its operand with the
  LLVM instruction that implements it.
  This function can be extended in the future to support pointer arithmetic, or
  increment operations with other kind of supported types.
  @param a tuple indicating the increment or decrement type and the type of the argument
  @throws Codegen_error if the given operator and type combination is invalid.
          This should theoretically never happen, as the type and operator given
          should have been semantically checked.
*)
let increment_operator = function
  (* For technical reasons, we flip the operator so that the 1 is always
     on the right; this is irrelevant for the int type, but it is
     semantic-changing for the float type and floating pointer operations. *)
  | (Incr, TypI) -> Fun.flip L.build_add  llvm_one
  | (Decr, TypI) -> Fun.flip L.build_sub  llvm_one
  | (Incr, TypF) -> Fun.flip L.build_fadd llvm_onef
  | (Decr, TypF) -> Fun.flip L.build_fsub llvm_onef
  | _ -> Util.raise_codegen_error "Invalid types with increment/decrement operator."

(**
  Function incapsulating the logic of short-circuiting boolean operators,
  identifying the values for which the condition short-circuits and
  the test to apply to it.
  @param op the binary operator
  @throws Codegen_error if an invalid non-boolean binary operator is given.
          This should theoretically never happen, as the operator given
          should have been semantically checked.
*)
let boolean_operations_quit_conditions = function
  | And -> (llvm_false, L.Icmp.Eq)
  | Or  -> (llvm_true,  L.Icmp.Ne)
  | _   -> Util.raise_codegen_error "Invalid boolean operator."

(** Translate a MicroC type into a concrete LLVM implementation type *)
let rec lltype_of_typ = function
  | TypI             -> int_type
  | TypB             -> bool_type
  | TypC             -> char_type
  | TypF             -> float_type
  | TypA(t, Some(s)) -> L.array_type (lltype_of_typ t) s
  | TypA(t, None)      (* Unbounded arrays are for all purposes considered as pointers;
                          clang and other tools compiling C to LLVM also do this *)
  | TypP(t)          -> L.pointer_type (lltype_of_typ t)
  | TypV             -> void_type

(**
  Use the given builder to add a terminating instructor to a block if and
  only if another one does not exist already. Else, simply do nothing.
  This helper function is necessary in order not to insert additional terminating
  instructions to a block that already terminates (e.g.: using an early return)
  @param builder the builder used to insert the terminator given
  @param f the function to which the builder is passed to
*)
let add_terminator builder f =
  match L.block_terminator (L.insertion_block builder) with
    | Some(_) -> ()
    | None    -> f builder |> ignore

(**
  The main symbol table scope carried around during codegen.
  Contains the namespaces for functions and variables, and associates
  to each name either the function definition llvalue
  or the llvalue associated with the variable.
*)
type sym = { fun_sym : L.llvalue Symbol_table.t
           ; var_sym : L.llvalue Symbol_table.t
           }

(**
  Generate the LLVM code for a semantically annotated access AST node.
  @param block_maker a closure to create new blocks in the current function
  @param builder the builder used to generate the instructions; if the function
         requires it, it might be modified to point to another newly created block
  @param sym the variable symbol table for the current scope
  @param a the access node
  @returns an llvalue that represents the *address* being accessed by this expression
  @throws Codegen_error if an error occurs
*)
let rec codegen_access block_maker builder sym a =
  match a.node with
  | AccVar(id) ->
    (match Symbol_table.lookup id (sym.var_sym) with
     | Some(v) -> v
     | None    -> Util.raise_codegen_error @@ "Undefined variable \"" ^ id ^ "\".")
  | AccDeref(e) ->
    (* Simply return the value given by the expression, since that
       will be the address; if needed, it will be dereferenced later *)
    codegen_expr block_maker builder sym e
  | AccIncr(a, p, i) ->
    (* Simple function to pick the correct resulting llvalue
       according to the given kind of operation *)
    let pick_value before after = function
      | Pre  -> before
      | Post -> after in
    (* Obtain the correct kind of instruction, according both
       to the kind of operation and the operand type *)
    let do_increment = increment_operator (i, a.ann) in
    let var    = codegen_access block_maker builder sym a in
    let before = L.build_load var        "" builder in
    let after  = do_increment before     "" builder in
    let _      = L.build_store var after    builder in
    (* After the variable has been modified, pick the resulting value *)
    pick_value before after p
  | AccIndex(a, i) ->
    let var = codegen_access block_maker builder sym a in
    let index = codegen_expr block_maker builder sym i in
    (* Depending on the type of the reference, access it in different way: *)
    let indexing =
      (match a.ann with
       (* Fixed-size array need to be dereferenced with an initial
          zero-index to get through the pointer; *)
       | TypA(_, Some(_)) -> [| llvm_zero; index |]
       (* Arrays without fixed size on the other hand are
          implemented as equivalent to pointers; simply dereference *)
       | TypA(_, None)    -> [| index |]
       | _                -> Util.raise_codegen_error "Invalid type annotation in indexing.") in
    L.build_gep var indexing "" builder

(**
  Generate the LLVM code for a semantically annotated expr AST node.
  @param block_maker a closure to create new blocks in the current function
  @param builder the builder used to generate the instructions; if the function
         requires it, it might be modified to point to another newly created block
  @param sym the variable symbol table for the current scope
  @param e the expr node
  @returns an llvalue representing the value produced by the expression
  @throws Codegen_error if an error occurs
 *)
and codegen_expr block_maker builder sym e =
  match e.node with
  | ILiteral(i)  -> L.const_int int_type i
  | CLiteral(c)  -> L.const_int char_type (Char.code c)
  | BLiteral(b)  -> if b then llvm_true else llvm_false
  | FLiteral(f)  -> L.const_float float_type f
  | SLiteral(s)  -> L.const_string llcontext s
                    (* Use the annotation given by the semantic checking to
                       define the polymorphic NULL type with the appropriate type *)
  | Null         -> L.const_pointer_null (lltype_of_typ e.ann)
                    (* Access expression already returns a pointer here, simply return the llvalue *)
  | Addr(a)      -> codegen_access block_maker builder sym a
  | Access(a)    ->
    let var = codegen_access block_maker builder sym a in
    (match (e.ann, a.ann) with
     | (TypA(_, None), TypA(_, None))    -> var
     | (TypA(_, None), TypA(_, Some(_))) -> L.build_gep var [| llvm_zero; llvm_zero |] "" builder
     | _                                 -> L.build_load var "" builder )
  | Assign(a, e) ->
    let p = codegen_access block_maker builder sym a in
    let v = codegen_expr   block_maker builder sym e in
    (* All access operations return an address, so a final store is required *)
    L.build_store v p builder |> ignore;
    v
  | AssignOp(a, op, b) ->
    (* Codegen the access, so that pre/post-increment operations will
       only be executed once. This is the reason why this expression is not
       desugared and instead treated here as an independent case. *)
    let p = codegen_access block_maker builder sym a in
    let a_maker = fun builder -> p in
    let b_maker = fun builder -> codegen_expr block_maker builder sym b in
    let v = build_binary_operator op block_maker a_maker b_maker builder (a.ann, b.ann) in
    (* Finally, store the variable back as it would be done in an assignment expression *)
    L.build_store v p builder |> ignore;
    (* This expression evaluates to the assigned value, so return it *)
    v
  | UnaryOp(uop, e) ->
    let ev = codegen_expr block_maker builder sym e in
    unary_operator (uop, e.ann) ev "" builder
  | BinaryOp(op, a, b) ->
    (* Define the two lazy building closures for the subexpression *)
    let a_maker = fun builder -> codegen_expr block_maker builder sym a in
    let b_maker = fun builder -> codegen_expr block_maker builder sym b in
    build_binary_operator op block_maker a_maker b_maker builder (a.ann, b.ann)
  | Call(f, args) ->
    let fundef =
      (match Symbol_table.lookup f sym.fun_sym with
        | Some(v) -> v
        | None    -> Util.raise_codegen_error @@ "Undefined function \"" ^ f ^ "\".") in
    (* Sequentially generate (in a left-to-right evaluation order, as given by List.map)
       the concrete function call arguments *)
    let actuals = List.map (codegen_expr block_maker builder sym) args in
    L.build_call fundef (Array.of_list actuals) "" builder

(**
  Generate the LLVM code for a binary operation expression.
  @param op the binary operator to apply
  @param block_maker a closure to create new blocks in the current function
  @param a_maker a closure that given a builder generates the first  expression
  @param b_maker a closure that given a builder generates the second expression
  @param builder the builder used to generate the instructions; if the function
         requires it, it might be modified to point to another newly created block
  @param types a tuple with the types of the given expression
  @returns an llvalue representing the value produced by the expression
  @throws Codegen_error if an error occurs
 *)
and build_binary_operator op block_maker a_maker b_maker builder types =
  (* Helper function that simply sequentially constructs and combines
     the two operands given to the binary operator *)
  let strict_block_sequence instr =
    let av = a_maker builder in
    let bv = b_maker builder in
    instr av bv "" builder
  in match types with
  | (TypB, TypB) -> codegen_boolean_op op block_maker a_maker b_maker builder
                    (* Obtain the concrete instruction operation for the given expression,
                       and simply generate both expression using their maker closures *)
  | _            -> strict_block_sequence (binary_operator (op, types))

(**
  Generate the LLVM code for a short-circuiting boolean application.
  @param op the logical binary operator to apply
  @param block_maker a closure to create new blocks in the current function
  @param a_maker a closure that given a builder generates the first  expression
  @param b_maker a closure that given a builder generates the second expression
  @param builder the builder used to generate the instructions; if the function
         requires it, it might be modified to point to another newly created block
  @returns an llvalue representing the value produced by the expression
  @throws Codegen_error if an error occurs
 *)
and codegen_boolean_op op block_maker a_maker b_maker builder =
  let (quit_value, quit_comparison) = boolean_operations_quit_conditions op in

  let left_block    = block_maker "left"   in
  let right_block   = block_maker "right"  in
  let cont_block    = block_maker "result" in
  let left_builder  = L.builder_at_end llcontext left_block  in
  let right_builder = L.builder_at_end llcontext right_block in
  let cont_builder  = L.builder_at_end llcontext cont_block  in

  (* Directly branch to the left expression, so that it can be
     cleanly separated from the original builder *)
  add_terminator builder (L.build_br left_block);

  (* Codegen the first expression *)
  let a_value = a_maker left_builder in
  (* Continue if the value produced by the condition is or is not false, depending on the operator. *)
  let condition = L.build_icmp quit_comparison a_value llvm_false "" left_builder in
  add_terminator left_builder (L.build_cond_br condition cont_block right_block);

  (* We managed to arrive here, codegen the second expression. *)
  let b_value = b_maker right_builder in
  add_terminator right_builder (L.build_br cont_block);

  (* Use the phi instruction to get the final, either short-circuited or fully-evaluated, result. *)
  let result = Llvm.build_phi [(quit_value, left_block) ; (b_value, right_block)] "" cont_builder in

  (* Reposition the builder at the end of the newly created cont_block. *)
  L.position_at_end cont_block builder;

  (* Finally, return the llvalue for the result. *)
  result

(* Global/local variables and parameters definition section *)

(* Default variable initializer; exploit the Llvm.undef utility to give the same semantics as C. *)
let var_initializer t = L.undef (lltype_of_typ t)

(* Codegen a global variable declaration. *)
let codegen_global llmodule sym (t, id) =
  let var = L.define_global id (var_initializer t) llmodule in
  let _ = Symbol_table.add_entry id var sym.var_sym in ()

(* Codegen a standard variable declaration. *)
let codegen_vardecl builder sym (t, id) =
  let local = L.build_alloca (lltype_of_typ t) "" builder in
  let _ = Symbol_table.add_entry id local sym.var_sym in local

(* Codegen the declaration of a parameter variable, in the function body. *)
let codegen_local_param builder sym (t, id) param =
  match t with
    | TypA(_, _) ->
      (* If the type in question is a direct array, we do not need to allocate the
         variable space for the array, and we directly use the pointer passed as llvalue.
         This is also part of the implicit reason why it is easier to assume that arrays
         cannot be reassigned.*)
      Symbol_table.add_entry id param sym.var_sym |> ignore
    | _ ->
      (* Else, first allocate the value, store the parameter in it and then insert the
          allocated space as llvalue in the symbol table.  *)
      let local = L.build_alloca (lltype_of_typ t) "" builder in
      Symbol_table.add_entry id local sym.var_sym |> ignore;
      L.build_store param local builder           |> ignore

(**
  Generate the LLVM code for a statement or declaration expression inside a block.
  @param fundef the current function definition
  @param sym the current symbol table
  @param builder the builder used to generate the instructions; if the function
         requires it, it might be modified to point to another newly created block
  @param s the declaration or statement expression
  @returns true iff this expression does not interrupt the codegen flow of the next instructions
  @throws Codegen_error if an error occurs
 *)
let rec codegen_stmtordec fundef sym builder s =
  match s.node with
  | Dec(t, id) -> codegen_vardecl builder sym (t, id) |> ignore; true
  | Stmt(s)    -> codegen_stmt fundef sym builder s

(**
  Generate the LLVM code for a statement.
  @param fundef the current function definition llvalue
  @param sym the current symbol table
  @param builder the builder used to generate the instructions; if the function
         requires it, it might be modified to point to another newly created block
  @returns true iff this instruction does not interrupt the codegen flow of the next instructions
  @throws Codegen_error if an error occurs
 *)
and codegen_stmt fundef sym builder stmt =
  (* Closure that allows expressions to create new blocks in the current function *)
  let block_maker name = L.append_block llcontext name fundef in
  (* Utility function that creates a while-like looping structure.
     It requires a closure picking the initial block from which execution should start,
     choosing from the test_block (while construct) and the body_block (do-while construct). *)
  let build_while choose_first_block cond body =
    let test_block = L.append_block llcontext "test" fundef in
    let body_block = L.append_block llcontext "body" fundef in
    let cont_block = L.append_block llcontext "cont" fundef in
    let test_builder = L.builder_at_end llcontext test_block in
    let body_builder = L.builder_at_end llcontext body_block in

    (* Immediately jump to the first block to be executed *)
    add_terminator builder (L.build_br @@ choose_first_block test_block body_block) |> ignore;

    (* Codegen the test: first codegen the boolean expression, then branch according to it *)
    let cond_val = codegen_expr block_maker test_builder sym cond in
    L.build_cond_br cond_val body_block cont_block test_builder |> ignore;

    (* Codgen the body; we try to branch to the test block if no terminator already exists *)
    codegen_stmt fundef sym body_builder body  |> ignore;
    add_terminator body_builder (L.build_br test_block) |> ignore;

    (* Update the initial builder given to point at the end of the cont_block *)
    L.position_at_end cont_block builder;

    (* In all circumstances, we return true to signal the fact that codegen
       after this statement can proceed, even if return instructions appear inside of
       while blocks *)
    true
  in match stmt.node with
  | Block(b) ->
    let block_sym = {sym with var_sym = Symbol_table.begin_block sym.var_sym} in
    let fold_block_statement continue stmtordec =
      if continue then (* If we can continue, generate the next statement/declaration *)
        codegen_stmtordec fundef block_sym builder stmtordec
      else
        false (* Otherwise, make the entire block return false *)
    (* Foldl each statement in the block, starting with true *)
    in List.fold_left fold_block_statement true b
  | Expr(e) -> codegen_expr block_maker builder sym e |> ignore; true
  | Return(opt_e) ->
    (match opt_e with
     | None    -> add_terminator builder L.build_ret_void
     | Some(e) -> let ret_expr = codegen_expr block_maker builder sym e in
                  add_terminator builder (L.build_ret ret_expr));
                  false (* Do not continue codegen to the subsequent statements *)
  | If(c, then_stmt, else_stmt) ->
    let then_block = L.append_block llcontext "then" fundef in
    let else_block = L.append_block llcontext "else" fundef in
    let cont_block = L.append_block llcontext "cont" fundef in
    let then_builder = L.builder_at_end llcontext then_block in
    let else_builder = L.builder_at_end llcontext else_block in

    (* Immediately generate the condition and then branch to either block *)
    let condition = codegen_expr block_maker builder sym c in
    L.build_cond_br condition then_block else_block builder  |> ignore;

    (* Codegen the then_block *)
    codegen_stmt fundef sym then_builder then_stmt           |> ignore;
    add_terminator then_builder (L.build_br cont_block)      |> ignore;

    (* Codegen the else_block *)
    codegen_stmt fundef sym else_builder else_stmt           |> ignore;
    add_terminator else_builder (L.build_br cont_block)      |> ignore;

    (* Update the initial builder given to point at the end of the cont_block *)
    L.position_at_end cont_block builder;

    (* Again, in all circumstances we return true to signal the fact that codegen
       after this statement can proceed, even if return instructions appear inside of
       the inner blocks *)
    true
  | While(cond, body)   -> build_while (fun t b -> t) cond body (* Pick the test block as entry *)
  | DoWhile(body, cond) -> build_while (fun t b -> b) cond body (* Pick the body block as entry *)

(**
  Generate the LLVM code for a function declaration.
  @param llmodule the current llmodule
  @param sym the current symbol table
  @param f the semantically checked fundef structure
  @throws Codegen_error if an error occurs
 *)
let codegen_fundecl llmodule sym f =
  let return_type   = lltype_of_typ f.typ in
  let formals_types = f.formals |> List.map fst |> List.map lltype_of_typ in
  let function_type = L.function_type return_type (Array.of_list formals_types) in
  let fundef        = L.define_function f.fname function_type llmodule in
  let self_sym      = {sym with fun_sym = Symbol_table.add_entry f.fname fundef sym.fun_sym} in
  let builder       = L.builder_at_end llcontext (L.entry_block fundef) in
  let parameters    = Array.to_list (L.params fundef) in
  let local_sym     = {self_sym with var_sym = Symbol_table.begin_block sym.var_sym} in

  (* First, declare the parameter variables *)
  List.iter2 (codegen_local_param builder local_sym) f.formals parameters;

  (* Start the main code generation of the function *)
  codegen_stmt fundef local_sym builder f.body |> ignore;

  (* Add an implicit return void terminator, if it is not already present *)
  match f.typ with
    | TypV -> add_terminator builder L.build_ret_void
    | _ -> ()

(**
  Generate the LLVM code for a top-level declaration.
  @param llmodule the current llmodule
  @param sym the current symbol table
  @param topdecl the AST node representing the declaration
  @throws Codegen_error if an error occurs
 *)
let codegen_topdecl llmodule sym topdecl =
  match topdecl.node with
  | Fundecl(fundecl) -> codegen_fundecl llmodule sym fundecl
  | Vardecl(t, id)   -> codegen_global  llmodule sym (t, id)

(* List of default base builtin functions to be declared *)
let builtin_functions =
  [ ("print",  L.function_type void_type [| int_type |])
  ; ("getint", L.function_type int_type  [||]          )
  ]

(* Globally declare a builtin function prototype *)
let declare_builtin llmodule sym (name, lltype) =
  Symbol_table.add_entry name (L.declare_function name lltype llmodule) sym.fun_sym |> ignore

(* Main resulting LLVM module name *)
let module_name = "microcc"

(**
  Generate the LLVM module given a semantically checked AST program.
  @param the semantically check program node
  @returns the resulting compiled llmodule
 *)
let codegen (Prog(topdecls)) =
  let llmodule = L.create_module llcontext module_name in
  let starting_sym = { fun_sym = Symbol_table.empty_table ()
                     ; var_sym = Symbol_table.empty_table ()
                     } in
  List.iter (declare_builtin llmodule starting_sym) builtin_functions;
  List.iter (codegen_topdecl llmodule starting_sym) topdecls;
  llmodule
