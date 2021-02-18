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
  Information kept in the codegen symbol table for structures.
  It provides the lltype associated with the struct and the information
  contained in the struct declaration (i.e.: the field names and their types.)
  We keep a list in order to lookup the element index, later used in gep accesses.
*)
type struct_info = L.lltype * (identifier * L.lltype) list

(**
  The main scoped symbol table carried around during codegen.
  Contains the namespaces for functions and variables, and associates
  to each name either the function definition llvalue
  or the llvalue associated with the variable.
*)
type sym = { fun_sym    : L.llvalue   Symbol_table.t
           ; var_sym    : L.llvalue   Symbol_table.t
           ; struct_sym : struct_info Symbol_table.t
           }

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
  (* Operations with pointers: for now only equality is supported *)
  | (Eq,    (TypP(_), TypP(_))) -> L.build_icmp L.Icmp.Eq
  | (Neq,   (TypP(_), TypP(_))) -> L.build_icmp L.Icmp.Ne
  (* Operations with characters: for now, only equality is allowed *)
  | (Eq,    (TypC, TypC)) -> L.build_icmp L.Icmp.Eq
  | (Neq,   (TypC, TypC)) -> L.build_icmp L.Icmp.Ne
  (* Binary boolean operators: these versions do not provide short-circuiting *)
  | (And,   (TypB, TypB)) -> L.build_and
  | (Or,    (TypB, TypB)) -> L.build_or
  (* Operations with booleans: for now, only equality is allowed *)
  | (Eq,    (TypB, TypB)) -> L.build_icmp L.Icmp.Eq
  | (Neq,   (TypB, TypB)) -> L.build_icmp L.Icmp.Ne
  (* Comma operator, reserved for future definitions of the operator *)
  | (Comma, (_,    _))    -> (fun a b _ _ -> b)
  (* Otherwise, invalid *)
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
  Function incapsulating the logic of compile-time constant binary operators,
  associating each operator and the types of its operands with the
  LLVM instruction that constructs and implements it.
  @param the binary operator, and a tuple indicating the types of the given values
  @throws Codegen_error if the given operator and type combination is invalid.
          This should theoretically never happen, as the types given
          should have been semantically checked.
*)
let const_binary_operator = function
  (* Constant integer binary operators *)
  | (Add,   (TypI, TypI)) -> L.const_add
  | (Sub,   (TypI, TypI)) -> L.const_sub
  | (Mul,   (TypI, TypI)) -> L.const_mul
  | (Div,   (TypI, TypI)) -> L.const_sdiv
  | (Mod,   (TypI, TypI)) -> L.const_srem
  | (Lt,    (TypI, TypI)) -> L.const_icmp L.Icmp.Slt
  | (Le,    (TypI, TypI)) -> L.const_icmp L.Icmp.Sle
  | (Gt,    (TypI, TypI)) -> L.const_icmp L.Icmp.Sgt
  | (Ge,    (TypI, TypI)) -> L.const_icmp L.Icmp.Sge
  | (Eq,    (TypI, TypI)) -> L.const_icmp L.Icmp.Eq
  | (Neq,   (TypI, TypI)) -> L.const_icmp L.Icmp.Ne
  (* Constant floating binary operators *)
  | (Add,   (TypF, TypF)) -> L.const_fadd
  | (Sub,   (TypF, TypF)) -> L.const_fsub
  | (Mul,   (TypF, TypF)) -> L.const_fmul
  | (Div,   (TypF, TypF)) -> L.const_fdiv
  | (Lt,    (TypF, TypF)) -> L.const_fcmp L.Fcmp.Olt
  | (Le,    (TypF, TypF)) -> L.const_fcmp L.Fcmp.Ole
  | (Gt,    (TypF, TypF)) -> L.const_fcmp L.Fcmp.Ogt
  | (Ge,    (TypF, TypF)) -> L.const_fcmp L.Fcmp.Oge
  | (Eq,    (TypF, TypF)) -> L.const_fcmp L.Fcmp.Oeq
  | (Neq,   (TypF, TypF)) -> L.const_fcmp L.Fcmp.One
  (* Constant boolean operators *)
  | (And,   (TypB, TypB)) -> L.const_and
  | (Or,    (TypB, TypB)) -> L.const_or
  (* Constant operations with characters: for now, only equality is allowed *)
  | (Eq,    (TypC, TypC)) -> L.const_icmp L.Icmp.Eq
  | (Neq,   (TypC, TypC)) -> L.const_icmp L.Icmp.Ne
  (* Comma operator, reserved for future definitions of the operator *)
  | (Comma, (_,    _))    -> (fun a b -> b)
  (* Otherwise, invalid *)
  | _ -> Util.raise_codegen_error "Invalid types with const binary operator"

(**
  Function incapsulating the logic of constant unary operators,
  associating each operator and the type of its operand with the
  LLVM instruction that constructs and implements it.
  @param the unary operator, and the types of the operand given
  @throws Codegen_error if the given operator and type combination is invalid.
          This should theoretically never happen, as the type and operator given
          should have been semantically checked.
*)
let const_unary_operator = function
  | (Neg, TypI) -> L.const_neg
  | (Neg, TypF) -> L.const_fneg
  | (Not, TypB) -> L.const_not
  | _ -> Util.raise_codegen_error "Invalid types with const unary operator."

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

(**
  Function translating a MicroC type into a concrete LLVM implementation type.
  @param struct_sym the symbol-table containing struct definitions
  @param the type to be translated
  @throws Codegen_error if an invalid non-existent struct is given.
          This should theoretically never happen, as the structs are already
          semantically and scope checked.
*)
let rec lltype_of_typ struct_sym = function
  | TypI             -> int_type
  | TypB             -> bool_type
  | TypC             -> char_type
  | TypF             -> float_type
  | TypA(t, Some(s)) -> L.array_type (lltype_of_typ struct_sym t) s
  | TypA(t, None)       (* Unbounded arrays are for all purposes considered as pointers with
                           clang and other tools compiling C to LLVM also do this.
                           This behaviour is also supported in the C standard; for more info, see
                           the report description. *)
  | TypP(t)          -> L.pointer_type (lltype_of_typ struct_sym t)
  | TypV             -> void_type
  | TypS(name)       ->
    (match Symbol_table.lookup name struct_sym with
    | None -> Util.raise_codegen_error @@ "Undefined struct \"" ^ name ^ "\"."
    | Some(t, _) -> t)

(**
  Use the given builder to add a terminating instruction to a block iff
  another terminating instruction does not already exist at the end.
  Else, simply do nothing. This helper function is necessary in order
  not to insert additional terminating instructions to blocks that
  already terminate (e.g.: using an early return)
  @param builder the builder used to insert the terminator given
  @param f the function to which the builder is passed to
*)
let add_terminator builder f =
  (* Get the block terminator of the builder given, if present *)
  match L.block_terminator (L.insertion_block builder) with
    | Some(_) -> ()
    | None    -> f builder |> ignore

(**
  Cast the given address into a pointer, if required.
  For example, this is necessary in the case where a sized
  array is given to a function that expects an unsized
  array (or, in the future, a pointer casted from the array.)
  @param et the expected type of the access expression
  @param at the actual   type of the access expression
  @param var the address given by the access
  @param builder the current builder used to write instructions
  @return an llvalue with the expected semantic type
 *)
let unify_access et at var builder =
  (match (et, at) with
  (* Unsized arrays are already pointers; simply return their memory *)
  | (TypA(_, None),    TypA(_, None))    -> var
  (* A sized array is expected, simply return the memory address as-is *)
  | (TypA(_, Some(_)), TypA(_, Some(_))) -> var
  (* An unsized array is expected, so obtain the initial pointer to the sized array *)
  | (TypA(_, None),    TypA(_, Some(_))) -> L.build_gep var [| llvm_zero; llvm_zero |] "" builder
  (* Standard case: load the value from the access address *)
  | _                                    -> L.build_load var "" builder)

(**
  Generate the LLVM code for a semantically annotated access AST node,
  returning *only* the address that the access expression entails, and not
  its value.
  @param block_maker a closure to create new blocks in the current function
  @param sym the variable symbol table for the current scope
  @param builder the builder used to generate the instructions; if the function
         requires it, it might be modified to point to another newly created block
  @param a the access node
  @returns an llvalue that represents the *address* being accessed by this expression
  @throws Codegen_error if an error occurs
*)
let rec codegen_access block_maker sym builder a =
  match a.node with
  | AccVar(id) ->
    (match Symbol_table.lookup id (sym.var_sym) with
     | Some(v) -> v
     | None    -> Util.raise_codegen_error @@ "Undefined variable \"" ^ id ^ "\".")
  | AccDeref(e) ->
    (* Simply return the value given by the expression, since that
       will be the address; if needed, it will be dereferenced later *)
    codegen_expr block_maker sym builder e
  | AccIndex(a, i) ->
    let var = codegen_access block_maker sym builder a in
    let index = codegen_expr block_maker sym builder i in
    (* Depending on the type of the reference, access it in different ways: *)
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
  | AccStruct(a, m) ->
    let var = codegen_access block_maker sym builder a in
    (* Get the field index corresponding to the requested variable.
       (Ideally, we could refactor this section to monadically combine
        these sequential steps with a >>= bind in a suitable monad.) *)
    let field_index =
      (* Get the struct name from the annotation. *)
      (match a.ann with
      | TypS(sname) ->
        (* Get the struct information from the struct name *)
        (match Symbol_table.lookup sname sym.struct_sym with
        | Some(st, fields) ->
          let names_to_indexes = List.mapi (fun i (v, _) -> (v, i)) fields in
          (* Get the field index from the struct information *)
          (match List.assoc_opt m names_to_indexes with
          | Some(index) -> index
          | None ->  Util.raise_codegen_error "Invalid undefined member in struct.")
        | None -> Util.raise_codegen_error "Invalid undefined struct name.")
      | _ -> Util.raise_codegen_error "Invalid non-struct type annotation in member access.") in
    L.build_struct_gep var field_index "" builder

(**
  Generate the LLVM code for a semantically annotated expr AST node.
  @param block_maker a closure to create new blocks in the current function
  @param sym the variable symbol table for the current scope
  @param builder the builder used to generate the instructions; if the function
         requires it, it might be modified to point to another newly created block
  @param e the expr node
  @returns an llvalue representing the value produced by the expression
  @throws Codegen_error if an error occurs
 *)
and codegen_expr block_maker sym builder e =
  match e.node with
  | ILiteral(i)  -> L.const_int int_type i
  | CLiteral(c)  -> L.const_int char_type (Char.code c)
  | BLiteral(b)  -> if b then llvm_true else llvm_false
  | FLiteral(f)  -> L.const_float float_type f
  | SLiteral(s)  -> (* The literal given might be required to be cast to
                       an unsized array, as it is done for standard arrays.
                       L.build_global_string does not contain a null terminator, which we add.
                       Note how this differs with the global string declaration,
                       where instead the llvalue initializer L.const_string is used. *)
                    let str = L.build_global_string (s ^ String.make 1 '\000') "" builder in
                    unify_access e.ann (string_type s) str builder
  | Null         -> (* Use the annotation given by the semantic checking to
                       define the polymorphic NULL type with the appropriate type *)
                    L.const_pointer_null (lltype_of_typ sym.struct_sym e.ann)
  | Addr(a)      -> (* Access expression already returns a pointer here, simply return the llvalue *)
                    codegen_access block_maker sym builder a
  | Increment(a, p, i) ->
    (* Simple function to pick the correct resulting llvalue
       according to the given kind of operation *)
    let pick_value before after = function
      | Pre  -> after
      | Post -> before in
    (* Obtain the correct kind of instruction, according both
       to the kind of operation and the operand type *)
    let do_increment = increment_operator (i, a.ann) in
    (* Start the concrete sequential codegen section: *)
    let var    = codegen_access block_maker sym builder a in
    let before = L.build_load var     "" builder in
    let after  = do_increment before  "" builder in
    let _      = L.build_store after var builder in
    (* After the variable has been modified, pick the resulting value *)
    pick_value before after p
  | Access(a) ->
    (* Obtain the address and extract its value. *)
    let var = codegen_access block_maker sym builder a in
    (* Unify the given address if necessary, using as expected type the
       type of this expression and the access type. *)
    unify_access e.ann a.ann var builder
  | Assign(a, e) ->
    let var   = codegen_access block_maker sym builder a in
    let value = codegen_expr   block_maker sym builder e in
    (* All access operations return an address, so a final store is required *)
    L.build_store value var builder |> ignore;
    value
  | AssignOp(a, op, b) ->
    (* Codegen the access, so that pre/post-increment operations will
       only be executed once. This is the reason why this expression is not
       desugared and instead treated here as an independent case. *)
    let var = codegen_access block_maker sym builder a in
    (* The left value requires a load, since var is an address coming from access. *)
    let a_maker = fun builder -> L.build_load var "" builder in
    let b_maker = fun builder -> codegen_expr block_maker sym builder b in
    let value   = build_binary_operator op block_maker a_maker b_maker builder (a.ann, b.ann) in
    (* Finally, store the variable back as it would be done in an assignment expression *)
    L.build_store value var builder |> ignore;
    (* This expression evaluates to the assigned value, so return it *)
    value
  | UnaryOp(uop, e) ->
    let ev = codegen_expr block_maker sym builder e in
    unary_operator (uop, e.ann) ev "" builder
  | BinaryOp(op, a, b) ->
    (* Define the two lazy building closures for the subexpression *)
    let a_maker = fun builder -> codegen_expr block_maker sym builder a in
    let b_maker = fun builder -> codegen_expr block_maker sym builder b in
    build_binary_operator op block_maker a_maker b_maker builder (a.ann, b.ann)
  | Call(f, args) ->
    let fundef =
      (match Symbol_table.lookup f sym.fun_sym with
        | Some(v) -> v
        | None    -> Util.raise_codegen_error @@ "Undefined function \"" ^ f ^ "\".") in
    (* Sequentially generate in a left-to-right evaluation order
       the concrete function call arguments, as required by the tests *)
    let actuals = List.map (codegen_expr block_maker sym builder) args in
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
  in match op with
  | And | Or -> build_boolean_operator op block_maker a_maker b_maker builder
                (* Obtain the concrete instruction operation for the given expression,
                   and simply generate both expression using their maker closures *)
  | _        -> strict_block_sequence (binary_operator (op, types))

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
and build_boolean_operator op block_maker a_maker b_maker builder =
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
let var_initializer struct_sym t = L.undef (lltype_of_typ struct_sym t)

(**
  Codegen a global variable initializer for the compile-time constant expression.
  This function might in the future encapsulate and allocate values differently
  compared to codegen_expr, since here we generate initializers for a static storage.
  Compile-time constants are calculated here using the corresponding constant version
  of the binary/unary operators employed.
  @param sym the current symbol table, used for struct types
  @param e the expression to generate the initializer
  @return the llvalue corresponding to the given constant value expression
  @throws Codegen_error if an error occurs
*)
let rec expr_initializer sym e = match e.node with
  | ILiteral(i)  -> L.const_int int_type i
  | CLiteral(c)  -> L.const_int char_type (Char.code c)
  | BLiteral(b)  -> if b then llvm_true else llvm_false
  | FLiteral(f)  -> L.const_float float_type f
  | SLiteral(s)  -> (* Here we give a string literal initializer, with a
                       null-terminator as indicated by the ending "z"; note how in
                       the standard codegen we generate a L.build_global_string instead. *)
                    L.const_stringz llcontext s
  | Null         -> L.const_pointer_null (lltype_of_typ sym.struct_sym e.ann)
  | UnaryOp(uop, a)    -> const_unary_operator (uop, a.ann) (expr_initializer sym a)
  | BinaryOp(op, a, b) -> const_binary_operator (op, (a.ann, b.ann)) (expr_initializer sym a) (expr_initializer sym b)
  | _ -> Util.raise_codegen_error @@ "Invalid constant global initializer."

(* Codegen a global variable declaration. *)
let codegen_global llmodule sym (t, id) init =
  (* Obtain the initializer expression for the global variable.
     If there is no constant initializer, then use the default initializer for the
     type being declared. Otherwise, codegen the llvalue of the constant expression. *)
  let global_init =
    Option.fold ~none:(var_initializer sym.struct_sym t)
                ~some:(expr_initializer sym) init in
  let var = L.define_global id global_init llmodule in
  let _ = Symbol_table.add_entry id var sym.var_sym in ()

(* Codegen a standard local variable declaration, with optional initializer. *)
let codegen_local_vardecl sym builder block_maker (t, id, opt_e) =
  let var = L.build_alloca (lltype_of_typ sym.struct_sym t) "" builder in
  (* Helper function to generate a initialized local variable, if it is
     initialized. Special cases are required when the given value is an array. *)
  let get_initialized_var e =
      let ev = codegen_expr block_maker sym builder e in
      (match t with
        (* If we are considering an array variable, then
           the variable simply aliases the given array;
           return it as-is, without allocating any more memory. *)
        | TypA(_, _) -> ev
        (* Else, store (i.e.: copy) the value in the variable. *)
        | _          -> L.build_store ev var builder |> ignore; var) in
  (* If the initializer is not given, simply give the allocated variable. *)
  let local = Option.fold ~none:var ~some:get_initialized_var opt_e in
  let _ = Symbol_table.add_entry id local sym.var_sym in local

(* Codegen the declaration of a parameter variable, inside the function body. *)
let codegen_local_param sym builder (t, id) param =
  match t with
    | TypA(_, _) ->
      (* If the type considered is a direct array, we do not need to allocate the
         variable space for the array, and we directly use the pointer passed as llvalue.
         This is also part of the implicit reason why it is easier to assume that arrays
         cannot be reassigned. *)
      Symbol_table.add_entry id param sym.var_sym |> ignore
    | _ ->
      (* Else, first allocate the value, store the parameter in it and then insert the
          allocated space as llvalue in the symbol table.  *)
      let local = L.build_alloca (lltype_of_typ sym.struct_sym t) "" builder in
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
let rec codegen_stmtordec fundef sym block_maker builder s =
  match s.node with
  | Dec(t, id, opt_e) -> codegen_local_vardecl sym builder block_maker (t, id, opt_e) |> ignore; true
  | Stmt(s)           -> codegen_stmt fundef sym builder s

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
    let cond_val = codegen_expr block_maker sym test_builder cond in
    L.build_cond_br cond_val body_block cont_block test_builder |> ignore;

    (* Codgen the body; we try to branch to the test block if no terminator already exists *)
    codegen_stmt fundef sym body_builder body           |> ignore;
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
        codegen_stmtordec fundef block_sym block_maker builder stmtordec
      else
        false (* Otherwise, make the entire block return false.
                 An additional check could be inserted here to signal the fact that
                 if there are other statements they will never be executed/generated. *)
    (* Foldl each statement in the block, starting with true *)
    in List.fold_left fold_block_statement true b
  | Expr(e) -> codegen_expr block_maker sym builder e |> ignore; true
  | Return(opt_e) ->
    (match opt_e with
     | None    -> add_terminator builder L.build_ret_void
     | Some(e) -> let ret_expr = codegen_expr block_maker sym builder e in
                  add_terminator builder (L.build_ret ret_expr));
                  false (* Do not continue codegen to the subsequent statements *)
  | If(c, then_stmt, else_stmt) ->
    let then_block = L.append_block llcontext "then" fundef in
    let else_block = L.append_block llcontext "else" fundef in
    let cont_block = L.append_block llcontext "cont" fundef in
    let then_builder = L.builder_at_end llcontext then_block in
    let else_builder = L.builder_at_end llcontext else_block in

    (* Immediately generate the condition and then branch to either block *)
    let condition = codegen_expr block_maker sym builder c in
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
  let return_type   = lltype_of_typ sym.struct_sym f.typ in
  let formals_types = f.formals |> List.map fst |> List.map (lltype_of_typ sym.struct_sym) in
  let function_type = L.function_type return_type (Array.of_list formals_types) in
  let fundef        = L.define_function f.fname function_type llmodule in
  let self_sym      = {sym with fun_sym = Symbol_table.add_entry f.fname fundef sym.fun_sym} in
  let builder       = L.builder_at_end llcontext (L.entry_block fundef) in
  let parameters    = Array.to_list (L.params fundef) in
  let local_sym     = {self_sym with var_sym = Symbol_table.begin_block sym.var_sym} in

  (* First, declare the parameter variables *)
  List.iter2 (codegen_local_param local_sym builder) f.formals parameters;

  (* Start the main code generation of the function *)
  codegen_stmt fundef local_sym builder f.body |> ignore;

  (* Add an implicit return terminator, if it is not already present.
     According to C semantics, if a non-void returning function does not
     explicitly return a value at the end of the function, then the returned
     value is simply undefined. In a similar fashion, we simply return an
     undefined value if the add_terminator function succeeds. *)
  match f.typ with
    | TypV -> add_terminator builder L.build_ret_void
    | _    -> add_terminator builder (L.build_ret (L.undef return_type))

(**
  Generate the LLVM code for a struct declaration.
  @param llmodule the current llmodule
  @param sym the current symbol table
  @param n the name of the struct
  @param fs the fields of the struct, as a list of (type, identifier)
  @param topdecl the AST node representing the declaration
  @throws Codegen_error if an error occurs
 *)
let codegen_struct llmodule sym (n, fs) =
  let typed_fields =
    List.map (fun (t, v) -> (v, lltype_of_typ sym.struct_sym t)) fs in
  let struct_type = L.struct_type llcontext (Array.of_list (List.map snd typed_fields)) in
  Symbol_table.add_entry n (struct_type, typed_fields) sym.struct_sym |> ignore

(**
  Generate the LLVM code for a top-level declaration.
  @param llmodule the current llmodule
  @param sym the current symbol table
  @param topdecl the AST node representing the declaration
  @throws Codegen_error if an error occurs
 *)
let codegen_topdecl llmodule sym topdecl =
  match topdecl.node with
  | Fundecl(fundecl)     -> codegen_fundecl llmodule sym fundecl
  | Vardecl(t, id, init) -> codegen_global  llmodule sym (t, id) init
  | Structdecl(n, fs)    -> codegen_struct  llmodule sym (n, fs)

(* List of default base builtin functions to be declared *)
let llvm_builtins sym =
  Builtins.builtin_functions
    |> List.map
      (fun (id, r, a) ->
        (id, L.function_type (lltype_of_typ sym.struct_sym r)
                             (List.map (lltype_of_typ sym.struct_sym) a |> Array.of_list)))

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
  let starting_sym = { fun_sym    = Symbol_table.empty_table ()
                     ; var_sym    = Symbol_table.empty_table ()
                     ; struct_sym = Symbol_table.empty_table ()
                     } in
  List.iter (declare_builtin llmodule starting_sym) (llvm_builtins starting_sym);
  List.iter (codegen_topdecl llmodule starting_sym) topdecls;
  llmodule
