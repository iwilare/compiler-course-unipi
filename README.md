# MicroC LLVM compiler

Compiler for a C-like language written in OCaml, using LLVM as compilation backend and ocamllex/Menhir as frontend. [Read the report documenting the main design choices.](report.pdf)

## Features

- `do-while` loops;
- pre/post increment/decrement operators, i.e., `++` and `--`;
- abbreviation for assignment operators, i.e., `+=`, `-=`, `*=`, `/=` and `%=`;
- variable declaration with initialization, e.g., `int i = 0`;
- multi-dimensional arrays;
- floating point arithmetic;
- strings as in C, i.e. null-terminated arrays of characters;
- structs.

## Files structure

### Syntax

- [ast.ml          ](src/ast.ml): definition of the polymorphically annotated AST and its pretty-printing procedures.
- [position.ml     ](src/position.ml): definition of a located AST and of positions, shared among all modules. This is separated from the generic AST module ast.ml, of which this file represents a concrete AST instance with positions as annotations.
- [symbol_table.mli](src/symbol_table.mli): the symbol table.mli interface, specifying the procedures implemented by the Symbol table abstract data type.
- [symbol_table.ml ](src/symbol_table.ml): concrete implementation of the symbol table.mli interface, using linked lists of hash tables.

### Frontend

- [scanner.mll     ](src/scanner.mll): ocamllex specification of the Scanner module.
- [parser.mly      ](src/parser.mly): Menhir specification of the Parser module, and is exposed through the parser engine.ml interface.
- [parser_engine.ml](src/parser_engine.ml): exposed interface of the Parser, leveraging the Menhir Incremental API.

### Backend

- [semant.ml       ](src/semant.ml): Semant module, implementing the semantic checker and the semantic AST definition.
- [codegen.ml      ](src/codegen.ml): Codegen module, interfacing the compiler with the LLVM compiler framework and transforming semantically-checked ASTs into the LLVM IR.
- [builtins.ml     ](src/builtins.ml): name and type information of the MicroC builtin functions, so that they are logically independent from both the Semant and the Codegen module and can be separately extended with more procedures.
- [util.ml         ](src/util.ml): utilities and error managing procedures.

### Main

- [microcc.ml      ](src/microcc.ml): main compiler command-line interface logic.
- [opt_pass.ml     ](src/opt_pass.ml): optimization flags that can be optionally provided to LLVM to optimize the resulting bitcode.
- [rt-support.c    ](src/rt-support.c): runtime support for MicroC programs, with primitives such as print, getint, printchar, etc.

## Getting started

Ensure all requirements are installed, then simply run:

```
make
```

## Requirements

- ocamlbuild
- menhir
- ppx_deriving
- llvm
