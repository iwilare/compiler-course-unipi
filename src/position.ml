(* Annotation for Position-annotated AST *)

(* These definitions are both common for the Semant module
   and the Parser module. They are defined here in order to logically
   identify the annotation of a "located AST" having lexing positions
   as annotations, while allowing the Semant module not to have to import
   the entire Parser module just to have the position type definition,
   which is kept here as abstraction for the internal representation
   of locations. *)

(* Position information given from the lexical nodes up to the AST nodes *)
type position = Lexing.position * Lexing.position

(* Dummy position used for desugaring *)
let dummy_pos = (Lexing.dummy_pos, Lexing.dummy_pos)
