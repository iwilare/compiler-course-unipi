open Ast

(* Declare a global variable collecting the information about
   the builtin runtime functions that will be linked against
   the final executable. These function need to be declared
   both for semantic checking and for code-generation.
   Inserting them in this environment allows us to not repeat
   ourselves in defining them and to separate the semantic and
   codegen logic from the functions per-se; this turns out to
   be very practical when extending the code library with more
   functions. *)

let builtin_functions =
    [ "print",      TypV, [TypI]
    ; "printfloat", TypV, [TypF]
    ; "printchar",  TypV, [TypC]
    ; "getint",     TypI, []
    ]
