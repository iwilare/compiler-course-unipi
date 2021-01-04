exception DuplicateEntry

(* Implement a symbol table as a chained list of hash tables,
   with each table representing a scope and with lookup searching
   and traversing up through the chain. The head of the list
   represent the current innermost scope, going outwards. *)
type 'a t = ((string, 'a) Hashtbl.t) list

(* Create an empty global base scope. *)
let empty_table () = [Hashtbl.create 0]

(* Simply append a newly created scope at the head of the table;
   this does not alter nor modify any of the above scopes, which can
   be effectively shared, similar as the inner nodes of a tree. *)
let begin_block table = Hashtbl.create 0 :: table

(* This function is essentially never used in the codegen section,
   since no new scope creation should alter previously created scopes. *)
let end_block table = List.tl table

(* Simply lookup the variable in current scopre/hash table; if it is
   not present, None is returned. Variable shadowing is enabled thanks
   to the fact that lookup starts searching from the innermost scope. *)
let rec lookup symbol table =
    match table with
        | []       -> None
        | (hd::tl) ->
            match Hashtbl.find_opt hd symbol with
                | None    -> lookup symbol tl
                | Some(v) -> Some(v)

(* Directly define a variable in the current scope, modifying the
   first scope and inserting the variable data in the first hash table.
   Throw DuplicateEntry if a variable is declared in the exact same scope.
   Note that since we not query upper scope, a variable can shadow*one in
   the previous scope. *)
let add_entry symbol info table =
    let current = List.hd table in
    match Hashtbl.find_opt current symbol with
        | None    -> Hashtbl.add current symbol info; table
        | Some(v) -> raise DuplicateEntry
