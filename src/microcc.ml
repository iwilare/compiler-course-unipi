type action = Dump_ast | Semantic_check | Dump_llvm_ir | Compile

let () =
  Llvm.enable_pretty_stacktrace ();
  let action     = ref Compile in
  let filename   = ref ""      in
  let outputfile = ref "a.bc"  in
  let optimize   = ref false   in
  let spec_list = [
    "-p", Arg.Unit(fun () -> action := Dump_ast),       "Parse and print AST" ;
    "-s", Arg.Unit(fun () -> action := Semantic_check), "Perform semantic checks" ;
    "-d", Arg.Unit(fun () -> action := Dump_llvm_ir),   "Print the generated code" ;
    "-o", Arg.String(fun name -> outputfile := name),   "Place the output into file" ;
    "-O", Arg.Set(optimize),                            "Optimize the generated code" ;
    "-c", Arg.Unit(fun () -> action := Compile),        "Compile (default)"
  ]
  in
  let usage = Printf.sprintf "Usage:\t%s [-p|-s|-d|-c] <source_file>\n" (Sys.argv.(0)) in
  Arg.parse spec_list (fun file -> filename := file) usage;
  let in_channel = if !filename = "" then stdin else open_in !filename in
  let lexbuf = Lexing.from_channel in_channel ~with_positions:true in
  try
    let (Ast.Prog(ds)) as p = Parser_engine.parse lexbuf in

    if !action = Dump_ast then
      begin
        Printf.printf "Number of found declarations: %d\n" (List.length ds);
        Printf.printf "Ast dump\n%s\n" (Ast.show_program p)
      end
    else
      begin
        let semantic_ast = Semant.check p in
        if !action = Semantic_check then
          begin
            Printf.printf "Semantic checks: pass\n"
          end
        else
          begin
            let llvm_module = Codegen.codegen semantic_ast in
            (if !optimize then
               Opt_pass.optimize_module llvm_module |> ignore
            );
            if !action = Dump_llvm_ir then
              begin
                print_string (Llvm.string_of_llmodule llvm_module)
              end
            else if not (Llvm_bitwriter.write_bitcode_file llvm_module !outputfile) then
              Printf.fprintf stderr "An error occured when generating the file"
          end
      end
  with
  | Util.Lexing_error(m) ->
    Printf.fprintf stderr "Lexing error. %s:%s\n" !filename m; exit 1
  | Util.Syntax_error(m) ->
    Printf.fprintf stderr "Syntax error. %s:%s\n" !filename m; exit 2
  | Util.Semantic_error(m) ->
    Printf.fprintf stderr "Semantic error. %s:%s\n" !filename m; exit 3
  | Util.Codegen_error(m) ->
    Printf.fprintf stderr "Codegen error. %s:%s\n" !filename m; exit 3
