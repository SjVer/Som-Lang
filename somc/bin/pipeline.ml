module Ident = Symbols.Ident
module C = Configs.Cli

let has_dumped () =
  Report.last_was_compact := false;
  Report.has_reported := true

module ReadFile = Query.Make(struct
  type a = string * Span.t option
  type r = string
  let c (file, imp_span) =
    try
      let chan = open_in file in
      let len = in_channel_length chan in
      let str = really_input_string chan len in
      close_in chan;
      str
    with _ ->
      let file = file in
      let open Report in
      let open Error in
      make_error (Other_error (Could_not_open file)) imp_span
      |> report;
      Stdlib.exit 1
end)

module ParseFile = Query.Make(struct
  type a = string * Span.t option
  type r = Parse.Ast.ast
  let c (file, imp_span) =
    let source = ReadFile.call (file, imp_span) in
    let ast = Parse.parse file source imp_span in

    if !C.args.dump_ast && imp_span = None then begin
      Report.report_note "dumping parsed AST:"; 
      Parse.Print_ast.print_ast ast;
      has_dumped ()
    end;

    ast
end)

module AnalyzeFile = Query.Make(struct
  type a = string * Span.t option
  type r = Parse.Ast.ast
  let c (file, imp_span) =
    let ast = ParseFile.call (file, imp_span) in
    let ast = Analysis.resolve ast in
    
    if !C.args.dump_rast && imp_span = None then begin
      Report.report_note "dumping resolved AST:";
      Parse.Print_ast.print_ast ast;
      has_dumped ()
    end;

    ast
end)

module TypecheckFile = Query.Make(struct
  type a = string
  type r = Typing.TAst.tast
  let c file =
    let ast = AnalyzeFile.call (file, None) in
    
    (* we can just manually insert the ast from the prelude
       in the parsed ast because it isn't really imported
       so it doesn't need its imports to be resolved. *)
    let ast' = if !C.args.no_prelude
      then ast
      else Analysis.add_implicit_prelude ast
    in

    let env = Typing.Env.empty in
    let tast = Typing.typecheck_ast env ast' in
    
    if !C.args.dump_tast then begin
      Report.report_note "dumping typed AST:";
      Typing.Print_tast.print_tast tast;
      has_dumped ()
    end;
    
    tast
end)

module LowerFile = Query.Make(struct
  type a = string
  type r = Lambda.Ir.program
  let c file =
    let tast = TypecheckFile.call file in
    if !Report.has_errored then Report.exit ();

    let program = Lambda.convert tast in

    if !C.args.dump_ir then begin
      Report.report_note "dumping lambda IR:";
      Lambda.Print.print_program program;
      has_dumped ()
    end;
    
    program
end)

module CodegenFile = Query.Make(struct
  type a = string
  type r = Codegen.cmodule
  let c file =
    let program = LowerFile.call file in
    let cmodule = Codegen.codegen_program program in
    
    if !C.args.dump_raw_llvm then begin
      Report.report_note "dumping C module:";
      Codegen.Print.print_cmodule cmodule;
      has_dumped ()
    end;

    cmodule
end)

(* export functions bc otherwise i'd somehow
   have to solve dependency cycles *)
let init () =
  Report.Util.read_file_fn := (fun f -> ReadFile.call (f, None));
  Analysis.Import.get_ast_from_file := fun f s ->
    AnalyzeFile.call (f, Some s)

let () = init ()
