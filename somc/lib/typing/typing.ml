module Env = Env
module TAst = Tast
module PrintTAst = Print_tast
module Types = Types

open Parse.Ast
open TAst
open Infer

let mk s i = Some {span=s; item=i}

let rec typecheck_tl_node env node =
  let Parse.Ast.{span; item : toplevel} = node in
  match item with
    | TL_Definition {vd_name; vd_expr} ->
      (* let patt' = infer_patt ~level:0 env vd_patt in *)
      let name' = { span = vd_name.span; item = Types.new_var 0 } in
      let expr' = infer_expr env vd_expr in
      unify env span name'.item expr'.typ;
      
      let _expr'' = set_ty (generalize (-1) expr'.typ) expr' in
      failwith "TODO"
      (* mk span (TL_Definition {patt=patt'; expr=expr''})  *)
  
    | TL_Type_Definition {td_name; td_params=_; td_typ} ->
      let t = Parse_type.parse env 0 td_typ.item in
      (* TODO: params *)
      Env.add_alias env td_name.item t;
      None

    | TL_Import _ -> failwith "Import node survived analysis"

    | TL_Module (n, tls) ->
      (* TODO: only use an empty env if typechecking
         a section that's actually a file? *)
      let env' = Env.empty () in
      let tls' = typecheck env' tls in
      (* prerr_string (n.item ^ ": "); Env.show stderr env'; *)
      Env.add_section env n.item env';
      mk span (TL_Section (n, tls'))

and typecheck env (ast : ast) =
  let rec go acc = function
      | hd :: tl ->
        let hd' = typecheck_tl_node env hd in
        begin match hd' with
          | Some hd' -> go (acc @ [hd']) tl
          | None -> go acc tl
        end
      | _ -> acc
  in
  let tast = go [] ast in
  tast