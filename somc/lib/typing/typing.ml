module Env = Env
module TAst = Tast
module Print_tast = Print_tast
module Types = Types

open Parse.Ast
open TAst
open Infer
open Unify

let typecheck_toplevel env (node : Ast.toplevel Ast.node) =
  let mk item = {span = node.span; item} in
  match node.item with
    | Ptl_value_def vdef ->
      (* there's no pattern so just type the bound expr *)
      let expr = infer_expr env vdef.vd_expr in
      let expr = set_ty (generalize (-1) expr.typ) expr in

      (* construct the updated env and typed vdef *)
      let env = Env.add_value env vdef.vd_name.item expr.typ in
      let vdef' =
        {
          vd_name = vdef.vd_name;
          vd_expr = expr;
        }
      in
      env, [mk (Ttl_value_def vdef')]
  
    | Ptl_type_def tdef ->
      let env, t = Parse_type.parse_complex
        env tdef.td_params tdef.td_name.item tdef.td_type.item
      in
      let env = Env.add_alias env tdef.td_name.item t in
      
      (* keep the tdef for backend purposes *)
      let tdef' =
        {
          td_name = tdef.td_name;
          td_type = {span = tdef.td_type.span; item = t};
        }
      in
      env, [mk (Ttl_type_def tdef')]

    | Ptl_extern_def edef ->
      let t = Parse_type.parse env 0 edef.ed_type.item in
      let env = Env.add_value env edef.ed_name.item t in

      (* keep the edef for backend purposes *)
      let edef' =
        {
          ed_native_name = edef.ed_native_name;
          ed_name = edef.ed_name;
          ed_type = {span = edef.ed_type.span; item = t};
        }
      in
      env, [mk (Ttl_extern_def edef')]

    | Ptl_import _ -> failwith "Import node survived analysis"
    | Ptl_module _ -> failwith "Module node survived analysis"

let remove_unused =
  let warn_and_false what (ident : Ident.t node) =
    (* if !Configs.Cli.args.verbose then begin
      let name = Ident.to_string ident.item in
      let msg = Printf.sprintf "unused %s `%s`" what name in
      Report.(report (make_warning msg (Some ident.span)))
    end; *)
    ignore (what, ident);
    false
  in

  let f (tl : toplevel node) =
    if tl.span.Span.file <> !Configs.Cli.args.file then
      match tl.item with
      | Ttl_value_def vdef ->
        let key = `Val vdef.vd_name.item in
        if Symbols.get_usages key == [] then
          warn_and_false "value" vdef.vd_name
        else true

      | Ttl_type_def tdef ->
        let key = `Type tdef.td_name.item in
        if Symbols.get_usages key == [] then
          warn_and_false "type" tdef.td_name
        else true

      | _ -> true
    else true
  in
  List.filter f

let typecheck_ast env ast =
  let rec go env = function
    | [] -> env, []
    | tl :: ast ->
      let env, ttl = typecheck_toplevel env tl in
      let env, tast = go env ast in
      env, ttl @ tast
  in
  let tast = snd (go env ast) in
  remove_unused tast
