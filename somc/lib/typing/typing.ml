module Env = Env
module TAst = Tast
module PrintTAst = Print_tast
module Types = Types

open Parse.Ast
open TAst
open Infer
open Unify

let typecheck_toplevel env node =
  let mk item = {span = node.Parse.Ast.span; item} in
  match (node.item : Parse.Ast.toplevel) with
    | TLValueDef vdef ->
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
      env, [mk (TLValueDef vdef')]
  
    | TLTypeDef tdef ->
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
      env, [mk (TLTypeDef tdef')]

    | TLExternDef edef ->
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
      env, [mk (TLExternDef edef')]

    | TLImport _ -> failwith "Import node survived analysis"
    | TLModule _ -> failwith "Module node survived analysis"

let typecheck_ast env ast =
  let rec go env = function
    | [] -> env, []
    | tl :: ast ->
      let env, ttl = typecheck_toplevel env tl in
      let env, tast = go env ast in
      env, ttl @ tast
  in
  let tast = snd (go env ast) in
  if !Report.has_reported then Report.exit ()
  else tast
