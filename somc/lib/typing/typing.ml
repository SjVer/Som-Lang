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
    | TL_Value_Definition vdef ->
      (* there's no pattern so just type the bound expr *)
      let expr = infer_expr env vdef.vd_expr in
      let expr' = set_ty (generalize (-1) expr.typ) expr in

      (* construct the updated env and typed vdef *)
      let env' = Env.add_value env vdef.vd_name.item expr'.typ in
      let vdef' =
        {
          vd_name = vdef.vd_name;
          vd_expr = expr';
        }
      in
      env', [mk (TL_Value_Definition vdef')]
  
    | TL_Type_Definition tdef ->
      let t = Parse_type.parse env 0 tdef.td_type.item in
      let env' = Env.add_alias env tdef.td_name.item t in
      
      (* keep the tdef for backend purposes *)
      let tdef' =
        {
          td_name = tdef.td_name;
          td_type = {span = tdef.td_type.span; item = t};
        }
      in
      env', [mk (TL_Type_Definition tdef')]

    | TL_Import _ -> failwith "Import node survived analysis"
    | TL_Module _ -> failwith "Module node survived analysis"

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
