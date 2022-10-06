module Env = Env
module TAst = Tast
module PrintTAst = Print_tast
module Path = Path

open Parse.Ast
open TAst
open Infer

let mk s i = {span=s; item=i}

let typecheck_tl_node env node =
  let Ast.{span; item : toplevel} = node in match item with
    | TL_Definition {patt; expr} ->
      let env', patt' = infer_patt ~level:0 env patt in
      let expr' = infer_expr env expr in
      unify span patt'.typ expr'.typ;

      let expr'' = set_ty (generalize (-1) expr'.typ) expr' in
      env', mk span (TL_Definition {patt=patt'; expr=expr''}) 
    
    | _ -> failwith "idk"

let typecheck env (ast : ast) =
  let rec go env acc = function
      | hd :: tl ->
        let env', hd' = typecheck_tl_node env hd in
        go env' (acc @ [hd']) tl
      | _ -> env, acc
  in
  let _env', tast = go env [] ast in
  tast