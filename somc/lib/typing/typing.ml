module Env = Env
module TAst = Tast
module PrintTAst = Print_tast
module Path = Path

open Parse.Ast
open TAst
open Infer

let mk s i = {span=s; item=i}

let rec parse_type =
  let open Parse.Ast in function
  | TY_Grouping t -> parse_type t.item
  | TY_Any -> Infer.new_var 0
  | TY_Variable _ -> Infer.new_gen_var ()
  | TY_Effect (Some t) -> TEff (parse_type t.item)
  | TY_Effect None -> TEff (TPrim PVoid)
  | TY_Function (a, r) ->
    TFun (parse_type a.item, parse_type r.item)
  | TY_Tuple ts -> 
    let map = List.map (fun t -> parse_type t.item) in
    TTup (map ts)
  | TY_Builtin b -> begin
      let open Types in
      let p = match b with
        | BT_Int (s, w) -> PInt (s, w)
        | BT_Float w -> PFloat w
        | BT_Void -> PVoid
      in
      TPrim p
    end
  | TY_Construct (None, t) -> TName (Path.from_ident t.item)
  | _ -> assert false

let rec typecheck_tl_node env node =
  let Ast.{span; item : toplevel} = node in match item with
    | TL_Declaration (n, t) ->
      let t' = parse_type t.item in
      Env.extend_var env n t', mk span (TL_Declaration (n, t'))

    | TL_Definition {patt; expr} ->
      (* TODO: check and unify with declarations *)

      let env', patt' = infer_patt ~level:0 env patt in
      let expr' = infer_expr env expr in
      unify span patt'.typ expr'.typ;

      let expr'' = set_ty (generalize (-1) expr'.typ) expr' in
      env', mk span (TL_Definition {patt=patt'; expr=expr''}) 
    
    | TL_Section (n, tls) ->
      let tls' = typecheck env tls in
      (* TODO: extend env with section n *)
      env, mk span (TL_Section (n, tls'))

    | TL_Link (_, tl) ->
      let env', tl' = typecheck_tl_node env tl in
      (* TODO: bind to name? *)
      env', tl'

    | _ -> failwith "typecheck_tl_node"

and typecheck env (ast : ast) =
  let rec go env acc = function
      | hd :: tl ->
        let env', hd' = typecheck_tl_node env hd in
        go env' (acc @ [hd']) tl
      | _ -> env, acc
  in
  let _env', tast = go env [] ast in
  tast