module Env = Env
module TAst = Tast
module PrintTAst = Print_tast
module Types = Types
module Path = Path

open Parse.Ast
open TAst
open Infer

let mk s i = Some {span=s; item=i}

let check_declaration oldenv newenv s =
  let newenv' = ref newenv in
  let new_symbols = (Env.diff oldenv newenv).symbols in
  let find n = Env.SMap.find n oldenv.symbols in

  (* checks if a declaration for [n] exists and whatnot *)
  let f n t =
    try
      let dt = find (n ^ "\\decl") in
      (* try *)
        let t' = instantiate 0 t in
        unify s dt t';
        newenv' := Env.add_symbol !newenv' n dt

      (* with Report.Error r ->
          let note = Printf.sprintf
            "'%s' is declared with type `%s`"
            n (Types.show dt false)
          in
          report (Report.add_note note r) *)

    with Not_found -> ()
  in

  (* execute per newly added symbol *)
  Env.SMap.iter f new_symbols;
  !newenv'

(* TODO:
    checking defs with decls is kinda buggy as rn we
    figure out which names to check by checking the
    difference between the env before- and after the
    definition, so any redefinitions will not have their
    type checked against any declarations.
*)

let rec typecheck_tl_node env node =
  let Parse.Ast.{span; item : toplevel} = node in
  match item with
    | TL_Declaration (n, t) ->
      let t' = Parse_type.parse env 0 t.item in
      let env' = Env.add_symbol env (n.item ^ "\\decl") t' in
      let tnode = {span = t.span; item=t'} in
      env', mk span (TL_Declaration (n, tnode))

    | TL_Definition {patt; expr} ->
      let env', patt' = infer_patt ~level:0 env patt in
      let expr' = infer_expr env expr in

      unify span patt'.typ expr'.typ;
      let env'' = check_declaration env env' expr.span in
      
      let expr'' = set_ty (generalize (-1) expr'.typ) expr' in
      env'', mk span (TL_Definition {patt=patt'; expr=expr''}) 
  
    | TL_Type_Definition {name; params=_; typ} ->
      let t = Parse_type.parse env 0 typ.item in
      (* TODO: params *)
      Env.add_alias env name.item t, None

    | TL_Import _ -> failwith "Import node survived analysis"

    | TL_Section (n, tls) ->
      let env', tls' = typecheck env tls in
      let env'' = Env.add_section env' n.item env' in
      env'', mk span (TL_Section (n, tls'))

    | TL_Link (_, tl) ->
      let env', tl' = typecheck_tl_node env tl in
      (* TODO: bind to name? *)
      env', tl'

and typecheck env (ast : ast) =
  let rec go env acc = function
      | hd :: tl ->
        let env', hd' = typecheck_tl_node env hd in
        begin match hd' with
          | Some hd' -> go env' (acc @ [hd']) tl
          | None -> go env' acc tl
        end
      | _ -> env, acc
  in
  let env', tast = go env [] ast in
  env', tast