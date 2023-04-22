open Typing.TAst
open Matrix
open Ir

let lower_literal = function
  | Tli_int i -> Const_int i
  | Tli_char c -> Const_int (Char.code c)
  | Tli_float f -> Const_float f
  | Tli_string s -> Const_string s
  | Tli_null -> Const_null

let find_local env v =
  match Env.find env (Ident v) with
    | Var_local var -> var
    | _ -> invalid_arg "lower_binding Tpat_variable"

let rec preprocess_action env scrut patt expr =
  match patt.item with
    | Tpat_literal _ | Tpat_wildcard -> expr

    | Tpat_variable v ->
      let var = find_local env v in
      Expr_let (var, scrut, expr)

    | Tpat_construct (_, args) ->
      let f acc patt = preprocess_action env acc patt expr in
      List.fold_left f scrut args

    | Tpat_tuple patts ->
      let f acc patt = preprocess_action env acc patt expr in
      List.fold_left f scrut patts

let wildcard =
  {
    span = Span.dummy "<file>";
    item = Tpat_wildcard;
    typ = Typing.Types.TNever;
  }

(* generating trees

   sources:
    - http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf
    - https://compiler.club/compiling-pattern-matching/ 
*)

let pp_scrutinee ppf scrut =
  let open Format in
  fprintf ppf "e%a"
    (pp_print_list pp_print_string)
    (List.map (fun s -> "." ^ string_of_int s) scrut)
and pp_expr = Print.print_expr'
and pp_ident = Format.pp_print_string

type check =
  | Default
  | Const of const
  | Tag of int
  [@@deriving show {with_path = false}]

type tree =
  | Fail
  | Leaf of expr
  | Node of (check * scrutinee * tree) list
  [@@deriving show {with_path = false}]

let collect_signatures env mat =
  let sigs = ref [] in
  let comp (_, c1, a1) (_, c2, a2) = c1 = c2 && a1 = a2 in
  let add_sign matcher check arity =
    let sign = matcher, check, arity in
    if List.exists (comp sign) !sigs then ()
    else sigs := !sigs @ [sign]
  in

  let get_signature patt =
    match [@ warning "-8"] patt.item with
      | Tpat_literal l ->
        let m = function Tpat_literal l' when l' = l -> [wildcard] in
        add_sign m (Const (lower_literal l)) 0
      | Tpat_construct (i, args) ->
        let get_tag ident =
          match Env.find env ident.item with
            | Var_tag tag -> tag
            | _ -> failwith "Tpat_construct ident no tag"
        in
        let tag = get_tag i in
        let m = function Tpat_construct (i, args)
          when get_tag i = tag -> args
        in
        add_sign m (Tag tag) (List.length args)
      | Tpat_tuple args ->
        let arity = List.length args in
        let m = function Tpat_tuple args
          when List.length args = arity -> args
        in
        add_sign m Default arity
      | _ -> ()
  in
  List.iter get_signature (first_column mat);
  !sigs

let rec specialize env mat =
  (* get the relevant rows and their actions *)
  let get_rows_and_action matcher arity (row, action) =
    try
      let patts = match (hd row).item with
        | Tpat_wildcard | Tpat_variable _ ->
          List.init arity (fun _ -> wildcard)
        | patt -> matcher patt
      in
      Some (patts @ tl row, action)
    with Match_failure _ -> None
  in

  (* specialize a specific signature *)
  let do_spec (matcher, check, arity) =
    let scrut = hd mat.occurances in
    let occurances =
      if arity <> 0 then
        List.init arity (fun i -> scrut @ [i])
          @ tl mat.occurances
      else
        mat.occurances
    in

    let rows, actions = 
      List.combine mat.rows mat.actions
      |> List.filter_map (get_rows_and_action matcher arity)
      |> List.split
    in

    let mat' = {occurances; rows; actions} in
    (* print mat'; *)
    let tree = generate_tree env mat' in

    check, scrut, tree
  in

  collect_signatures env mat
  |> List.map do_spec

and generate_tree env mat =
  if List.flatten mat.rows = [] then
    (* matrix is empty, so no pattern can be matched *)
    Fail
  
  else if row_is_irrefutable (hd mat.rows) then
    (* first row is irrefutable, so we're done *)
    Leaf (hd mat.actions)
  
  else
    (* swap *)
    let mat = swap_to_refutable_column mat in

    (* specialize *)
    let cases = specialize env mat in

    let def_mat = default_of mat in
    if List.flatten def_mat.rows <> [] then
      let def_tree = generate_tree env def_mat in
      Node (cases @ [Default, hd mat.occurances, def_tree])
    else
      Node cases

(* compiling trees *)

let wrap_in_let var body_fn =
  let r = Env.fresh () in
  Expr_let (r, var, body_fn (Var_local r))

let rec compile_scrutinee expr = function
  | [] -> expr
  | i :: rest ->
    wrap_in_let expr (fun expr -> 
      let expr = Expr_get (expr, i) in
      compile_scrutinee expr rest)

let compile_check scrut = function
  | Default -> Expr_atom (Atom_const (Const_int 1))
  | Const c ->
    let magic = Atom_magic Symbols.Magic.Magic_eq in
    Expr_call (magic, [Atom_const c; scrut])
  | Tag t ->
    let magic = Atom_magic Symbols.Magic.Magic_tageq in
    Expr_call (magic, [Atom_const (Const_int t); scrut])

let rec compile_tree expr = function
  | Fail -> Expr_fail
  | Leaf action -> action
  | Node cases ->
    let rec f = function
      | (check, scrut, tree) :: rest ->
        let scrut' = compile_scrutinee expr scrut in
        let tree' = compile_tree expr tree in
        if check <> Default then
          wrap_in_let scrut' (fun scrut' ->
            let cond = compile_check (Atom_var scrut') check in
            let rest' = f rest in
            Expr_if (cond, tree', rest'))
        else
          tree'

      | [] -> Expr_fail
    in
    f cases

(* lowering *)

let lower_cases env scrut cases =
  let f (env, patt, act) =
    patt, preprocess_action env scrut patt act
  in
  let cases = List.map f cases in

  let mat = initial cases in
  let tree = generate_tree env mat in
  compile_tree scrut tree

let lower_binding env patt value body =
  match patt.item with
    | Tpat_wildcard -> Expr_sequence (value, body)
    | Tpat_variable v ->
      let var = find_local env v in
      Expr_let (var, value, body)
    | _ ->
      failwith "lower_binding"

let lower_lambda env patt body =
  match patt.item with
    | Tpat_wildcard ->
      Expr_lambda ([Env.mangle "_"], body)
    | Tpat_variable v ->
      let var = find_local env v in
      Expr_lambda ([var], body)
    | _ ->
      failwith "lower_lambda"
