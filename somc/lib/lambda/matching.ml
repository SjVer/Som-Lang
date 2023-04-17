open Typing.TAst
open Matrix
open Ir

let find_local env v=
  match Env.find env (Ident v) with
    | Var_local var -> var
    | _ -> invalid_arg "lower_binding Tpat_variable"

let lower_literal = function
  | Tli_int i -> Const_int i
  | Tli_char c -> Const_int (Char.code c)
  | Tli_float f -> Const_float f
  | Tli_string s -> Const_string s
  | Tli_null -> Const_nil

(* generating trees *)

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
  | Bind of ident * scrutinee * tree
  | Node of (check * scrutinee * tree) list
  [@@deriving show {with_path = false}]

let collect_signatures _env mat =
  let sigs = ref [] in
  let add_sign matcher check arity =
    let sign = matcher, check, arity in
    if List.exists ((=) sign) !sigs then ()
    else sigs := !sigs @ [sign]
  in

  let get_signature patt =
    match [@ warning "-8"] patt.item with
      | Tpat_literal l ->
        let m = function Tpat_literal _ -> [] in
        add_sign m (Const (lower_literal l)) 0
      (* | Tpat_construct (i, args) ->
        let tag = match Env.find env ident.item with
          | Var_tag tag -> tag
          | _ -> failwith "Tpat_construct ident no tag"
        in
        Some (Tag tag, List.length args) *)
      | Tpat_tuple args ->
        let arity = List.length args in
        let m = function Tpat_tuple args -> args in
        add_sign m Default arity
      | _ -> ()
  in
  List.iter get_signature (first_column mat);
  !sigs

let rec specialize env mat =
  (* specialize a specific signature *)
  let do_spec (matcher, check, arity) =
    let f acc (row, action) =
      try
        let patts = matcher (hd row).item in
        acc @ [patts @ tl row, action]
      with Match_failure _ -> acc
    in
    let rows, actions = 
      List.combine mat.rows mat.actions
      |> List.fold_left f []
      |> List.split
    in

    let scrut = hd mat.occurances in
    let occurances = List.init arity (fun i -> scrut @ [i]) in
    let tree = generate_tree env {occurances; rows; actions} in
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

(* lowering *)

let lower_binding env patt value body =
  match patt.item with
    | Tpat_wildcard -> Expr_sequence (value, body)
    | Tpat_variable v ->
      let var = find_local env v in
      Expr_let (var, value, body)
    | _ -> failwith "lower_binding"
    (* | patt -> compile_match env [(patt, value)] *)

let lower_lambda env patt body =
  match patt.item with
    | Tpat_wildcard ->
      Expr_lambda ([Env.mangle "_"], body)
    | Tpat_variable v ->
      let var = find_local env v in
      Expr_lambda ([var], body)
    | _ ->
      let mat = initial [patt, body] in
      let tree = generate_tree env mat in
      pp_tree Format.std_formatter tree;
      Format.print_newline ();
      ignore (tree);
      failwith "lower_lambda"