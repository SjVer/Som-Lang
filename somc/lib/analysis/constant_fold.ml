open Parse.Ast
open Symbols.Ident

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> a * pow a (n - 1)

let fold_app f es =
  let exception Cant_fold in
  try match f.item, nmapi es with
    | Pexp_ident {span=_; item=Ident i},
      [Pexp_literal a; Pexp_literal b] -> begin
        let res = match i, a, b with
          | "+", Pli_int a, Pli_int b -> Pli_int (a + b)
          | "+", Pli_float a, Pli_float b -> Pli_float (a +. b)
          | "-", Pli_int a, Pli_int b -> Pli_int (a - b)
          | "-", Pli_float a, Pli_float b -> Pli_float (a -. b)
          | "*", Pli_int a, Pli_int b -> Pli_int (a * b)
          | "*", Pli_float a, Pli_float b -> Pli_float (a *. b)
          | "/", Pli_int a, Pli_int b -> Pli_int (a / b)
          | "/", Pli_float a, Pli_float b -> Pli_float (a /. b)
          | "^", Pli_int a, Pli_int b -> Pli_int (pow a b)
          | "%", Pli_int a, Pli_int b -> Pli_int (a mod b)
          | _ -> raise Cant_fold
        in
        true, Pexp_literal res
      end
    | Pexp_ident {span=_; item=Ident i},
      [Pexp_literal a] -> begin
        let res = match i, a with
          | "~+", Pli_int a -> Pli_int (abs a)
          | "~+", Pli_float a -> Pli_float (abs_float a)
          | "~-", Pli_int a -> Pli_int (- a)
          | "~-", Pli_float a -> Pli_float (-. a)
          | _ -> raise Cant_fold
        in
        true, Pexp_literal res
      end
    | _ -> raise Cant_fold
  with Cant_fold -> false, Pexp_apply (f, es)

let rec fold_expr e =
  let folded, item = match e.item with
    | Pexp_grouping e -> false, Pexp_grouping (fold_expr e)
    (*
    | Pexp_binding
    | Pexp_lambda
    *)
    | Pexp_sequence (e1, e2) -> false, Pexp_sequence (fold_expr e1, fold_expr e2)
    | Pexp_constraint (e, t) -> false, Pexp_constraint (fold_expr e, t)
    | Pexp_apply (f, es) ->
      let f' = fold_expr f in
      let es' = List.map fold_expr es in
      fold_app f' es'
    | Pexp_tuple es -> false, Pexp_tuple (List.map fold_expr es)
    | Pexp_construct (i, es) -> false, Pexp_construct (i, List.map fold_expr es)
    | _ as e -> false, e
  in
  let ghost = e.span.ghost || folded in
  {
    span = {e.span with ghost};
    item;
  }

let rec fold_constants (ast : ast) : ast =
  let go tl =
    let item = match tl.item with
      | Ptl_value_def b ->
        Ptl_value_def {b with vd_expr = fold_expr b.vd_expr}
      | Ptl_module (n, ast) ->
        Ptl_module (n, fold_constants ast)
      | _ as tl -> tl
    in
    {tl with item}
  in
  List.map go ast