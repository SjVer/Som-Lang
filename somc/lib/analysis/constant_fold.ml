open Parse.Ast
open Symbols.Ident

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> a * pow a (n - 1)

let fold_app f es =
  let exception Cant_fold in
  try match f.item, nmapi es with
    | EXIdentifier {span=_; item=Ident i},
      [EXLiteral a; EXLiteral b] -> begin
        let res = match i, a, b with
          | "+", LIInt a, LIInt b -> LIInt (a + b)
          | "+", LIFloat a, LIFloat b -> LIFloat (a +. b)
          | "-", LIInt a, LIInt b -> LIInt (a - b)
          | "-", LIFloat a, LIFloat b -> LIFloat (a -. b)
          | "*", LIInt a, LIInt b -> LIInt (a * b)
          | "*", LIFloat a, LIFloat b -> LIFloat (a *. b)
          | "/", LIInt a, LIInt b -> LIInt (a / b)
          | "/", LIFloat a, LIFloat b -> LIFloat (a /. b)
          | "^", LIInt a, LIInt b -> LIInt (pow a b)
          | "%", LIInt a, LIInt b -> LIInt (a mod b)
          | _ -> raise Cant_fold
        in
        true, EXLiteral res
      end
    | EXIdentifier {span=_; item=Ident i},
      [EXLiteral a] -> begin
        let res = match i, a with
          | "~+", LIInt a -> LIInt (abs a)
          | "~+", LIFloat a -> LIFloat (abs_float a)
          | "~-", LIInt a -> LIInt (- a)
          | "~-", LIFloat a -> LIFloat (-. a)
          | _ -> raise Cant_fold
        in
        true, EXLiteral res
      end
    | _ -> raise Cant_fold
  with Cant_fold -> false, EXApplication (f, es)

let rec fold_expr e =
  let folded, item = match e.item with
    | EXGrouping e -> false, EXGrouping (fold_expr e)
    (*
    | EXBinding
    | EXLambda
    *)
    | EXSequence (e1, e2) -> false, EXSequence (fold_expr e1, fold_expr e2)
    | EXConstraint (e, t) -> false, EXConstraint (fold_expr e, t)
    | EXApplication (f, es) ->
      let f' = fold_expr f in
      let es' = List.map fold_expr es in
      fold_app f' es'
    | EXTuple es -> false, EXTuple (List.map fold_expr es)
    | EXConstruct (i, es) -> false, EXConstruct (i, List.map fold_expr es)
    | _ as e -> false, e
  in
  let ghost = e.span.ghost || folded in
  {
    span = {e.span with ghost};
    item;
  }

let rec fold_constants ast =
  let go tl =
    let item = match tl.item with
      | TLValueDef b ->
        TLValueDef {b with vd_expr = fold_expr b.vd_expr}
      | TLModule (n, ast) ->
        TLModule (n, fold_constants ast)
      | _ as tl -> tl
    in
    {tl with item}
  in
  List.map go ast