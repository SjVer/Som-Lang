open Parse.Ast
open Parse.Ident

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> a * pow a (n - 1)

let fold_app f es =
  let exception Cant_fold in
  try match f.item, nmapi es with
    | EX_Identifier {span=_; item=Ident i},
      EX_Literal a :: EX_Literal b :: [] -> begin
        let res = match i, a, b with
          | "+", LI_Int a, LI_Int b -> LI_Int (a + b)
          | "+", LI_Float a, LI_Float b -> LI_Float (a +. b)
          | "-", LI_Int a, LI_Int b -> LI_Int (a - b)
          | "-", LI_Float a, LI_Float b -> LI_Float (a -. b)
          | "*", LI_Int a, LI_Int b -> LI_Int (a * b)
          | "*", LI_Float a, LI_Float b -> LI_Float (a *. b)
          | "/", LI_Int a, LI_Int b -> LI_Int (a / b)
          | "/", LI_Float a, LI_Float b -> LI_Float (a /. b)
          | "^", LI_Int a, LI_Int b -> LI_Int (pow a b)
          | "%", LI_Int a, LI_Int b -> LI_Int (a mod b)
          | _ -> raise Cant_fold
        in
        true, EX_Literal res
      end
    | EX_Identifier {span=_; item=Ident i},
      EX_Literal a :: [] -> begin
        let res = match i, a with
          | "~+", LI_Int a -> LI_Int (abs a)
          | "~+", LI_Float a -> LI_Float (abs_float a)
          | "~-", LI_Int a -> LI_Int (- a)
          | "~-", LI_Float a -> LI_Float (-. a)
          | _ -> raise Cant_fold
        in
        true, EX_Literal res
      end
    | _ -> raise Cant_fold
  with Cant_fold -> false, EX_Application (f, es)

let rec fold_expr e =
  let folded, item = match e.item with
    | EX_Application (f, es) ->
      let f' = fold_expr f in
      let es' = List.map fold_expr es in
      fold_app f' es'
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
      | TL_Definition b ->
        TL_Definition {b with expr = fold_expr b.expr}
      | TL_Module (n, ast) ->
        TL_Module (n, fold_constants ast)
      | _ as tl -> tl
    in
    {tl with item}
  in
  List.map go ast