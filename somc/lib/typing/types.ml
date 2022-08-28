type type_expr =
  {
    desc: type_desc;
  }

and type_desc =
  | TVar of string option
  | TFun of type_expr * type_expr
  | TTuple of type_expr list
  | TConstr of Path.t * type_expr list
  | TNil
  | TVariant of (string * type_expr) list