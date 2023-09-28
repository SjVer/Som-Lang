open Lambda.Ir

type ctype =
  | Ctype_value
  | Ctype_long
  | Ctype_val_pointer
  | Ctype_fun_pointer of int
  | Ctype_other of string

type cexpr =
  | Cexpr_atom of atom
  | Cexpr_prim of prim * atom list
  | Cexpr_call of ident * atom list

type cstmt =
  | Cstmt_expr of cexpr
  | Cstmt_assign of ident * cexpr
  | Cstmt_return of cexpr

type cblock = cstmt list

type cdecl =
  | Cdecl_global of ident * cexpr
  | Cdecl_function of ident * ident list * cblock
  | Cdecl_external of ident * int

type cmodule = 
  {
    mutable decls: cdecl list;
    mutable builtins: (ident * ctype list) list;
  }

let empty_cmodule () =
  {
    decls = [];
    builtins = [];
  }

let add_decl m decl =
  m.decls <- m.decls @ [decl]

let add_builtin m name args =
  m.builtins <- m.builtins @ [name, args]

let lookup_builtin m name =
  List.assoc_opt name m.builtins