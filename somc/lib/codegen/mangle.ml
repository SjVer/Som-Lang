open Typing.Types
open Symbols.Ident

(*
  Identifier mangling:
    foo      -> "foo"
    foo'     -> "foo."
    foo::bar -> "foo..bar"
*)

let mangle_str = String.map (fun c -> if c = '\'' then '.' else c)

let rec mangle_ident = function
  | Ident i -> mangle_str i
  | Cons (hd, tl) -> mangle_str hd ^ ".." ^ mangle_ident tl

(*
  Type mangling:
    $i.u.32     -> "P$iu32"
    Flt         -> "Flt"
    Int!        -> "E$Int"
    Str List    -> "A$Str$List$."
    Int -> Flt  -> "F$Int$Flt$."
    (Chr, Bln)  -> "T$Chr$Bln$."
    <never>     -> "N$"
*)

let mangle_prim_type = function
  | PInt (s, w) -> "i" ^ (if s then "s" else "u") ^ string_of_int w
  | PFloat w -> "f" ^ string_of_int w
  | PVoid -> "v"

let rec mangle_type = function
  | TName p -> mangle_ident p
  | TPrim p -> "P$" ^ mangle_prim_type p
  | TEff t -> "E$" ^ mangle_type t
  | TApp (a, t) -> "A$" ^ mangle_type a ^ "$" ^ mangle_type t ^ "$."
  | TFun (a, r) -> "F$" ^ mangle_type a ^ "$" ^ mangle_type r ^ "$."
  | TTup ts -> "T$" ^ (String.concat "$" (List.map mangle_type ts)) ^ "$."
  | TNever -> "N$"
  | _ -> failwith "Mangle.mangle_type"

(* 
  Symbol mangling:
    foo: Int -> Float   -> "_Sfoo$$F$Int$Flt$."
*)

let mangle_symbol ident typ =
  "_S$" ^ mangle_ident ident ^ "$$" ^ mangle_type typ
