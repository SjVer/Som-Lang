open Ir

module Ident = Typing.TAst.Ident
module IMap = Map.Make(Ident)

type t = var IMap.t

let empty = IMap.empty

let add = IMap.add

let bind_global vars ident var = IMap.add ident (Var_global var) vars
let bind_local vars ident var = IMap.add ident (Var_local var) vars

let find env ident = IMap.find ident env

let mangle =
  let c = ref 0 in
  fun str -> begin
    incr c;
    str ^ "/" ^ string_of_int !c
  end

let mangle_ident i = Ident.to_string i |> mangle

let fresh () = mangle "r"

