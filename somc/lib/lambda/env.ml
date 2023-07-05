open Ir

module Ident = Typing.TAst.Ident
module IMap = Map.Make(Ident)

type t = 
  {
    symbols: var IMap.t;
    arities: int IMap.t;
  }

let empty =
  {
    symbols = IMap.empty;
    arities = IMap.empty;
  }

let add_symbol env ident var =
  {env with symbols = IMap.add ident var env.symbols}

let find env ident = IMap.find ident env.symbols

let mangle =
  let c = ref 0 in
  fun str -> begin
    incr c;
    str ^ "/" ^ string_of_int !c
  end

let mangle_ident i =
  Ident.to_string i |> mangle

let fresh () = mangle "r"

