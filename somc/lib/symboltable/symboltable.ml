module Ident = Ident

module IMap = Map.Make(Ident)

type 'a entry =
  {
    symbol: 'a;
    original_ident: Ident.t;
    usages: Span.t list;
  }

type ('v, 't) t =
  {
    values: 'v entry IMap.t;
    types: 't entry IMap.t;
  }

type ('v, 't) symbol_table = ('v, 't) t