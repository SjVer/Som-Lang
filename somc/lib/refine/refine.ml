module Import = Import

open Parse.Ast

let desugar =
  let rec go acc = function
    | n :: ns -> begin
        try
          let i = match n.item with
            | TL_Import i ->
              Import.desugar i
            | _ -> n.item
          in
          go (acc @ [{n with item=i}]) ns
        with Report.Error.Error (e, s, n) ->
          Report.report e s n;
          go acc ns
      end
    | [] -> acc
  in go []