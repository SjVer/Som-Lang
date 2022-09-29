module ParseFileQuery = Query.Make(struct
  type a = string
  type r = Parse.Ast.ast
  let c f = Somc.Parse.parse f
end)

module TypecheckFileQuery = Query.Make(struct
  type a = string
  type r = unit
  let c f =
    let ast = ParseFileQuery.call f in
    Somc.Typing.typecheck ast
end)