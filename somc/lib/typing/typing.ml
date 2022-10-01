module TAst = Tast
module PrintTAst = Print_tast

open Parse.Ast

let typecheck ast =
  List.iter (fun {span=_; item} -> match item with
    | TL_Definition {patt=_; expr} ->
      ignore (Infer.infer_expr Env.empty expr)
      (* let texpr = Infer.infer_expr Env.empty expr in *)
      (* Print_tast.print_expr_node texpr; *)
      (* print_endline (Types.show_type texpr.typ) *)
    | _ -> ()
  ) ast