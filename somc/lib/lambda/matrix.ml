open Typing.TAst
open Ir

let (@.) list i = List.nth list i
let hd, tl = List.hd, List.tl


type matrix =
  {
    occurances: scrutinee list;
    rows: pattern tnode list list;
    actions: expr list;
  }


let initial cases =
  let patts, acts = List.split cases in
  {
    occurances = [[]];
    rows = List.map (fun p -> [p]) patts;
    actions = acts;
  }

let first_column mat = List.map (fun row -> hd row) mat.rows

let is_irrefutable = function
  | Tpat_wildcard | Tpat_variable _ -> true
  | _ -> false

let row_is_irrefutable patts =
  List.map (fun n -> n.item) patts
  |> List.for_all is_irrefutable

let default_of mat =
  let rows, actions =
    let map i row =
      if is_irrefutable (hd row).item then
        Some (row, mat.actions @. i)
      else None
    in
    List.mapi map mat.rows
    |> List.filter_map (fun x -> x)
    |> List.split 
  in
  {
    occurances = mat.occurances;
    rows; actions;
  }

let swap_to_refutable_column mat =
  let column =
    let rec find_column i row =
      if row = [] then None
      else if not (is_irrefutable (hd row).item)
        then Some i
        else find_column (i + 1) (tl row)
    in
    List.map (find_column 0) mat.rows
    |> List.filter_map (fun x -> x)
    |> List.hd
  in
  let swap list =
    let x0 = List.hd list in
    let xi = List.nth list column in
    let f c x' i x = if i = c then x' else x in
    List.mapi (f 0 xi) list 
    |> List.mapi (f column x0)
  in
  {
    occurances = swap mat.occurances;
    rows = List.map swap mat.rows;
    actions = mat.actions;
  }

let print mat =
  let module B = PrintBox in
  let open Format in
  let occ =
    let f scrut =
      let s =
        List.map (fun s -> "." ^ string_of_int s) scrut
        |> String.concat ""
      in
      B.text ("e" ^ s)
      |> B.center_h
      |> B.pad' ~lines:0 ~col:1
    in
    List.map f mat.occurances
    |> Array.of_list
  in
  let mat' =
    let rows =
      let fold_row i =
        let f patt =
          Span.show_span patt.span
          |> B.text
          |> B.center_h
          |> B.pad' ~lines:0 ~col:1
        in
        List.nth mat.rows i
        |> List.map f
        |> Array.of_list
      in
      Array.init (List.length mat.rows) fold_row
      |> Array.append (Array.make 1 occ)
    in
    B.grid rows
  in
  let act =
    let f expr =
      ignore (flush_str_formatter ());
      Print.print_expr' str_formatter expr;
      flush_str_formatter ()
      |> B.text
      |> B.center_h
      |> B.pad' ~lines:0 ~col:1
    in
    B.text " " :: List.map f mat.actions
  in

  B.frame (B.hlist [mat'; B.vlist act])
  |> PrintBox_text.output stdout;
  print_newline ()

