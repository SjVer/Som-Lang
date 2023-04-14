open Typing.TAst
open Ir

let (@.) list i = List.nth list i
let hd, tl = List.hd, List.tl


type scrutinee = extract list

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

let is_irrefutable = function
  | Tpat_wildcard | Tpat_variable _ -> true
  | _ -> false

let row_is_irrefutable =
  Array.for_all is_irrefutable

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



