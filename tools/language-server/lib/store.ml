open Lsp
open Somc

(* types *)

exception Exit = Somc.Report.Exit

type status =
  | Typechecked of string * Parse.Ast.ast * Typing.TAst.tast
  | Analyzed of string * Parse.Ast.ast
  | Parsed of string * Parse.Ast.ast
  | Opened of string
  | Unopened

type t = (Uri.t, status) Hashtbl.t

(* functions *)

let create () : t = Hashtbl.create 10
let get s uri = Hashtbl.find_opt s uri

(* We're calling [Query.force] instead of [Query.call]
   because file contents can change while the server
   is running. So instead we manually keep track of
   document status and reset it when a document is
   changed. (on textDocument/didChange notification) *)

let add s uri = Hashtbl.replace s uri Unopened

let read s uri = match get s uri with
  | Some (Typechecked (c, _, _)) -> c
  | Some (Analyzed (c, _)) -> c
  | Some (Parsed (c, _)) -> c
  | Some (Opened c) -> c
  | _ ->
    let path = Uri.to_path uri in
    let c = Pipeline.ReadFile.force path in
    Hashtbl.replace s uri (Opened c);
    c

let parse s uri = match get s uri with
  | Some (Typechecked (_, a, _)) -> a
  | Some (Analyzed (_, a)) -> a
  | Some (Parsed (_, a)) -> a
  | _ ->
    let path = Uri.to_path uri in
    let c = read s uri in
    try
      let ast = Pipeline.ParseFile.force (path, false) in
      Hashtbl.replace s uri (Parsed (c, ast));
      ast
    with Exit _ -> []

let analyse s uri = match get s uri with
  | Some (Typechecked (_, a, _)) -> a
  | Some (Analyzed (_, a)) -> a
  | _ ->
    let path = Uri.to_path uri in
    let c = read s uri in
    try
      let ast = Pipeline.AnalyzeFile.force (path, false) in
      Hashtbl.replace s uri (Analyzed (c, ast));
      ast
    with Exit _ -> []
    
let check s uri = match get s uri with
  | Some (Typechecked (_, _, a)) -> a
  | _ ->
    let module Log = (val Logs.src_log (Logs.Src.create __MODULE__)) in
    Log.info (fun f -> f "checking %s" (Uri.to_path uri));
    let path = Uri.to_path uri in
    let c = read s uri in
    let ast = analyse s uri in
    try
      let tast = Pipeline.TypecheckFile.force path in
      Hashtbl.replace s uri (Typechecked (c, ast, tast));
      tast
    with Exit _ -> []