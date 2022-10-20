open Lsp
open Somc

(* types *)

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

let add s uri = Hashtbl.add s uri Unopened

let read s uri = match get s uri with
  | Some (Typechecked (c, _, _)) -> c
  | Some (Analyzed (c, _)) -> c
  | Some (Parsed (c, _)) -> c
  | Some (Opened c) -> c
  | _ ->
    let path = Uri.to_path uri in
    let c = Pipeline.ReadFile.call path in
    Hashtbl.replace s uri (Opened c);
    c

let parse s uri = match get s uri with
  | Some (Typechecked (_, a, _)) -> a
  | Some (Analyzed (_, a)) -> a
  | Some (Parsed (_, a)) -> a
  | _ ->
    let path = Uri.to_path uri in
    let c = read s uri in
    let ast = Pipeline.ParseFile.call (path, false) in
    Hashtbl.replace s uri (Parsed (c, ast));
    ast

let analyse s uri = match get s uri with
  | Some (Typechecked (_, a, _)) -> a
  | Some (Analyzed (_, a)) -> a
  | _ ->
    let path = Uri.to_path uri in
    let c = read s uri in
    let ast = Pipeline.AnalyzeFile.call (path, false) in
    Hashtbl.replace s uri (Analyzed (c, ast));
    ast
    
let check s uri = match get s uri with
  | Some (Typechecked (_, _, a)) -> a
  | _ ->
    let path = Uri.to_path uri in
    let c = read s uri in
    let ast = Pipeline.AnalyzeFile.call (path, false) in
    let tast = Pipeline.TypecheckFile.call path in
    Hashtbl.replace s uri (Typechecked (c, ast, tast));
    tast
    