open Lsp
open Text_document
open Somc

(* types *)

exception Exit = Report.Exit

type status =
  | Typechecked of Parse.Ast.ast * Typing.TAst.tast
  | Analyzed of Parse.Ast.ast
  | Parsed of Parse.Ast.ast
  | Opened

type entry =
  {
    mutable doc: Text_document.t;
    mutable status: status;
  }

type t = (Uri.t, entry) Hashtbl.t

(* functions *)

let create () : t = Hashtbl.create 10

let get (s: t) uri = Hashtbl.find s uri

let get_doc s uri = (get s uri).doc
let set_doc s uri doc =
  let e = get s uri in
  e.doc <- doc;
  e.status <- Opened

let get_status s uri = (get s uri).status
let set_status s uri status =
  (get s uri).status <- status

let add (s: t) doc =
  let uri = documentUri doc in
  Hashtbl.add s uri {doc; status = Opened};
  doc

let close (s: t) uri = Hashtbl.remove s uri

(* We're not using the pipeline because file contents
   can change while the server is running. So instead
   we manually keep track of document status and reset
   it when a document is changed.
   (on textDocument/didChange notification) *)

let parse s uri =
  match get_status s uri with
    | Typechecked (a, _) -> a
    | Analyzed a -> a
    | Parsed a -> a
    | _ ->
      let doc = get_doc s uri in
      let path = Uri.to_path (documentUri doc) in
      try
        let ast = Parse.parse path (text doc) None in
        set_status s uri (Parsed ast);
        ast
      with Exit _ -> []

let analyse s uri =
  match get_status s uri with
    | Typechecked (a, _) -> a
    | Analyzed a -> a
    | _ ->
      let ast = parse s uri in
      try
        let ast' = Analysis.check ast in
        set_status s uri (Analyzed ast');
        ast
      with Exit _ -> []
      
let check s uri =
  match get_status s uri with
    | Typechecked ( _, a) -> a
    | _ ->
      let ast = parse s uri in
      try
        let ast' = Analysis.add_implicit_import_prelude ast in
        let _, tast = Typing.typecheck Typing.Env.empty ast' in
        set_status s uri (Typechecked (ast', tast));
        tast
      with Exit _ -> []