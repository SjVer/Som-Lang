open Lsp.Types
open Somc.Parse.Ast
open Somc.Span

module T = struct
  let module_ = 0
  let type_ = 1
  let class_ = 2
  let enum = 3
  let module_type = 4
  let record = 5
  let type_parameter = 6
  let parameter = 7
  let variable = 8
  let property = 9
  let enum_member = 10
  let event = 11
  let function_ = 12
  let method_ = 13
  let macro = 14
  let keyword = 15
  let modifier = 16
  let comment = 17
  let string = 18
  let number = 19
  let regexp = 20
  let operator = 21
  let decorator = 22

  let list =
    [ "namespace"
    ; "type"
    ; "class"
    ; "enum"
    ; "interface"
    ; "struct"
    ; "typeParameter"
    ; "parameter"
    ; "variable"
    ; "property"
    ; "enumMember"
    ; "event"
    ; "function"
    ; "method"
    ; "macro"
    ; "keyword"
    ; "modifier"
    ; "comment"
    ; "string"
    ; "number"
    ; "regexp"
    ; "operator"
    ; "decorator"
    ]
end

module M = struct
  let empty = 0

  let declaration = 1 lsl 0
  let definition = 1 lsl 1
  let readonly = 1 lsl 2
  let static = 1 lsl 3
  let deprecated = 1 lsl 4
  let abstract = 1 lsl 5
  let async = 1 lsl 6
  let modification = 1 lsl 7
  let documentation = 1 lsl 8
  let default_library = 1 lsl 9

  let union = ( lor )
  let encode = List.fold_left union 0

  let list =
    [ "declaration"
    ; "definition"
    ; "readonly"
    ; "static"
    ; "deprecated"
    ; "abstract"
    ; "async"
    ; "modification"
    ; "documentation"
    ; "defaultLibrary"
    ]

  let array = lazy (Array.of_list list)

  let to_legend =
    let cache = lazy (Hashtbl.create 3) in
    fun t ->
      let cache = Lazy.force cache in
      match Hashtbl.find_opt cache t with
      | Some x -> x
      | None ->
        let rec translate t i acc : string list =
          let is_set = Int.equal (t land 1) 1 in
          let t' = t lsr 1 in
          let acc' = if is_set then (Lazy.force array).(i) :: acc else acc in
          if Int.equal t' 0 then List.rev acc' else translate (i + 1) t' acc'
        in
        let res = translate t 0 [] in
        Hashtbl.add cache t res;
        res
end

let legend =
  SemanticTokensLegend.create
    ~tokenTypes:T.list
    ~tokenModifiers:M.list

type token =
  {
    type_: int;
    modifiers: int;
    line: int;
    col: int;
    length: int;
  }

(** Head of list is last token! *)
let encode_tokens tokens =
  let len = List.length tokens in
  let data = Array.init (len * 5) (fun _ -> 0) in

  let encode i t ldelta sdelta =
    data.(i) <- ldelta;
    data.(i + 1) <- sdelta;
    data.(i + 2) <- t.length;
    data.(i + 3) <- t.type_;
    data.(i + 4) <- t.modifiers;
  in

  let rec go i = function
    | [] -> ()
    | [t] -> encode 0 t t.line t.col
    | ct :: pt :: ts ->
      let ldelta = ct.line - pt.line in
      let sdelta =
        if ldelta = 0 then ct.col - pt.col
        else ct.col
      in
      encode (i * 5) ct ldelta sdelta;
      go (i - 1) (pt :: ts)
  in
  go (len - 1) tokens;
  Array.to_list data

let mk span type_ modifiers =
  {
    type_;
    modifiers = M.encode modifiers;
    line = span.start.line - 1;
    col = span.start.col - 1;
    length = span_length span;
  }

let get_tokens ast =
  let rec go_type node =
    let this = mk node.span in
    match node.item with
      | TY_Grouping g -> go_type g
      | TY_Any -> [this T.type_parameter []]
      | TY_Variable _ -> [this T.type_parameter []]
      | TY_Effect t ->
        let end' = {
            node.span.start with
            col = node.span.start.col + 1;
            offset = node.span.start.offset + 1
          }
        in
        let span' = {node.span with end_=end'} in
        let this' = mk span' T.keyword M.[async; modification] in
        begin match t with
          | Some t -> go_type t @ [this']
          | None -> [this']
        end 
      | TY_Function (t1, t2) -> go_type t2 @ go_type t1
      | TY_Tuple ts -> List.flatten (List.map go_type ts)
      | TY_Construct (t1, t2) -> begin
          let t2' = mk t2.span T.class_ [] in
          match t1 with
            | Some t -> t2' :: go_type t
            | None -> [t2']
        end
      | TY_Builtin _ -> [this T.type_ M.[default_library]]
      | _ -> []
  in
  let rec go_patt node =
    match node.item with
      | _ -> [mk node.span T.function_ []]
  in
  let rec go_expr node =
    match node.item with
      | EX_Grouping g -> go_expr g
      | EX_Binding (b, e) ->
        go_expr e @ go_expr b.expr @ go_patt b.patt
      | _ -> [mk node.span T.number []]
  in
  let rec go_toplevel node =
    match node.item with
      | TL_Declaration (n, t) ->
        let n' = mk n.span T.function_ M.[declaration] in
        go_type t @ [n']
      | TL_Definition {patt; expr} ->
        go_expr expr @ go_patt patt
      | _ -> []
  and go_ast = function
    | [] -> []
    | tl :: tls ->
      go_toplevel tl @ go_ast tls
  in go_ast ast