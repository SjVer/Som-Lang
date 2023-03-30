open Ast
open ANSITerminal

let p i str span =
  print_string [] (String.make i '\t' ^ str);
  print_string [Foreground Black] (" @" ^ Span.show_span_debug span);
  print_newline ()

(** show functions *)

let show_literal = function
  | LIInt i -> "Int " ^ string_of_int i
  | LIFloat f -> "Float " ^ string_of_float f
  | LIChar c -> "Char '" ^ String.make 1 c ^ "'"
  | LIString s -> "String \"" ^ String.escaped s ^ "\""
  | LINil -> "Nil"

let show_primitive_type = function
  | PTInt (Some (s, w)) ->
    Printf.sprintf "$i.%c.%d" (if s then 's' else 'u') w
  | PTFloat (Some w) -> Printf.sprintf "$f.%d" w
  | PTInt None -> "$i.*"
  | PTFloat None -> "$f.*"
  | PTVoid -> "$v"

let rec show_path = function
  | [] -> ""
  | [p] -> p.item
  | p :: ps -> p.item ^ "::" ^ show_path ps

(* print functions *)

let rec print_patt_node' i { span; item } =
  match item with
    | PAVariable n ->
      p i ("PAVariable " ^ n) span;
    
    | PAWildcard ->
      p i "PAWildcard" span
    
and print_type_node' i { span; item } =
  match item with
    | TYGrouping t ->
      p i "TYGrouping" span;
      print_type_node' (i + 1) t

    | TYForall (args, t) ->
      let args' = List.map ((^) "'") (nmapi args) in 
      let args'' = String.concat " " args' in 
      p i ("TYForall " ^ args'') span;
      print_type_node' (i + 1) t

    | TYAny ->
      p i "TYAny" span

    | TYVariable s ->
      p i ("TYVariable '" ^ s.item) span

    | TYEffect t ->
      p i "TYEffect" span;
      print_type_node' (i + 1) t
    
    | TYFunction (a, r) ->
      p i "TYFunction" span;
      print_type_node' (i + 1) a;
      print_type_node' (i + 1) r
    
    | TYTuple ts ->
      p i "TYTuple" span;
      List.iter (print_type_node' (i + 1)) ts

    | TYConstruct (a, t) ->
      p i ("TYConstruct " ^ (Ident.to_string t.item)) span;
      if Option.is_some a
      then print_type_node' (i + 1) (Option.get a);
    
    | TYPrimitive t ->
      p i ("TYPrimitive " ^ show_primitive_type t) span

and print_complex_type_node' i { span; item } =
  match item with
    | CTVariant rows ->
      p i "CTVariant" span;
      let f (id, ts) = begin
        p (i + 1) (Ident.to_string id.item) id.span;
        List.iter (print_type_node' (i + 2)) ts
      end in List.iter f rows
    | CTSimple t ->
      p i "CTSimple" span;
      print_type_node' (i + 1) t

and print_expr_node' i { span; item } =
  match item with
    | EXGrouping e -> 
      p i "EXGrouping" span;
      print_expr_node' (i + 1) e

    | EXBinding (bind, e) ->
      p i "EXBinding" span;
      print_patt_node' (i + 1) bind.vb_patt;
      print_expr_node' (i + 1) bind.vb_expr;
      print_expr_node' (i + 1) e

    | EXLambda {vb_patt; vb_expr} ->
      p i "EXLambda" span;
      print_patt_node' (i + 1) vb_patt;
      print_expr_node' (i + 1) vb_expr

    | EXSequence (e1, e2) ->
      p i "EXSequence" span;
      print_expr_node' (i + 1) e1;
      print_expr_node' (i + 1) e2

    | EXConstraint (e, t) ->
      p i "EXContstraint" span;
      print_expr_node' (i + 1) e;
      print_type_node' (i + 1) t;

    | EXApplication (a, es) ->
      p i "EXApplication" span;
      print_expr_node' (i + 1) a;
      List.iter (print_expr_node' (i + 1)) es

    | EXTuple es ->
      p i "EXTuple" span;
      List.iter (print_expr_node' (i + 1)) es

    | EXConstruct (n, es) ->
      p i ("EXConstruct " ^ Ident.to_string n.item) span;
      List.iter (print_expr_node' (i + 1)) es 

    | EXLiteral l ->
      p i ("EXLiteral " ^ show_literal l) span
    
    | EXIdentifier {span = _; item = id} ->
      p i ("EXIdentifier " ^ Ident.to_string id) span
    
    | EXMagical n -> p i ("EXMagical " ^ n) span
    | EXError -> p i "EXError" span

and print_import_kind_node' i { span; item } =
  match item with
    | IK_Module -> p i "IK_Module" span
    | IK_Simple n -> p i ("IK_Simple " ^ show_path n) span
    | IK_Glob -> p i "IK_Glob" span
    | IK_Rename (s, d) ->
      p i ("IK_Rename " ^ show_path s ^ " as " ^ d.item) span
    | IK_Nested is ->
      p i "IK_Nested" span;
      List.iter (print_import_kind_node' (i + 2)) is

and print_toplevel_node' i { span; item } =
  match item with
    | TLValueDef {vd_name; vd_expr} ->
      p i ("TLValueDef " ^ Ident.to_string vd_name.item) span;
      print_expr_node' (i + 1) vd_expr

    | TLTypeDef {td_params; td_name; td_type} ->
      let params =
        nmapi td_params
        |> List.map (fun p -> "'" ^ p ^ " ")
        |> String.concat ""
      in
      let name = params ^ Ident.to_string td_name.item in
      p i ("TLTypeDef " ^ name) span;
      print_complex_type_node' (i + 1) td_type

    | TLExternDef {ed_native_name; ed_name; ed_type} ->
      p i ("TLExternDef " ^ Ident.to_string ed_name.item) span;
      p (i + 1) ed_native_name.item ed_native_name.span;
      print_type_node' (i + 1) ed_type;

    | TLImport { i_path; i_kind} ->
      p i ("TLImport " ^ show_path i_path) span;
      print_import_kind_node' (i + 1) i_kind

    | TLModule (n, ast) ->
      p i ("TLModule " ^ n.item) span;
      print_ast' (i + 1) ast
    
and print_ast' i nodes =
  let first = ref true in

  let f i (tl: toplevel node) =
    if Configs.hide_stdlib_nodes && Span.is_in_stdlib tl.span
    then ()
    else begin
      if !first then first := false
      else print_newline ();
      print_toplevel_node' i tl
    end
  in

  let rec go = function
    | [] -> ()
    | [n] -> f i n
    | n :: ns ->
      f i n;
      go ns
  in go nodes

(* expose functions *)

let print_expr_node = print_expr_node' 0
let print_type_node = print_type_node' 0
let print_toplevel_node = print_toplevel_node' 0
let print_ast = print_ast' 0