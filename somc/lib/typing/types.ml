let next_id =
  let i = ref 0 in
  fun () -> incr i; !i

type t =
  | TName of Symbols.Ident.t
  | TPrim of prim
  | TVague of vague ref
  | TVar of var ref
  | TEff of t
  | TApp of t * t
  | TFun of t * t
  | TTup of t list
  | TNever
  | TError

and vague =
  | VGInt
  | VGFloat
  | VGSolved of t
  | VGGeneric of vague

and prim =
  | PInt of bool * int
  | PFloat of int
  | PVoid

and var =
  | VRUnbound of int * int (** [id] and [depth] *)
  | VRSolved of t
  | VRGeneric of int (** [id] *)

let new_var depth = TVar (ref (VRUnbound (next_id (), depth)))
let new_gen_var () = TVar (ref (VRGeneric (next_id ())))

let show_prim =
  let f = Printf.sprintf in
  function
    | PInt (s, w) ->
      let s' = if s then 's' else 'u' in
      f "$i.%c.%d" s' w
    | PFloat w -> f "$f.%d" w
    | PVoid -> "$v"

let show ty debug =
  (* mapping id's to names starting by 'a' *)
  let gnames = Hashtbl.create 10 in
  let unames = Hashtbl.create 10 in
	let gcount = ref 0 in
	let ucount = ref 0 in
	let next_name t =
    let i = match t with
      | `G -> incr gcount; !gcount - 1
      | `U -> incr ucount; !ucount - 1
    in
    let name = String.make 1 (Char.chr (97 + i mod 26)) ^
			if i >= 26 then string_of_int (i / 26) else ""
    in match t with
      | `G -> "'" ^ name
      | `U -> "'_" ^ name
	in

  (* typevar to string *)
  let show_var t map id =
    let name = match Hashtbl.find_opt map id with
      | Some n -> n
      | None ->
        let name = next_name t in
        Hashtbl.add map id name;
        name
    in
    if debug then match t with
      | `G -> "#" ^ string_of_int id
      | `U -> "#_" ^ string_of_int id
    else name
  in

  let wrap prim s = if prim then "(" ^ s ^ ")" else s in

  let rec go prim = function
    | TName p -> Symbols.Ident.to_string p
    | TPrim p -> show_prim p
    | TVague k -> begin match !k with
        | VGInt -> if debug then "<int>" else "$i.*"
        | VGFloat -> if debug then "<float>" else "$f.*"
        | VGGeneric k ->
          let k_str = go true (TVague (ref k)) in
          if debug then "#" ^ k_str else k_str
        | VGSolved t -> go prim t
      end
    | TVar {contents=VRUnbound (id, _)} -> show_var `U unames id
    | TVar {contents=VRSolved ty} -> go prim ty
    | TVar {contents=VRGeneric id} -> show_var `G gnames id
    | TApp (t1, t2) -> wrap prim (go false t1 ^ " " ^ go true t2)
    | TEff t -> "!" ^ go true t
    | TFun (a, r) ->
      let a' = go true a in (* eval a first *)
      wrap prim (a' ^ " -> " ^ go false r)
    | TTup ts ->
      let ts' = List.map (go true) ts in
      wrap prim (String.concat " ; " ts')
    | TNever -> if debug then "<never>" else ""
    | TError -> if debug then "<error>" else "_"

  in
  let ty_str = go false ty in

  (* prepend "'a 'b ." if there's generic typevars *)
  if debug && !gcount > 0 then
    let names =
      let f i _ a = show_var `G gnames i :: a in
      Hashtbl.fold f gnames []
    in
    String.concat " " (List.sort String.compare names) ^ " . " ^ ty_str
  else
    ty_str