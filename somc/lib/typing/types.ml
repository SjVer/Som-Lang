module ID = struct
  let current = ref 0

  let next () =
    let id = !current in
    current := id + 1;
    id

  let reset () =
    current := 0
end

type t =
  | TName of Path.t
  | TPrim of prim
  | TVar of var ref
  | TEff of t
  | TApp of t * t
  | TFun of t * t
  | TTup of t list
  | TNever
  | TError

and var =
  | Unbound of int * int (** [id] and [level] *)
  | Link of t (** already solved *)
  | Generic of int (** [id] *)

and prim =
  | PInt of bool * int
  | PFloat of int
  | PVoid

let new_var level = TVar (ref (Unbound (ID.next (), level)))
let new_gen_var () = TVar (ref (Generic (ID.next ())))

let show_prim =
  let f = Printf.sprintf in
  function
    | PInt (s, w) ->
      let s' = if s then 's' else 'u' in
      f "$i.%c.%d" s' w
    | PFloat w -> f "$f.%d" w
    | PVoid -> "$v"

let show ty debug =
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
  let do_var t map id =
    match Hashtbl.find_opt map id with
      | Some n -> n
      | None ->
        let name = next_name t in
        Hashtbl.add map id name;
        name
  in

  let ret prim s = if prim then "("^s^")" else s in

  let rec go prim = function
    | TName p -> Path.to_string p
    | TPrim p -> show_prim p
    | TVar {contents=Unbound (id, _)} -> do_var `U unames id
    | TVar {contents=Link ty} -> go prim ty
    | TVar {contents=Generic id} -> do_var `G gnames id
    | TApp (t1, t2) ->
      ret prim (go false t1 ^ " " ^ go true t2)
    | TEff t -> "!" ^ go true t
    | TFun (a, r) ->
      let a' = go true a in (* eval a first *)
      ret prim (a' ^ " -> " ^ go false r)
    | TTup ts ->
      let ts' = List.map (go true) ts in
      ret prim (String.concat "; " ts')
    | TNever -> if debug then "<never>" else ""
    | TError -> if debug then "<error>" else "_"

  in
  let ty_str = go false ty in
  if debug && !gcount > 0 then
    let names = Hashtbl.fold (fun _ v a -> v :: a) gnames [] in
    String.concat " " (List.sort String.compare names) ^ " . " ^ ty_str
  else
    ty_str