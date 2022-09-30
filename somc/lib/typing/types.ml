type t =
  | TName of Path.t
  | TPrim of prim
  | TVar of var ref
  | TEff of t
  | TApp of t * t
  | TFun of t * t
  | TTup of t list

and var =
  | Unbound of int * int (** [id] and [level] *)
  | Link of t (** already solved *)
  | Generic of int (** [id] *)

and prim =
  | PInt of bool * int
  | PFloat of int
  | PVoid

let show_prim =
  let f = Printf.sprintf in
  function
    | PInt (s, w) ->
      let s' = if s then 's' else 'u' in
      f "$i.%c.%d" s' w
    | PFloat w -> f "$f.%d" w
    | PVoid -> "%v"

let show_type ty =
  let names = Hashtbl.create 10 in
	let count = ref 0 in
	let next_name () =
		let i = !count in
		incr count;
    let name = String.make 1 (Char.chr (97 + i mod 26)) ^
			if i >= 26 then string_of_int (i / 26) else ""
    in "'" ^ name
	in

  let ret prim s = if prim then "("^s^")" else s in

  let rec go prim = function
    | TName p -> Path.to_string p
    | TPrim p -> show_prim p
    | TVar {contents=Unbound (id, _)} ->
      "_" ^ string_of_int id
    | TVar {contents=Link ty} -> go prim ty
    | TVar {contents=Generic id} -> begin
        match Hashtbl.find_opt names id with
          | Some n -> n
          | None ->
            let name = next_name () in
            Hashtbl.add names id name;
            name
      end
    | TApp (t1, t2) ->
      ret prim (go false t1 ^ " " ^ go true t2)
    | TEff t -> "!" ^ go true t
    | TFun (a, r) ->
      let a' = go true a in (* eval a first *)
      ret prim (a' ^ " -> " ^ go false r)
    | TTup ts ->
      let ts' = List.map (go true) ts in
      ret prim (String.concat "; " ts')

  in
  let ty_str = go false ty in
  if !count > 0 then
    let names = Hashtbl.fold (fun _ v a -> v :: a) names [] in
    String.concat " " (List.sort String.compare names) ^ " . " ^ ty_str
  else
    ty_str