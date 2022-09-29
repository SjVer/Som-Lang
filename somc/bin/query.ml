module type S = sig
	type a
	type r
	val c: a -> r  
end

module Make(S: S) = struct

  let db = Hashtbl.create 3

  let force arg =
    let result = S.c arg in
    Hashtbl.replace db arg result;
    result

  let call arg =
    match Hashtbl.find_opt db arg with
    | Some result -> result
    | None -> force arg

end