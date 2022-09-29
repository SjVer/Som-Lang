type t =
  {
    line: int;
    col: int;
    offset: int;
  }

let show_loc p =
  string_of_int p.line ^ ":" ^ string_of_int p.col