type pos = {line: int; col: int}

let show_pos p = string_of_int p.line ^ ":" ^ string_of_int p.col

type span = {file: string; start: pos; end_: pos}

let show_span s = s.file ^ "@" ^ show_pos s.start