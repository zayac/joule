open Lexing

type location =
  { loc_start: Lexing.position;
    loc_end: Lexing.position;
  }

let filename = ref "_none_"

let pos_loc s e = {
  loc_start = s;
  loc_end = e;
}

let curr lexbuf = {
  loc_start = lexbuf.lex_start_p; 
  loc_end = lexbuf.lex_curr_p;
}

let symbol_loc = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
}

let rhs_loc n = {
  loc_start = Parsing.rhs_start_pos n;
  loc_end = Parsing.rhs_end_pos n;
}

let (msg_file, msg_line, msg_chars, msg_to, msg_colon) =
  ("File \"", "\", line ", ", characters ", "-", ":")

(* return line, char from the given position *)
let get_pos_info pos =
  (pos.pos_lnum, pos.pos_cnum - pos.pos_bol)

let print_loc fmt loc =
  let (line, startchar) = get_pos_info loc.loc_start in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  Format.fprintf fmt "%s%s%s%i" msg_file !filename msg_line line;
  if startchar >= 0 then
    Format.fprintf fmt "%s%i%s%i" msg_chars startchar msg_to endchar

let print fmt loc = Format.fprintf fmt "%a%s@." print_loc loc msg_colon

type 'a loc = {
  value : 'a;
  loc : location;
}

let mkloc value loc = { value ; loc }

