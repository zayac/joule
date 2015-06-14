open Core.Std

let logic_from_string s =
  Parser.logic Lexer.read (Lexing.from_string s)

let _ =
  let cnf = Cnf.from_logic (logic_from_string "(and (or a b) (or a c))") in
  let result = Sat.solve_max ~verbose:true cnf in
  ()
  
