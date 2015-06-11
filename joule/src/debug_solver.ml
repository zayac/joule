open Core.Std

let logic_from_string s =
  Parser.logic Lexer.read (Lexing.from_string s)

let _ =
  let a = logic_from_string "(and (not a) (not b) (not l) c f_denoise__1_denoise f_initKMeans__1_initKMeans (or f_read__1_read_color f_read__1_read_grayscale f_read__1_read_unchanged) (or (not c) (not f_denoise__1_denoise) (not f_initKMeans__1_initKMeans) (not f_read__1_read_color) (not t) a b l s) (or (not c) (not f_denoise__1_denoise) (not f_initKMeans__1_initKMeans) (not f_read__1_read_grayscale) (not t) a b l s) (or (not c) (not f_denoise__1_denoise) (not f_initKMeans__1_initKMeans) (not f_read__1_read_unchanged) (not t) a b l s))" in
  let b = logic_from_string "(and (not a) (not b) (not l) c f_denoise__1_denoise f_initKMeans__1_initKMeans (or f_read__1_read_color f_read__1_read_grayscale f_read__1_read_unchanged) (or (not c) (not f_denoise__1_denoise) (not f_initKMeans__1_initKMeans) (not f_read__1_read_color) a b l t) (or (not c) (not f_denoise__1_denoise) (not f_initKMeans__1_initKMeans) (not f_read__1_read_grayscale) a b l t) (or (not c) (not f_denoise__1_denoise) (not f_initKMeans__1_initKMeans) (not f_read__1_read_unchanged) a b l t))" in
  (*let a = logic_from_string "(and (not a) (not b) (not l) c f_denoise__1_denoise f_initKMeans__1_initKMeans (or f_read__1_read_color f_read__1_read_grayscale f_read__1_read_unchanged) (or (not c) (not f_denoise__1_denoise) (not f_initKMeans__1_initKMeans) (not f_read__1_read_color) (not t) a b l s) (or (not c) (not f_denoise__1_denoise) (not f_initKMeans__1_initKMeans) (not f_read__1_read_grayscale) (not t) a b l s) (or (not c) (not f_denoise__1_denoise) (not f_initKMeans__1_initKMeans) (not f_read__1_read_unchanged) (not t) a b l s))" in*)
  (*let b = logic_from_string "(and (not a) (not b) (not l) (not t) c f_denoise__1_denoise f_initKMeans__1_initKMeans (or f_read__1_read_color f_read__1_read_grayscale f_read__1_read_unchanged) (or (not c) (not f_denoise__1_denoise) (not f_initKMeans__1_initKMeans) (not f_read__1_read_color) a b l s) (or (not c) (not f_denoise__1_denoise) (not f_initKMeans__1_initKMeans) (not f_read__1_read_grayscale) a b l s) (or (not c) (not f_denoise__1_denoise) (not f_initKMeans__1_initKMeans) (not f_read__1_read_unchanged) a b l s))" in*)
  let a, b = Cnf.from_logic a, Cnf.from_logic b in
  (*let not_a, not_b = Cnf.(~-a), Cnf.(~-b) in*)
  print_endline (Cnf.(to_string ((~-a * b) * (a + b))));
  ()
