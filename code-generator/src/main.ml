open Core.Std
open Codegen

let loop values_filename =
  let term_var_hash, bool_var_hash = In_channel.with_file values_filename
    ~f:(fun inx -> Parser.values_parse Lexer.read (Lexing.from_channel inx)) in
  let dirname = Filename.dirname values_filename in
  let ofname = dirname ^ "/CAL_FI_variables.h" in
  let _ = Codegen.open_code_hash_file dirname in
  let outc = Out_channel.create ofname in
  fprintf outc "#define COMMA ,\n";
  String.Map.iter ~f:(fun ~key ~data ->
    Codegen.generate_from_terms outc key data) term_var_hash;
  String.Map.iter ~f:(fun ~key ~data ->
    Codegen.generate_from_bools outc key data) bool_var_hash;
  Out_channel.flush outc;
  Out_channel.close outc

let command =
  Command.basic
    ~summary:"code generator"
    Command.Spec.(
      empty
      +> anon ("values-filename" %:string)
    )
    (fun values_filename () ->
      loop values_filename)

let () =
  Command.run command
