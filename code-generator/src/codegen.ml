open Core.Std
open Term

let dequotize s =
  if Poly.(s.[0] = '"') && Poly.(s.[String.length s - 1] = '"') then
    String.sub s 1 (String.length s - 2)
  else
    s

let term_to_type = function
  | Nil -> "const void *"
  | Symbol s -> dequotize s
  
let term_to_cpp_macro is_decl = function
  | Nil -> ""
  | Record (head, None) ->
    String.Map.fold head ~init:"COMMA "
      ~f:(fun ~key ~data acc ->
        let g, t = data in
        if is_decl then
          String.concat [acc; term_to_type t; " "; dequotize key]
        else
          acc ^ (dequotize key)
      )

let generate_from_terms outc name t =
  match t with
  | Nil
  | Record _ ->
    fprintf outc "#define CAL_FI_%s_decl %s\n" name (term_to_cpp_macro true t);
    fprintf outc "#define CAL_FI_%s_use %s\n" name (term_to_cpp_macro false t);
  | Choice (head, None) ->
    let more_than_one = ref false in
    let args = String.Map.fold head ~init:""
      ~f:(fun ~key ~data acc ->
        let g, t = data in
        if !more_than_one then
          let _ = more_than_one := true in
          String.concat [acc; ", "; term_to_type t; " "; dequotize key]
        else
          String.concat [acc; term_to_type t; " "; dequotize key]
      ) in
    fprintf outc "variant_message %s(%s);\n" name args

let generate_from_bools outc name value =
  match value with
  | false ->
    fprintf outc "#define f_%s\n" name
  | true -> ()
