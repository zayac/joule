open Core.Std
open Codegen

let ochannels = ref String.Map.empty

let create_or_open_file dirname key data =
  let file_name_id =
    let starti =
      if String.is_prefix key ~prefix:"f_" then 2
      else if String.is_prefix key ~prefix:"_" then
        match String.index_from key 1 '_' with
        | Some i -> i+1
        | None -> 0
      else
        0
    in
    match String.index_from key starti '_' with
    | Some i ->
      String.slice key starti i
    | None ->
      String.drop_prefix key starti
  in
  match String.Map.find !ochannels file_name_id with
  | None ->
    let ofname = String.concat [dirname; "/"; file_name_id; "_CAL_FI_variables.h"] in
    let outc = Out_channel.create ofname in
    fprintf outc "#define COMMA ,\n";
    fprintf outc "#define CAL_FI_HIDE_CLASSES 1\n";
    let _ = ochannels := String.Map.add !ochannels ~key:file_name_id ~data:outc in
    file_name_id, outc
  | Some outc -> file_name_id, outc

let loop values_filename =
  let term_var_hash, bool_var_hash = In_channel.with_file values_filename
    ~f:(fun inx -> Parser.values_parse Lexer.read (Lexing.from_channel inx)) in
  let dirname = Filename.dirname values_filename in
  let _ = Codegen.open_code_hash_file dirname in
  try
    let set = ref String.Set.empty in
    (* fill 'methods_variables' hash with variables representing body of methods *)
    String.Map.iter term_var_hash
      ~f:(fun ~key ~data ->
        if String.is_prefix key ~prefix:"method_body_code_" then
          methods_variables := String.Map.add !methods_variables (String.concat ["\""; key; "\""]) data
      );
    String.Map.iter term_var_hash
      ~f:(fun ~key ~data ->
        let file_name, outc = create_or_open_file dirname key data in
        printf "Generating code for term variable '%s' from '%s' file\n" key file_name;
        if not (String.Set.mem !set file_name) then
          begin
            Codegen.read_json_file file_name (String.concat [dirname; "/"; file_name; ".json"]);
            (*Codegen.term_to_cpp_placeholders file_name outc;*)
            Codegen.print_salvo_routing outc file_name;
            set := String.Set.add !set file_name
          end;
        Codegen.generate_from_terms outc file_name key data
      );
    String.Map.iter bool_var_hash
      ~f:(fun ~key ~data ->
        let _, outc = create_or_open_file dirname key data in
        Codegen.generate_from_bools outc key data
      );
  with Codegen.WrongFormat msg ->
    Printf.eprintf "%s\n" msg;
  String.Map.iter !ochannels
    ~f:(fun ~key ~data ->
      Out_channel.flush data;
      Out_channel.close data
    )

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
