open Core.Std
open Term

exception WrongFormat of string

let method_body_hash = ref String.Map.empty

let methods_variables = ref String.Map.empty

let salvos_mapping = ref String.Map.empty

let variant_args = ref String.Map.empty

let inherited_classes = ref String.Map.empty
let generated_classes = ref String.Map.empty

let dequotize s =
  if Poly.(s.[0] = '"') && Poly.(s.[String.length s - 1] = '"') then
    String.sub s 1 (String.length s - 2)
  else
    s

let field_to_string name = function
  | Symbol s ->
    String.concat [s; " "; dequotize name; ";\n"]
  | _ -> raise (WrongFormat "unexpected format of a field")

let gen_serialize_method fields =
  let fields = List.map ~f:dequotize fields in
  let body =
    if Poly.(List.length fields > 0) then
      sprintf "\t\tar(%s);" (String.concat ~sep:", " fields)
    else
      ""
  in
  sprintf "\n\ttemplate<class Archive>
\tvoid serialize(Archive & ar)
\t{
%s
\t}\n" body

let rec get_method_type_from_term t =
  match t with
  | Tuple [Symbol ret; Choice (map, None)] ->
    begin
      match String.Map.find map "\"code\"" with
      | Some (cnf, Symbol s) when Cnf.equal cnf Cnf.make_true -> Some ret, s
      (*| Some (cnf, Nil) when Cnf.equal cnf Cnf.make_true -> Some ret, ""*)
      | _ -> raise (WrongFormat ("unexpected format of method body: " ^ (Term.to_string t)))
    end
  | _ -> raise (WrongFormat ("unexpected format of method body: " ^ (Term.to_string t)))

let method_to_string cl name t =
  let name = dequotize name in
  let header = String.substr_replace_all name ~pattern:"%self%" ~with_:cl in
  let ret, hash = get_method_type_from_term t in
  match String.Map.find !method_body_hash hash with
  | Some value ->
    let params, body = value in
    let params, body = List.map ~f:dequotize params, dequotize body in
    let index = ref 0 in
    let ss = String.fold header ~init:""
                ~f:(fun acc el ->
                  if Poly.(el = ',') then
                    let _ = index := !index + 1 in
                    String.concat [acc; " "; List.nth_exn params (!index - 1); ","]
                  else if Poly.(el = ')' && List.length params > 0) then
                    String.concat [acc; " "; List.nth_exn params !index; ")"]
                  else
                    acc ^ (String.make 1 el)) in
    String.concat [Option.value_map ~default:"" ~f:(fun x -> x ^ " ") ret; ss; " "; body; "\n"]
  | None -> raise (WrongFormat ("cannot find definition for a method with hash value " ^ hash))

let rec term_to_type t =
  match t with
  | Nil -> "size_t" (* dummy type *)
  | Symbol s -> dequotize s
  | Record (r, None) ->
    let code = create_class_decl "dummy" r in
    let name = sprintf "class_%d" (String.hash code) in
    let code = create_class_decl name r in
    inherited_classes := String.Map.add !inherited_classes ~key:name ~data:code;
    name
  | _ -> raise (WrongFormat ("unexpected format of a type: " ^ (Term.to_string t)))

and create_class_decl name map =
  let fields = ref String.Map.empty in
  let methods = ref String.Map.empty in
  String.Map.iter map
    ~f:(fun ~key ~data ->
      let _, data = data in
      if String.contains key '(' then
        methods := String.Map.add !methods ~key ~data
      else
        fields := String.Map.add !fields ~key ~data
    );
  let cl_start = String.concat ["struct "; name; "{\n"] in
  let field_s = String.Map.fold !fields ~init:[]
                  ~f:(fun ~key ~data acc ->
                    acc @ ["\t"; field_to_string key data]
                  ) in
  let fl = String.Map.keys !fields in
  let methods_s = String.Map.fold !methods ~init:[]
                  ~f:(fun ~key ~data acc ->
                    acc @ ["\t"; method_to_string name key data]
                  ) in
  String.concat (cl_start :: field_s @ methods_s @ [gen_serialize_method fl; "};\n"])

let number_of_elements term =
  match term with
  | Nil -> 0
  | Record (head, None) -> String.Map.length head
  | _ -> raise (WrongFormat ("unexpected format of a term " ^ (Term.to_string term)))

let term_to_cpp_macro is_decl term =
  match term with
  | Nil -> ""
  | Record (head, None) ->
    String.Map.fold head ~init:""
      ~f:(fun ~key ~data acc ->
        let g, t = data in
        if is_decl then
          String.concat [acc; " COMMA "; term_to_type t; " "; dequotize key]
        else
          String.concat [acc; " COMMA "; dequotize key]
      )
  | _ -> raise (WrongFormat ("unexpected format of a term " ^ (Term.to_string term)))

let term_to_cpp_macro_decl term =
  term_to_cpp_macro true term

let term_to_cpp_macro_use term =
  term_to_cpp_macro false term


let term_to_cpp_macro_types term =
  match term with
  | Nil -> ""
  | Record (head, None) ->
    String.Map.fold head ~init:""
      ~f:(fun ~key ~data acc ->
        let _, t = data in
        String.concat [acc; " COMMA "; term_to_type t]
      )
  | _ -> raise (WrongFormat ("unexpected format of a term " ^ (Term.to_string term)))

(*
let term_to_cpp_placeholders file outc =
  match String.Map.find !variant_args file with
  | None ->()
  | Some map ->
    String.Map.iter map
      ~f:(fun ~key ~data ->
        let variant = key in
        fprintf outc "#define %s_DOWN_%s_tuple_get\n" file variant;
        fprintf outc "#define %s_DOWN_%s_types\n" file variant
      )
*)

let term_to_cpp_macro_variants outc file variant term =
  match String.Map.find !variant_args file with
  | None -> ()
    (*fprintf outc "#define %s_DOWN_%s_tuple_get\n" file variant;*)
    (*fprintf outc "#define %s_DOWN_%s_types\n" file variant*)
  | Some v ->
    match String.Map.find v variant with
    | None -> ()
      (*fprintf outc "#define %s_DOWN_%s_tuple_get\n" file variant;*)
      (*fprintf outc "#define %s_DOWN_%s_types\n" file variant*)
    | Some start_arg ->
      begin
        let n = number_of_elements term in
        let s = ref "" in
        for i = start_arg to start_arg+n-1 do
          s := String.concat [!s; " COMMA "; "std::get<"; string_of_int i; ">(_data)"]
        done;
        fprintf outc "#define %s_DOWN_%s_tuple_get %s\n" file variant !s;
        fprintf outc "#define %s_DOWN_%s_types %s\n" file variant (term_to_cpp_macro_types term)
      end


let open_code_hash_file dirname =
  try
    let filename = dirname ^ "/code-hash" in
    method_body_hash :=
      In_channel.with_file filename
        ~f:(fun inx -> Parser.code_hash_parse Lexer.read (Lexing.from_channel inx))
  with Sys_error _ -> ()

let read_json_file short_name file_name =
  let json = Yojson.Basic.from_file file_name in
  let open Yojson.Basic.Util in
  let salvos = json |> member "salvos" |> to_list in
  let salvos_lst = List.map salvos
              ~f:(fun json ->
                let name = json |> member "name" |> to_string in
                let mapping = json |> member "mapping" |> to_list in
                let l = List.map mapping
                          ~f:(fun json ->
                            let ch = json |> member "channel" |> to_int in
                            let var = json |> member "variant" |> to_string in
                            ch, var)
                in
                name, l
              )
  in
  salvos_mapping := String.Map.add !salvos_mapping ~key:short_name ~data:(String.Map.of_alist_exn salvos_lst);
  let variants = json |> member "variants" |> to_list in
  let variants_lst = List.map variants
                       ~f:(fun json ->
                         let name = json |> member "name" |> to_string in
                         let args = json |> member "arguments" |> to_int in
                         name, args)
  in
  variant_args := String.Map.add !variant_args ~key:short_name ~data:(String.Map.of_alist_exn variants_lst)

let print_salvo_routing outc file_name =
  match String.Map.find !salvos_mapping file_name with
  | None -> ()
  | Some v ->
    String.Map.iter v ~f:(fun ~key ~data ->
      let sl = List.map data ~f:(fun (c, v) -> sprintf "{%d COMMA \"%s\"}" c v) in
      fprintf outc "#define %s_DOWN_%s_ochannels %s\n" file_name key (String.concat ~sep:" COMMA " sl)
    )

let generate_from_terms outc file_name name t =
  (*let _ = inherited_classes := String.Map.empty in*)
  let _ =
    match t with
    | Nil ->
      fprintf outc "#define %s_DOWN_%s_decl %s\n" file_name name (term_to_cpp_macro_decl t);
      fprintf outc "#define %s_DOWN_%s_use %s\n" file_name name (term_to_cpp_macro_use t);
      term_to_cpp_macro_variants outc file_name name t
    | Record (map, None) ->
      begin
        let index = String.substr_index name ~pattern:"DOWN_class_" in
        match index with
        | Some _ ->
          begin
            match String.chop_suffix name ~suffix:"_out" with
            | Some s ->
              fprintf outc "%s" (create_class_decl s map)
            | None -> ()
          end
        | None ->
          begin
            fprintf outc "#define %s_DOWN_%s_decl %s\n" file_name name (term_to_cpp_macro_decl t);
            fprintf outc "#define %s_DOWN_%s_use %s\n" file_name name (term_to_cpp_macro_use t);
            term_to_cpp_macro_variants outc file_name name t
          end
      end
    | Choice (head, None) ->
      fprintf outc "#define %s_UP_%s" file_name file_name;
      if String.Map.is_empty head then
        fprintf outc "\n"
      else
        begin
          fprintf outc " do {\\\n";
          String.Map.iter head
            ~f:(fun ~key ~data ->
              fprintf outc "\tif (_msg.getType() == %s) {\\\n" key;
              fprintf outc "\t\toutput(1, std::move(_msg));\\\n";
              fprintf outc "\t\treturn;\\\n";
              fprintf outc "\t}\\\n"
            );
          fprintf outc "} while (0);\n"
        end
    | Symbol _ -> ()
    | _ -> raise (WrongFormat ("unexpected format of a term " ^ (Term.to_string t)))
  in
  let set =
    match String.Map.find !generated_classes file_name with
    | None -> ref String.Set.empty
    | Some s -> ref s
  in
  String.Map.iter !inherited_classes
    ~f:(fun ~key ~data ->
      if not (String.Set.mem !set key) then
      begin
        fprintf outc "%s\n" data;
        set := String.Set.add !set key
      end
    );
  generated_classes := String.Map.add !generated_classes ~key:file_name ~data:!set

let generate_from_bools outc name value =
  match value with
  | false ->
    fprintf outc "#define %s\n" name
  | true -> ()
