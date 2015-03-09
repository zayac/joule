open Core.Std
open Term

exception WrongFormat of string

let method_body_hash = ref String.Map.empty

let dequotize s =
  if Poly.(s.[0] = '"') && Poly.(s.[String.length s - 1] = '"') then
    String.sub s 1 (String.length s - 2)
  else
    s

let term_to_type t =
  match t with
  | Nil -> "const void *"
  | Symbol s -> dequotize s
  | _ -> raise (WrongFormat ("unexpected format of a type: " ^ (Term.to_string t)))

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

let term_to_cpp_macro_tuple_get term =
    let n = number_of_elements term in
    let s = ref "" in
    for i = 0 to n-1 do
      s := String.concat [!s; " COMMA "; "std::get<"; string_of_int i; ">(data)"]
    done;
    !s

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

let field_to_string name = function
  | Symbol s ->
    String.concat [s; " "; dequotize name; ";\n"]
  | _ -> raise (WrongFormat "unexpected format of a field")

let get_method_type_from_term t =
  match t with
  | Symbol hash
  | Tuple [Symbol "override"; Symbol hash] ->
    None, hash
  | Tuple [Symbol ret; Tuple [Symbol "override"; Symbol hash]]
  | Tuple [Symbol ret; Symbol hash] ->
    Some ret, hash
  | _ -> raise (WrongFormat ("unexpected format of method body: " ^ (Term.to_string t)))

let method_to_string cl name t =
  let name = dequotize name in
  let header = String.substr_replace_all name ~pattern:"%self%" ~with_:cl in
  let ret, hash = get_method_type_from_term t in
  match String.Map.find !method_body_hash ("\"" ^ hash ^ "\"") with
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

let create_class_decl name map =
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
  let methods_s = String.Map.fold !methods ~init:[]
                  ~f:(fun ~key ~data acc ->
                    acc @ ["\t"; method_to_string name key data]
                  ) in
  String.concat (cl_start :: field_s @ methods_s @ ["};\n"])

let open_code_hash_file dirname =
  try
    let filename = dirname ^ "/code-hash" in
    method_body_hash :=
      In_channel.with_file filename
        ~f:(fun inx -> Parser.code_hash_parse Lexer.read (Lexing.from_channel inx))
  with Sys_error _ -> ()

(*let read_json_file file_name =*)
  (*let json = Yojson.Basic.from_file file_name in*)
  (*let open Yojson.Basic.Util in*)
  (*[>try<]*)
    (*let code_hash = json |> member "code_hash" |> to_list in*)
    (*let hash_list = List.map code_hash*)
                      (*~f:(fun json ->*)
                        (*let hash = json |> member "hash" |> to_string in*)
                        (*let args = json |> member "arguments" |> to_list |> filter_string in*)
                        (*let code = json |> member "code" |> to_string in*)
                        (*hash, (args, code)*)
                      (*) in*)
    (*method_body_hash := String.Map.of_alist_exn hash_list*)
  (*[>with Yojson.Basic.Util.Type_error _ -><]*)
    (*[>()<]*)

let generate_from_terms outc file_name name t =
  
  match t with
  | Nil ->
    fprintf outc "#define %s_DOWN_%s_decl %s\n" file_name name (term_to_cpp_macro_decl t);
    fprintf outc "#define %s_DOWN_%s_use %s\n" file_name name (term_to_cpp_macro_use t);
    fprintf outc "#define %s_DOWN_%s_tuple_get %s\n" file_name name (term_to_cpp_macro_tuple_get t);
    fprintf outc "#define %s_DOWN_%s_types %s\n" file_name name (term_to_cpp_macro_types t)
  | Record (map, None) ->
    begin
      let index = String.substr_index name ~pattern:"DOWN_class_" in
      match index with
      | Some i ->
        (*let short_name = String.drop_prefix name i in*)
        fprintf outc "%s" (create_class_decl name map)
      | None ->
        begin
          fprintf outc "#define %s_DOWN_%s_decl %s\n" file_name name (term_to_cpp_macro_decl t);
          fprintf outc "#define %s_DOWN_%s_use %s\n" file_name name (term_to_cpp_macro_use t);
          fprintf outc "#define %s_DOWN_%s_tuple_get %s\n" file_name name (term_to_cpp_macro_tuple_get t);
          fprintf outc "#define %s_DOWN_%s_types %s\n" file_name name (term_to_cpp_macro_types t)
        end
    end
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
  | Symbol _ -> ()
  | _ -> raise (WrongFormat ("unexpected format of a term " ^ (Term.to_string t)))

let generate_from_bools outc name value =
  match value with
  | false ->
    fprintf outc "#define %s\n" name
  | true -> ()
