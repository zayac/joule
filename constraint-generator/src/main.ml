open Core.Std

let return_value = ref 0

let in_terms_map = ref String.Map.empty
let out_terms_map = ref String.Map.empty
let constrs = ref []
let classes = ref String.Map.empty

let parse_terms_file dirname s =
  let file_name = String.concat [dirname; "/"; s; ".terms"] in
  let _ = Location.filename := s in
  In_channel.with_file file_name
    ~f:(fun inx ->
      let in_terms, out_terms, cstrs, cl = Parser.term_parse Lexer.read (Lexing.from_channel inx) in
      in_terms_map := String.Map.add !in_terms_map ~key:s ~data:in_terms;
      out_terms_map := String.Map.add !out_terms_map ~key:s ~data:out_terms;
      constrs := !constrs @ cstrs;
      classes := String.Map.merge !classes cl
                   ~f:(fun ~key data ->
                     match data with
                     | `Both _ -> raise (Generator.MessageFormat_Error ("class name " ^ key ^ " duplication"))
                     | `Left v
                     | `Right v -> Some v
                   )
    )

let loop filename =
  try
    let dirname = Filename.dirname filename in
    let _ = Location.filename := filename in
    let g = In_channel.with_file filename
              ~f:(fun inx -> Parser.netlist_parse Lexer.read (Lexing.from_channel inx))
    in
    let _ = Generator.G.iter_vertex (fun v -> parse_terms_file dirname v ) g in
    let ofname = String.concat [Filename.chop_extension filename; ".constraints"] in
    let outc = Out_channel.create ofname in
    List.iter !constrs
      ~f:(fun (l, r) ->
        fprintf outc "%s <= %s;\n" (Term.to_string l) (Term.to_string r)
      );
    String.Map.iter !in_terms_map
      ~f:(fun ~key ~data ->
        printf "%s: " key;
        Int.Map.iter data
          ~f:(fun ~key ~data ->
            printf "%d " key
          );
        printf "\n"
      );
    String.Map.iter !out_terms_map
      ~f:(fun ~key ~data ->
        printf "%s: " key;
        Int.Map.iter data
          ~f:(fun ~key ~data ->
            printf "%d " key
          );
        printf "\n"
      );
    Generator.G.iter_edges_e (fun (sname, (s, d), dname) ->
      match String.Map.find !out_terms_map sname with
      | None -> raise (Sys_error (sprintf "Cannot find component '%s'\n" sname))
      | Some lc ->
        begin
          match String.Map.find !in_terms_map dname with
          | None -> raise (Sys_error (sprintf "Cannot find component '%s'\n" dname))
          | Some rc ->
            begin
              match Int.Map.find lc s with
              | None -> raise (Sys_error (sprintf "Cannot find output channel %d in component '%s'\n" s sname))
              | Some left ->
                begin
                  match Int.Map.find rc d with
                  | None -> raise (Sys_error (sprintf "Cannot find output channel %d in component '%s'\n" d dname))
                  | Some right ->
                    fprintf outc "%s <= %s;\n" (Term.to_string left) (Term.to_string right)
                end
            end
        end
    ) g
  with Lexer.Syntax_Error msg
     | Errors.Parsing_Error msg
     | Generator.MessageFormat_Error msg
     | Sys_error msg ->
    let _ = return_value := 1 in
    Printf.eprintf "%s\n" msg

let command =
  Command.basic
    ~summary:"netlist"
    Command.Spec.(
      empty
      +> anon ("filename" %:string)
    )
    (fun filename () ->
      loop filename)

let _ =
  let _ = Command.run command in
  !return_value
