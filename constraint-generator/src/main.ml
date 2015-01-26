open Core.Std

let return_value = ref 0

let loop filename =
  try
    let dirname = Filename.dirname filename in
    let netlist = In_channel.with_file filename
      ~f:(fun inx -> Parser.netlist_parse Lexer.read (Lexing.from_channel inx)) in
    let interfaces = ref String.Map.empty in
    let ofname = String.concat [Filename.chop_extension filename; ".constraints"] in
    let outc = Out_channel.create ofname in
    List.iter netlist
      ~f:(fun (inv, outv) ->
        let constrs = ref [] in
        let classes = ref String.Map.empty in
        let add_interface fn =
          let file_name = String.concat [dirname; "/"; fn; ".terms"] in
          let _ = Location.filename := file_name in
          let data = In_channel.with_file file_name
                       ~f:(fun inx ->
                         let terms, cstrs, cl = Parser.term_parse Lexer.read (Lexing.from_channel inx) in
                         constrs := !constrs @ cstrs;
                         classes := String.Map.merge !classes cl
                           ~f:(fun ~key data ->
                                match data with
                                | `Both _ -> raise (Generator.MessageFormat_Error ("class name " ^ key ^ " duplication"))
                                | `Left v
                                | `Right v -> Some v
                              );
                        terms
                      )
          in
          interfaces := String.Map.add !interfaces ~key:fn ~data
        in
        if not (String.Map.mem !interfaces inv) then
          add_interface inv;
        if not (String.Map.mem !interfaces outv) then
          add_interface outv;
        let _, out_term = String.Map.find_exn !interfaces inv in
        let in_term, _ = String.Map.find_exn !interfaces outv in
        let new_cstrs = Generator.gen_class_constraints !classes out_term in_term in
        constrs := !constrs @ new_cstrs;
        List.iter !constrs
          ~f:(fun (l, r) ->
            fprintf outc "%s <= %s;\n" (Term.to_string l) (Term.to_string r)
        );
        fprintf outc "%s\n<=\n%s;\n\n" (Term.to_formatted_string out_term) (Term.to_formatted_string in_term);
      );
    Out_channel.flush outc;
    Out_channel.close outc
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
