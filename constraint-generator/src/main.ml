open Core.Std

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
        let add_interface fn =
          let file_name = String.concat [dirname; "/"; fn; ".terms"] in
          let _ = Location.filename := file_name in
          let data = In_channel.with_file file_name
                      ~f:(fun inx ->
                        let terms, cstrs = Parser.term_parse Lexer.read (Lexing.from_channel inx) in
                        List.iter cstrs
                          ~f:(fun (l, r) ->
                            fprintf outc "%s <= %s;\n" (Term.to_string l) (Term.to_string r)
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
        fprintf outc "%s\n<=\n%s;\n\n" (Term.to_formatted_string out_term) (Term.to_formatted_string in_term);
      );

    Out_channel.flush outc;
    Out_channel.close outc
  with Lexer.Syntax_Error msg
     | Errors.Parsing_Error msg
     | Sys_error msg ->
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

let () =
  Command.run command
