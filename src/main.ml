open Core.Std

let loop dot_output debug verbose filename =
  Location.filename := filename;
  if debug then Log.set_output stdout;
  Log.logf "reading from %s" filename;
  Log.output_header "Syntax Analysis/Parsing";
  let constrs, logic = In_channel.with_file filename
    ~f:(fun inx -> Parser.parse Lexer.read (Lexing.from_channel inx)) in
  Log.logf "%d constraints have been read:" (List.length constrs);
  List.iter constrs ~f:(fun el -> Log.logf "\t%s" (Constr.to_string el))

let command =
  Command.basic
    ~summary:"Resolve constraints from AstraKahn Term Algebra (AKTA)"
    ~readme:(fun () -> "More detailed information")
    Command.Spec.(
      empty
      +> flag "-dot-output" (optional string) ~doc:"string output network \
        graph in dot format to a file provided as the argument"
      +> flag "-debug" no_arg ~doc:" print debug information"
      +> flag "-verbose" no_arg ~doc:" print preliminary computation \
        results"
      +> anon ("filename" %:string)
    )
    (fun dot_output debug verbose filename () ->
      loop dot_output debug verbose filename)

let () =
  let picosat_version = Picosat.version () in
  let build_info = Printf.sprintf
    "Version: %s\nOcaml version: %s\nPicoSAT version: %s\nBuild platform: %s\n\
     Build date: %s"
    (Version.version) (Version.ocaml_version) picosat_version (Version.platform)
      (Version.compile_time) in
  Command.run ~version:(Version.version) ~build_info
    command
