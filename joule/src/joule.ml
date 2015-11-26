open Core.Std
open Network
open Statistics

let return_value = ref 0

(* module for creating dot-files *)
module Dot = Graph.Graphviz.Dot(struct
  include G (* use the graph module from above *)
  let escape_quotes s =
    String.concat
      (List.map ~f:(fun x -> if x = '"' then "\\\"" else String.of_char x)
         (String.to_list s))
  let edge_attributes (_, e, _) =
    [`Label (escape_quotes (Constr.to_string e)); `Color 4711]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = [`Shape `Circle]
  let vertex_name v =
    String.concat
      ["\"";
       escape_quotes (Network.Node.to_string v);
       "\""]
  let default_vertex_attributes _ = [ `Style `Filled; `Color 0x6495ED;
    `Fontcolor 0xFFFFFF;
    `Fontsize 10;
    `Fontname "Helvetica"]
  let default_edge_attributes _ = [ `Fontcolor 0x000000; `Fontsize 11;
    `Fontname "Helvetica"]
  let graph_attributes _ = []
  end)

let create_dot_output g dot_output =
  Log.logf "a network graph is saved in .dot format in '%s'" dot_output;
  Out_channel.with_file dot_output
    ~f:(fun oc -> Dot.output_graph oc g)

let loop dot_output debug verbose stats format_output limit filename =
  try
    Location.filename := filename;
    Statistics.t.name <- filename;
    if debug then Log.set_output stdout;
    Log.logf "reading from %s" filename;
    (* get constraints from the file and generate boolean constraints *)
    Log.output_header "Syntax Analysis/Parsing";
    let constrs, logic = In_channel.with_file filename
                           ~f:(fun inx -> Parser.parse Lexer.read (Lexing.from_channel inx)) in
    Statistics.t.constraints <- List.length constrs;
    let constrs, logic = Transform.union constrs logic in
    Statistics.t.aux_constraints <- (List.length constrs) - Statistics.t.constraints;
    Statistics.t.term_variables <-
      String.Set.length
        (List.fold ~init:String.Set.empty
           ~f:(fun acc t ->
             let v, v' = Constr.get_vars t in
             String.Set.union acc (String.Set.union v v')
           ) constrs
        );
    Statistics.t.flags <-
      String.Set.length
        (List.fold ~init:String.Set.empty
           ~f:(fun acc t ->
             let v, v' = Constr.get_flags t in
             String.Set.union acc (String.Set.union v v')
           ) constrs
        );
    if verbose then
      begin
        printf "The network contains %d constraints:\n" (List.length constrs);
        List.iter constrs ~f:(fun el -> printf "  %s\n" (Constr.to_string el))
      end;
    (* graph construction *)
    Log.output_header "Graph construction";
    let g = Network.constrs_to_graph_exn constrs in
    let _ = Option.value_map ~default:() ~f:(create_dot_output g) dot_output in
    (* create a network of constraints *)
    Log.output_header "Traversal order for constraints";
    let traversal_list = Network.traversal_order g in
    Log.output_header "Solving constraints";
    match Solver.solve_exn traversal_list logic verbose limit with
    | None ->
      let _ = return_value := 1 in
      print_endline "No solution is found"
    | Some (bools, terms) ->
      begin
        if verbose then
          printf "\nExtended solution:\n";
        String.Map.iter bools
          ~f:(fun ~key ~data ->
            if verbose then
              printf "  %s = %s\n" key (if data then "true" else "false")
            else if String.Set.mem !Transform.initial_bool_variables key then
              printf "%s = %s\n" key (if data then "true" else "false")
          );
        let f ~key ~data =
          let to_string =
            if format_output then
              Term.to_formatted_string ~id:0
            else
              Term.to_string
          in
          if verbose then
            printf "  $%s = %s\n" key (to_string data)
          else if String.Set.mem !Transform.initial_term_variables key then
            printf "$%s = %s\n" key (to_string data) in
        if (not verbose && not (String.Set.is_empty !Transform.initial_bool_variables)) ||
          (verbose && not (String.Map.is_empty bools)) then
          print_newline ();
        String.Map.iter terms ~f;
        (* output also values for variables without any constraints *)
        let all_vars = G.fold_vertex
          (fun v acc ->
            match v with
            | Env_Out
            | Env_In -> acc
            | Internal x -> String.Set.union x acc
          ) g String.Set.empty in
        String.Set.iter
          (String.Set.diff all_vars (String.Set.of_list (String.Map.keys terms)))
          ~f:(fun x ->
            if Term.is_up_var x then f ~key:x ~data:Term.none
            else f ~key:x ~data:Term.Nil
          );
        if stats then
          print_endline (Statistics.str ())
      end
  with Lexer.Syntax_Error msg
     | Errors.Parsing_Error msg
     | Network.Topology_Error msg
     | Errors.Unsatisfiability_Error msg
     | Sys_error msg ->
    let _ = return_value := 1 in
    Printf.eprintf "%s\n" msg

let command =
  Command.basic
    ~summary:Build.synopsis
    ~readme:(fun () -> Build.description)
    Command.Spec.(
      empty
      +> flag "-dot-output" (optional string) ~doc:"string save a KPN graph \
        graph in .dot format and store in a file provided as an argument"
      +> flag "-debug" no_arg ~doc:" print debug information"
      +> flag "-verbose" no_arg ~doc:" print auxiliary computation results"
      +> flag "-stats" no_arg ~doc:" print statistics about execution"
      +> flag "-format-output" no_arg ~doc:" turn on indentation for result terms"
      +> flag "-iterations"(optional int) ~doc:"integer maximum number of \
           iterations for the solver"
      +> anon ("filename" %:string)
    )
    (fun dot_output debug verbose stats format_output iterations filename () ->
       loop dot_output debug verbose stats format_output iterations filename)

let _ =
  let picosat_version = Picosat.version () in
  let build_info = Printf.sprintf
    "Version: %s\nOCaml version: %s\nPicoSAT version: %s\nBuild platform: %s\n\
     Build date: %s"
    (Build.version) (Build.ocaml_version) picosat_version (Build.platform)
      (Build.compile_time) in
  let _ = Command.run ~version:(Build.version) ~build_info command in
  !return_value
