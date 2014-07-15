open Core.Std
open Network

(* module for creating dot-files *)
module Dot = Graph.Graphviz.Dot(struct
  include G (* use the graph module from above *)
  let edge_attributes (_, e, _) =
    [`Label (Constr.to_string e); `Color 4711]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = [`Shape `Circle]
  let vertex_name v =
    String.concat ["\""; Network.Node.to_string v; "\""]
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

let loop dot_output debug verbose limit filename =
  try
    Location.filename := filename;
    if debug then Log.set_output stdout;
    Log.logf "reading from %s" filename;
    (* get constraints from the file and generate boolean constraints *)
    Log.output_header "Syntax Analysis/Parsing";
    let constrs, logic = In_channel.with_file filename
        ~f:(fun inx -> Parser.parse Lexer.read (Lexing.from_channel inx)) in
    let constrs, logic = Transform.union constrs logic in
    Log.logf "%d constraints have been read:" (List.length constrs);
    List.iter constrs ~f:(fun el -> Log.logf "  %s" (Constr.to_string el));
    (* graph construction *)
    Log.output_header "Graph construction";
    let g = Network.constrs_to_graph_exn constrs in
    let _ = Option.value_map ~default:() ~f:(create_dot_output g) dot_output in
    (* create a network of constraints *)
    Log.output_header "Traversal order for constraints";
    let traversal_list = Network.traversal_order g in
    Log.output_header "Solving constraints";
    match Solver.solve_exn traversal_list logic verbose limit with
    | None -> print_endline "No solution is found"
    | Some (bools, terms) ->
      begin
        if verbose then
          printf "\nSolution:\n";
        String.Map.iter bools
          ~f:(fun ~key ~data ->
            if verbose then printf "  ";
            printf "%s = %s\n" key (if data then "true" else "false"));
        let f ~key ~data =
          if verbose then printf "  ";
          printf "$%s = %s\n" key (Term.to_string data) in
        if not (String.Map.is_empty bools) then
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
            f ~key:x ~data:Term.Nil
          )
      end
  with Lexer.Syntax_Error msg
     | Errors.Parsing_Error msg
     | Network.Topology_Error msg
     | Solver.Unsatisfiability_Error msg ->
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
      +> flag "-iterations"(optional int) ~doc:"integer maximum number of \
           iterations for the solver"
      +> anon ("filename" %:string)
    )
    (fun dot_output debug verbose iterations filename () ->
      loop dot_output debug verbose iterations filename)

let () =
  let picosat_version = Picosat.version () in
  let build_info = Printf.sprintf
    "Version: %s\nOCaml version: %s\nPicoSAT version: %s\nBuild platform: %s\n\
     Build date: %s"
    (Build.version) (Build.ocaml_version) picosat_version (Build.platform)
      (Build.compile_time) in
  Command.run ~version:(Build.version) ~build_info
    command
