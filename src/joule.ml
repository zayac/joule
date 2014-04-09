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
  let default_vertex_attributes _ = [ `Style `Filled;
    `Color 0x6495ED;
    `Fontcolor 0xFFFFFF;
    `Fontsize 10;
    `Fontname "Helvetica"]
  let graph_attributes _ = []
  end)

let create_dot_output g dot_output =
  Log.logf "outputting the network graph in dot format to %s" dot_output;
  Out_channel.with_file dot_output
    ~f:(fun oc -> Dot.output_graph oc g)

let loop dot_output debug verbose filename =
  try
    Location.filename := filename;
    if debug then Log.set_output stdout;
    Log.logf "reading from %s" filename;
    (* get constraints from the file and generate boolean constraints *)
    Log.output_header "Syntax Analysis/Parsing";
    let constrs, logic = In_channel.with_file filename
        ~f:(fun inx -> Parser.parse Lexer.read (Lexing.from_channel inx)) in
    Log.logf "%d constraints have been read:" (List.length constrs);
    List.iter constrs ~f:(fun el -> Log.logf "  %s" (Constr.to_string el));
    (* graph construction *)
    Log.output_header "Graph construction";
    let g = Network.constrs_to_graph_exn constrs in
    let _ = Option.value_map ~default:() ~f:(create_dot_output g) dot_output in
    (* create a network of constraints *)
    Log.output_header "Traversal order for constraints";
    let traversal_list = Network.traversal_order g in
    Log.output_header "Finding upper bounds for constraints";
    match Solver.solve_exn traversal_list logic with
    | None -> print_endline "No satisfiable model is found"
    | Some x ->
      begin
        let f ~key ~data =
          printf "$%s = %s\n" key (Term.to_string data) in
        String.Map.iter x ~f;
        (* output also values for variables without any constraints *)
        let all_vars = G.fold_vertex
          (fun v acc ->
            match v with
            | Env_Out
            | Env_In -> acc
            | Internal x -> String.Set.union x acc
          ) g String.Set.empty in
        String.Set.iter
          (String.Set.diff all_vars (String.Set.of_list (String.Map.keys x)))
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
