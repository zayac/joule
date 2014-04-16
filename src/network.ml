open Core.Std

module Log = Log.Make(struct let section = "network:" end)

exception Topology_Error of string

let topo_error msg =
  raise (Topology_Error (Errors.error msg))

type node =
  | Env_In
  | Env_Out
  | Internal of String.Set.t
  with sexp, compare

module Node = struct
  type t = node
  let hash = Hashtbl.hash
  let to_string = function
    | Env_In -> "env_in"
    | Env_Out -> "env_out"
    | Internal s ->
      Printf.sprintf "{%s}" (String.concat ~sep:", " (String.Set.to_list s))
  module T = struct
    type t = node with sexp, compare
  end
  include Comparable.Make(T)
end

module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Node)(Constr)

let constr_to_edge c =
  let src, dest = Constr.get_vars c in
  (if String.Set.is_empty src then Env_In else Internal src), c,
    (if String.Set.is_empty dest then Env_Out else Internal dest)

let edge_to_constr (_, e, _) = e

(* merge vertices that contain same variables *)
let merge_vertices g =
  let g' = ref g in
  let merge = function
  | Internal v as vnode ->
    let update = function
    | Internal v' as vnode' ->
      let module SS = String.Set in
      if SS.subset v v' && not (SS.equal v v') then
        begin
          Log.logf "merging node %s with node %s" (Node.to_string vnode) 
            (Node.to_string vnode');
          let succ_edges = G.succ_e !g' vnode in
          let pred_edges = G.pred_e !g' vnode in
          List.iter ~f:(fun (src, e, dest) ->
            g' := G.add_edge_e !g' (G.E.create (Internal v') e dest);
            g' := G.remove_edge_e !g' (src, e, dest)) succ_edges;
          List.iter ~f:(fun (src, e, dest) ->
            g' := G.add_edge_e !g' (G.E.create src e (Internal v'));
            g' := G.remove_edge_e !g' (src, e, dest)) pred_edges;
          if G.in_degree !g' (Internal v) = 0
            && G.out_degree !g' (Internal v) = 0 then
            g' := G.remove_vertex !g' (Internal v)
        end
    | Env_In | Env_Out -> () in
    G.iter_vertex update !g'
  | Env_In | Env_Out -> () in
  G.iter_vertex merge !g';
  !g'

(* checks for the presence of cycles that cause infinite list traversals *)
(*let infinite_lists_cycles g =*)
  (*let module C = Graph.Components.Make(G) in*)
  (*let lst = C.scc_list g in*)
  (*List.iter lst*)
    (*~f:(fun c ->*)
      (*if List.length c = 1 then ()*)
      (*else*)


(* check that all internal vertices (i.e. variables) are connected to either
   other vertices or the environment *)
let check_connectivity_exn g =
  let check = function
  | Internal x as vnode ->
    let module SS = String.Set in
    (* accumulate all vertices from the left and right parts of the
       neighbouring constraints *)
    let succ_vars = G.fold_succ_e
      (fun (_, (l, _), _) acc -> SS.union acc (Term.get_vars l)) g vnode SS.empty in
    let pred_vars = G.fold_pred_e
      (fun (_, (_, r), _) acc -> SS.union acc (Term.get_vars r)) g vnode SS.empty in
    let succ_diff = SS.diff x succ_vars in
    let pred_diff = SS.diff x pred_vars in
    if not (SS.is_empty succ_diff) then
      Log.log (lazy (Errors.constraint_missing `Upper succ_diff));
    if not (SS.is_empty pred_diff) then
      raise (topo_error (Errors.constraint_missing `Lower pred_diff));
  | Env_In | Env_Out -> () in
  G.iter_vertex check g

let constrs_to_graph_exn cstrs =
  let env_in_found = ref false in
  let env_out_found = ref false in
  let g = ref G.empty in
  let add c =
    let src, edge, dest = constr_to_edge c in
    let vertex = G.E.create src edge dest in
    if src = Env_In then env_in_found := true;
    if dest = Env_Out then env_out_found := true;
    Log.logf "adding edge (%s, \"%s\", %s) to the graph" (Node.to_string src)
      (Constr.to_string c) (Node.to_string dest);
    g := G.add_edge_e !g vertex in
  List.iter ~f:add cstrs;
  if not !env_in_found then
    topo_error "The input of the network is not connected to the environment";
  if not !env_out_found then
    Log.logf "The output of the network is not connected to the environment";
  Log.logf "merging nodes corresponding to the same set of vertices";
  g := merge_vertices !g;
  check_connectivity_exn !g;
  !g

let order g =
  let q = Queue.create () in
  G.iter_vertex
    (fun v ->
      if Int.(G.out_degree g v = 0) then Queue.enqueue q v
    ) g;
  let traversed = ref Node.Set.empty in
  let lst = ref [] in
  while not (Queue.is_empty q) do
    let v = Queue.dequeue_exn q in
    if not (Node.Set.mem !traversed v) then
      begin
        G.iter_pred_e
          (fun (s, e, _) ->
            lst := e :: !lst;
            Queue.enqueue q s
          ) g v;
      traversed := Node.Set.add !traversed v
      end
  done;
  List.rev !lst

let traversal_order g =
  let lst = order g in
  let counter = ref 1 in
  List.iter
    ~f:(fun x ->
      Log.logf "%d. %s" !counter (Constr.to_string x);
      counter := !counter + 1
    )
    lst;
  lst

