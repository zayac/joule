(** A Kahn Process Network construction and verification module *)
open Core.Std

exception Topology_Error of string

(** Each node in the network graph is basically a set of internal variables or
    alternatively environment input/output channel (we need to distinguish them
    explicitly. *)
type node =
  | Env_In
  | Env_Out
  | Internal of String.Set.t

val compare_node : node -> node -> int
val node_of_sexp : Sexplib.Sexp.t -> node
val sexp_of_node : node -> Sexplib.Sexp.t

(** Node module for the network graph *)
module Node : sig
  type t = node
  val hash : t -> int
  val to_string : t -> string
  include Comparable.S with type t := node
end

(** The graph representation of the network:
    - a persistent (i.e. immutable) data structure;
    - a directed graph;
    - with labeled edges *)
module G : sig
  include (module type of
    Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Node)(Constr))
end

(** Converts a constraint to an edge *)
val constr_to_edge : Constr.t -> G.edge

(** Converts an edge to a constraint *)
val edge_to_constr: G.edge -> Constr.t

(** [constrs_to_graph_exn cl] converts a list of constraints into a graph
    representation. The list provided must be consistent in terms of the
    topology (connectivity), otherwise the exception [Topology_Error] will be
    thrown. *)
val constrs_to_graph_exn : Constr.t list -> G.t

(** Find a traversal order for constraints from the environment source to the
    sink. The order is guaranteed to be correct if all nodes are reachable from
    both source and sink.

    Cycles in graphs are allowed. Specification is the following: if vertex x
    is visited before vertex y then either there is a path from x to y, or
    there is no path from y to x. In the particular case of a DAG, this
    simplifies to: if there is an edge from x to y, then x is visited before
    y. *)
val traversal_order : G.t -> Constr.t list
