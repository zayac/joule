open Core.Std

exception MessageFormat_Error of string

module Node : sig
  type t = string
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
  val default : t
end

module Edge : sig
  type t = int * int
  val compare : t -> t -> int
  val default : t
end

module G : sig
  include (module type of
    Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Node)(Edge))
end

(*val netlist : G.t*)
val in_term_map : (Term.t Core.Std.Int.Map.t) Core.Std.String.Map.t ref
val out_term_map : (Term.t Core.Std.Int.Map.t) Core.Std.String.Map.t ref

val match_in_channel : string -> int option * string
val match_out_channel : string -> int option * string

val gen_class_constraints :
     ((Cnf.t * Term.t) Core.Std.String.Map.t) Core.Std.String.Map.t
     -> Term.t
     -> Term.t
     -> (Term.t * Term.t) list
