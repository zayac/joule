open Core.Std

(** The exception is thrown when it is found that the network has unsatisfied
    constraints. *)
exception Unsatisfiability_Error of string

val iteration_limit : int

(** A unification procedure that finally returns unified variable constraints
    as well as logical constraints that need to be unified. *)
val solve_exn : (Term.t list * Term.t list) list -> Term.t String.Map.t option

(*
val solve_senior_multi_exn : int -> [> `Lower | `Upper] ->
  (Logic.t Term.Map.t * Logic.t Term.Map.t) String.Map.t * Logic.Set.t ->
  Logic.t Term.Map.t -> Logic.t Term.Map.t ->
  (Logic.t Term.Map.t * Logic.t Term.Map.t) String.Map.t * Logic.Set.t

*)
