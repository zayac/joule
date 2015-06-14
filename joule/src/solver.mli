open Core.Std

(** The exception is thrown when it is found that the network has unsatisfied
    constraints. *)
exception No_Solution of string

(** A unification procedure that finally returns unified variable constraints
    as well as logical constraints that need to be unified. *)
val solve_exn : Constr.t list -> Cnf.t Set.Poly.t -> bool -> int option
  -> (bool String.Map.t * Term.t String.Map.t) option
