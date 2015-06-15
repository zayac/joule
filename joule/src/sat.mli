open Core.Std

(** Finds any solution for a SAT problem *)
val solve : Cnf.t -> bool Int.Map.t option

(** Finds a SAT problem solution with maximum number of [true] variables *)
val solve_max : ?verbose:bool -> Cnf.t -> bool Int.Map.t option

(** Checks whether two Boolean expressions are equivalent *)
val equal : Cnf.t -> Cnf.t -> bool

val evaluate : bool Int.Map.t -> Cnf.t -> bool
