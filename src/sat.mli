open Core.Std

(** Finds any solution for a SAT problem *)
val solve : Cnf.t -> bool String.Map.t option

(** Finds a SAT problem solution with maximum number of [true] variables *)
val solve_max : Cnf.t -> bool String.Map.t option

(** Checks whether two Boolean expressions are equivalent *)
val equal : Cnf.t -> Cnf.t -> bool
