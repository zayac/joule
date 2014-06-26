open Core.Std

(** Finds solution for a SAT problem *)
val solve : Cnf.t -> bool String.Map.t option

(** Checks whether two Boolean expressions are equivalent *)
val equal : Cnf.t -> Cnf.t -> bool
