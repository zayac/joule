open Core.Std

val switch_of_alist_exn : (Logic.t * Term.t) list -> Term.t * Cnf.t Set.Poly.t

val map_of_alist_exn : (string * (Logic.t * Term.t)) list -> (Cnf.t * Term.t) String.Map.t * Cnf.t Set.Poly.t
