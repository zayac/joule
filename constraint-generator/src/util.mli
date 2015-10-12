open Core.Std

val switch_of_alist_exn : (Logic.t * Term.t) list -> Term.t

val map_of_alist_exn : (string * (Logic.t * Term.t)) list -> (Cnf.t * Term.t) String.Map.t
