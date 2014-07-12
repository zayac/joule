
open Core.Std

val bool_variables : String.Set.t ref

val term_variables : String.Set.t ref

val union : (Term.t * Term.t) list -> Cnf.t -> (Term.t * Term.t) list * Cnf.t
