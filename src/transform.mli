
open Core.Std

val initial_bool_variables : String.Set.t ref
val additional_bool_variables : String.Set.t ref

val initial_term_variables : String.Set.t ref
val additional_term_variables : String.Set.t ref

val union : (Term.t * Term.t) list -> Cnf.t -> (Term.t * Term.t) list * Cnf.t
