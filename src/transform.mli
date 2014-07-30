
open Core.Std

val initial_bool_variables : String.Set.t ref
val additional_bool_variables : String.Set.t ref

val initial_term_variables : String.Set.t ref
val union_term_variables : String.Set.t ref

(** transform a union term to a form specified in the description of [union]
    function and generated additional constraints for terms. *)
val to_union : Cnf.t ref -> Term.t -> Term.t * (Term.t * Term.t) list

(** [union t l] transforms a set of constraints [t] under boolean assumptions
    [l] that contain union terms of the form [(union t t')].

    Every [(union t t')] is replaced by a term
    [<a: $u, (not a): (union t t')>], where [a] is a free boolean variable and
    [$u] is a free term variable.  Furthermore, the following constraints are
    generated:

    [<a: $z, (not a): t> <= t]
    [<a: $z, (not a): t'> <= t']

    Additional boolean constraints may be generated.

    Notice: the function transforms module variables
    [additional_bool_variables] and [union_term_variables]. *)
val union : (Term.t * Term.t) list -> Cnf.t -> (Term.t * Term.t) list * Cnf.t
