(* A representation of a constraint (seniority relation) *)

open Core.Std

(** A constraint is a pair of terms *)
type t = Term.t * Term.t

(** A collection of terms guarded by a Boolean expression (in CNF) that are
    potential upper bounds for a single variable *)
type var_bounds = Term.t Cnf.Map.t

val compare_t : t -> t -> int
val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t

(** Lexicographical constraint comparison *)
include Comparable.S with type t := t

(** A hash function for a constraint *)
val hash : t -> int

(** The default value *)
val default : t

(** Converts a Boolean expression to string.  An optional argument [rel]
    specifies a string represenation for the seniority relation symbol *)
val to_string : ?rel:string -> t -> string

(** [get_vars t] returns two sets of variables from terms of in the left and
    right parts of the constraint *)
val get_vars : t -> String.Set.t * String.Set.t

(** [get_flags t] returns two sets of Boolean variables from terms in the left
    and right parts of the constraint *)
val get_flags : t -> String.Set.t * String.Set.t

(** A debugging function for printing the content of the upper bound array *)
val print_constraints : var_bounds String.Map.t -> unit

(** [substitute ub bv] replaces Boolean variables in [ub] with ground Boolean
    values from [bv] and returns a map of well-formed terms that represent
    upper bounds for each variable in the map *)
val substitute : var_bounds String.Map.t -> bool String.Map.t -> Term.t String.Map.t
