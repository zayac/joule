open Core.Std

(** A constraint for terms *)
type t = Term.t * Term.t

(** A constraint on variable *)
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

(** Convert contraint to syntaxical representation *)
val to_string : t -> string

(** [vars t] returns two sets of variables from terms of the form [Vars s] in
    the left and right parts of the constraint. *)
val get_vars : t -> String.Set.t * String.Set.t

val print_constraints : var_bounds String.Map.t -> unit

exception No_Solution of string

val substitute : var_bounds String.Map.t -> bool String.Map.t -> Term.t String.Map.t
