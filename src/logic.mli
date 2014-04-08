(** A wrapper over PicoSAT to ease solution of a SAT problem.  Boolean formulas
    can be provided in an arbitrary form (not CNF).  The solver can find all
    models instead of a single one. *)

open Core.Std

(** Basic representation of a boolean formula. *)
type t =
  | False
  | True
  | Not of t
  | Or of t * t
  | And of t * t
  | Var of string

type cnf =
  | Valid
  | Unsatisfiable
  | Satisfiable of t list list

type solution =
  | Any
  | Solutions of bool String.Map.t list

val compare_t : t -> t -> int
val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t

include Comparable.S with type t := t

(** Boolean formula string representation. *)
val to_string : t -> string

val set_to_string : Set.t -> string
val is_ground : t -> bool

val list_of_disjuncts : t list -> t

(** Reduce boolean formula.  Unnecessary [True], [False] terms are removed from
    the formula, double negations are removed. *)
val simplify : t -> t

(** Convert formula to Conjunctive Normal Form (CNF).  Returns a list of
    conjunctions.  A formula is simplified before conversion. *)
val to_cnf : t -> cnf

(** Find all assignments to variables in a conjunction of formulas in the list.
    In case of satisfiability, the result is a list of all possible solutions,
    i.e. a solution is a list of variable-value pairs, otherwise the result is
    [None]. *)
val all_solutions : Set.t -> bool String.Map.t list option

(** Find only a single (any) assignment for all variables.  In case of
    satisfiability, the result is a list of variable-value pairs, otherwise the
    result is [None]. *)
val solve : Set.t -> bool String.Map.t option

val evaluate : bool String.Map.t -> t -> t

(** {2 Logical operators } *)

(** Disjunction *)
val ( + ) : t -> t -> t

(** Conjunction *)
val ( * ) : t -> t -> t

(** Negation *)
val ( ~- ) : t -> t

(** Implication *)
val ( ==> ) : t -> t -> t

(** Converse implication *)
val ( <== ) : t -> t -> t

(** Equivalence *)
val ( <=> ) : t -> t -> t

