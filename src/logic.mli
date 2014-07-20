(** Boolean expressions in an arbitrary form *)

open Core.Std

(** Basic representation of a boolean formula. *)
type t =
  | False
  | True
  | Not of t
  | Or of t * t
  | And of t * t
  | Var of string

val compare_t : t -> t -> int
val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t

include Comparable.S with type t := t

(** Converts a Boolean expression to string.  Auxiliary optional arguments specify
    - [sand] -- string representation of the conjunction symbol;
    - [sor] -- string representation of the disjunction symbol;
    - [snot] -- string representation of the negation symbol;
    - [vprefix] -- a prefix string that is inserted before every Boolean variable.

*)
val to_string : ?sand:string -> ?sor:string -> ?snot:string -> ?vprefix:string -> t -> string
(** Checks whether a Boolean expression does not contain variables *)
val is_ground : t -> bool
(** Replaces variables in a Boolean expression [t] with the ones provided in
    the map and simplifies the expression *)
val evaluate : bool String.Map.t -> t -> t
(** Constructs a disjunction of Boolean expressions that are provided in a list *)
val list_of_disjuncts : t list -> t
(** Tries to reduce and to simplify a Boolean expression *)
val simplify : t -> t

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

val true_set : Set.t
val false_set : Set.t
