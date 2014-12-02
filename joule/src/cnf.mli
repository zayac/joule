(** Boolean expressions in Conjunctive Normal Form *)

open Core.Std

module CSet :
  sig
    include (module type of Set.Poly)
  end

(** A Boolean expression in CNF *)
type t = Logic.Set.t CSet.t

val compare_t : t -> t -> int
val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t

include Comparable.S with type t := t

(** Constructs a Boolean expression that evaluates to [false] *)
val make_false : t
(** Constructs a Boolean expression that evaluates to [true] *)
val make_true : t
(** Checks whether a Boolean expression is {e syntactically} identical to [false] *)
val is_false : t -> bool
(** Checks whether a Boolean expression is {e syntactically} identical to [true] *)
val is_true : t -> bool

val to_string : t -> string

(** Convert a Boolean expression to string.  Auxiliary optional arguments specify
    - [sand] -- string representation of the conjunction symbol;
    - [sor] -- string representation of the disjunction symbol;
    - [snot] -- string representation of the negation symbol;
    - [vprefix] -- a prefix string that is inserted before every Boolean variable.

*)
val to_friendly_string : ?sand:string -> ?sor:string -> ?snot:string -> ?vprefix:string -> t -> string
(** Checks whether a Boolean expression does not contain variables *)
val is_ground : t -> bool
(** Constructs a Boolean expression in CNF form from a logical formula *)
val from_logic : Logic.t -> t
(** Replaces variables in a Boolean expression [t] with the ones provided in
    the map and simplifies the expression *)
val evaluate : bool String.Map.t -> t -> Logic.t
(** Constructs a disjunction of Boolean expressions that are provided in a list *)
val list_of_disjuncts : t list -> t
(** Tries to reduce and to simplify a Boolean expression *)
val simplify : t -> t
(** Given a list of Boolean expressions, constructs a Boolean expression that
    excludes all pairwise combinations of list elements *)
val pairwise_not_and : t list -> t

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
