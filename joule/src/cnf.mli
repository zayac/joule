(** Boolean expressions in Conjunctive Normal Form *)

open Core.Std

(*module CSet = Set.Poly*)

(** A Boolean expression in CNF *)
type t = int

val compare_t : t -> t -> int
val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t

include Comparable.S with type t := t

(** Constructs a Boolean expression that evaluates to [false] *)
val make_false : t
(** Constructs a Boolean expression that evaluates to [true] *)
val make_true : t
(** Constructs a Boolean expression consisting of one variable *)
val make_var : string -> t
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
val evaluate : bool Int.Map.t -> t -> bool
(** Constructs a disjunction of Boolean expressions that are provided in a list *)
val list_of_disjuncts : t list -> t
(** Tries to reduce and to simplify a Boolean expression *)
(*val simplify : t -> t*)
(** Given a list of Boolean expressions, constructs a Boolean expression that
    excludes all pairwise combinations of list elements *)
val pairwise_not_and : t list -> t

(** {2 Logical operators } *)

(** Disjunction *)
val c_or : t -> t -> t
val ( + ) : t -> t -> t
val c_lor : t list -> t
(** Conjunction *)
val c_and : t -> t -> t
val ( * ) : t -> t -> t
val c_land : t list -> t
(** Negation *)
val c_not : t -> t
val ( ~- ) : t -> t
(** Implication *)
val ( ==> ) : t -> t -> t
(** Equivalence *)
val c_eq : t -> t -> t
val ( <=> ) : t -> t -> t
(** NOT OR *)
val c_nor : t -> t -> t
(** Exclusive OR *)
val c_xor : t -> t -> t
val c_nand : t -> t -> t

val set_to_t : t Core.Std.Set.Poly.t -> t

val get_cnf : unit -> t list list

val int_to_var : t -> string option

(*val int_string_var_table: (t, string) Core.Std.Hashtbl.Poly.t*)
