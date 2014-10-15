open Core.Std
open Logic

(** A generic non-ground term in Message Description Language *)
type t =
  | Nil
  | OrdinalInt of int
  | NominalInt of int
  | Symbol of string
  | Tuple of t list
  | List of t list * string option
  | Record of (Cnf.t * t) String.Map.t * string option
  | Choice of (Cnf.t * t) String.Map.t * string option
  | Var of string
  | Switch of t Cnf.Map.t

val compare_t : t -> t -> int
val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t

(** Lexicographical term comparison *)
include Comparable.S with type t := t

(** The exception is raised when the seniority relation is being resolved
    for two incomparable terms. *)
exception Incomparable_Terms of t * t with sexp

(** A hash function for a term *)
val hash : t -> int

(** Convert term to syntaxical representation *)
val to_string : t -> string

val to_formatted_string : ?id:int -> t -> string

(** This exception must be raised when non-ground term is provided as an
    argument for [is_nil_exn] function. *)
exception Non_Ground of t with sexp

(** [is_nil t] returns [true] only if a term [t] is ground and equals to
    [Nil] in the canonical form.
    Otherwise, it returns [false] for ground term.
    If [t] is not ground, then it throws an exception *)
val is_nil_exn : t -> bool
(** [is_nil t] does the same that [is_nil_exn t] does, but without throwing
    an exception. It returns [None] instead. *)
val is_nil : t -> bool option

(** [is_ground t] returns [true] only if term [t] does not contain term of
    the form [Var s] as children. *)
val is_ground : t -> bool

(** Provided a term [t], [canonize t] reduces it to the canonical
    form. Terms with variables [Var s] are not reduced. *)
val canonize : t -> t

(** [seniority t t'] compares two ground terms using the seniority relation.
    Throws an exception if one of the terms is not ground. *)
val seniority_exn : t -> t -> int

(** [get_vars t] returns a set of variable strings [s] from terms
    of the form [Var s] that are contained in [t] *)
val get_vars : t -> String.Set.t

(** Removes 'false' Boolean expressions from a switch.  Generates Boolean
    constraints if necessary. *)
val canonize_switch : t Logic.Map.t -> t Logic.Map.t * Logic.Set.t

(** Converts term to a well-formed term given a set of Boolean values *)
val to_wff : bool String.Map.t -> t -> t

(** Finds a join term for two terms *)
val join : t -> t -> (t * Cnf.Set.t) option

(** [none] is a bottom choice term equivalent to empty choice [(: :)] *)
val none : t

val is_choice : t -> bool
