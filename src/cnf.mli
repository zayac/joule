open Core.Std

module CSet :
  sig
    include (module type of Set.Poly)
  end

type t = Logic.Set.t CSet.t

val compare_t : t -> t -> int
val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t

include Comparable.S with type t := t

val make_false : t
val make_true : t
val is_false : t -> bool
val is_true : t -> bool
val to_string : t -> string
val is_ground : t -> bool
val from_logic : Logic.t -> t
val evaluate : bool String.Map.t -> t -> Logic.t
val ( * ) : t -> t -> t
val (+) : t -> t -> t
val (~-) : t -> t
val (==>) : t -> t -> t
val (<==) : t -> t -> t
val (<=>) : t -> t -> t
val list_of_disjuncts : t list -> t
val simplify : t -> t
