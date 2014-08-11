open Core.Std

type t =
  { var : string;
    logic : Cnf.t;
  }

val compare_t : t -> t -> int
val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t

include Comparable.S with type t := t
