open Core.Std

(** A collection of terms converters specified in terms of the equality
    logic. *)

(** [union t] makes a convertion of [union] tuple if it is provided as an input
    term.

    The convertion is performed if the input term is of type [(union [l1] [l2 |
    var ])] to [[l1 l2 | v]].

    The same works for a record: [(union { a: t1, b: t2 } {b: t3, c: t4 |
    var})] is converted to [{a: t1, b: (union t2 t3), c: t4 | var}]. *)
val union : Term.t -> Term.t * Logic.t
