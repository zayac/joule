(** Transformations based on equations for terms *)

open Core.Std

(** [union t] makes a conversion of a [union] tuple (a tuple of 3 elements and
    the first one is a symbol [union]) if it is provided as an input term.
    Otherwise, [t] is left unmodified.

    The transformation is made only if the second and the third tuple arguments
    are records.

    Example: [(union { a: t1, b: t2 } {b: t3, c: t4 |
    var})] is converted to [{a: t1, b: (union t2 t3), c: t4 | var}]. *)
val union : Term.t -> Term.t * Logic.t
