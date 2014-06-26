(** An auxiliary module to construct neat strings that represent positions in
    the code. *)

open Lexing

(** A location structure *)
type location = {
    loc_start: position;
    loc_end: position;
  }

(** A file name that is added to strings that represent positions *)
val filename : string ref

(** Constructs a [location] structure from start and end position *)
val pos_loc : position -> position -> location

(** Constructs a [location] structure from [Lexing.lexbuf] *)
val curr : lexbuf -> location

(** Prints a neat string that represents a position given a formatter *)
val print : Format.formatter -> location -> unit
