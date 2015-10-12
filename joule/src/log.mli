(** Logging facilities *)

(** Globally sets an output stream for logs *)
val set_output : out_channel -> unit
(** Disables logging *)
val disable_output : unit
(** Prints a neat header that represents a new work phase of a program *)
val output_header : string -> unit

module type S = sig
  (** Lazy logging *)
  val log: string Lazy.t -> unit
  (** Active logging *)
  val logf: ('a, out_channel, unit, unit) format4 -> 'a
end

include S

module type SECTION = sig
  val section: string
end

module Make (Section: SECTION): S
(**
  This module aims to be used on the first line of each module:

  [ module Log = Log.Make(struct let section = "module-name" end) ]

*)
