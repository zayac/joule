val set_output : out_channel -> unit
val disable_output : unit
val output_header : string -> unit


module type S = sig
  val log: string Lazy.t -> unit
  val logf: ('a, out_channel, unit, unit) format4 -> 'a
end

include S

module type SECTION = sig
  val section: string
end

module Make (Section: SECTION): S
(**
   This module aims to be used on the first line of each module:

   {| module Log = Log.Make(struct let section = "module-name" end) |}

*)
