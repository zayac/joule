open Printf

(* defaults *)
let output = ref None

let set_output o =
  output := Some o

let disable_output =
  output := None

let section_width = ref 0

let output_header s =
  match !output with
  | None -> ()
  | Some out ->
    let width = 80 in
    let space = (width - (String.length s)) / 2 - 1 in
    let filling = String.make space '=' in
    let header = String.concat " " [filling; s; filling] in
    if String.length header = 79 then
      fprintf out "%s=\n%!" header
    else
    fprintf out "%s" header

module type S = sig
  val log: string Lazy.t -> unit
  val logf: ('a, out_channel, unit, unit) format4 -> 'a
end

module type SECTION = sig
  val section: string
end

module Make (S: SECTION) = struct

  let () =
    if S.section <> "" then
      section_width := max (String.length S.section) !section_width

  let section =
    if !section_width = 0 then ""
    else sprintf "%-*s " !section_width S.section

  let log lazy_msg =
    match !output with
    | None -> ()
    | Some out ->
      fprintf out "%s%s\n%!" section (Lazy.force lazy_msg)

  let logf fmt =
    let print o = fprintf o "\n%!" in
    match !output with
    | None ->  ikfprintf print stdout fmt
    | Some out ->
      begin
        fprintf out "%s" section;
        kfprintf print out fmt
      end
end

include Make (struct
  let section = ""
  let width = 0
end)
