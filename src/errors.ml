
let print_error ?(loc=None) fmt =
  let _ = match loc with
  | None -> ()
  | Some l -> let open Location in print fmt l in
  Format.fprintf fmt "Error: "

let error ?(loc=None) msg =
  let open Format in
  (print_error ~loc:loc str_formatter; flush_str_formatter ()) ^ msg

exception Parsing_Error of string
  
let parse_error msg start finish =
  let open Location in
  raise (Parsing_Error (error ~loc:(Some (pos_loc start finish)) msg))
