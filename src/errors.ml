open Core.Std

let print_error ?(loc=None) fmt =
  let _ = match loc with
  | None -> ()
  | Some l -> let open Location in print fmt l in
  Format.fprintf fmt "Error: "

let error ?(loc=None) msg =
  let open Format in
  (print_error ~loc:loc str_formatter; flush_str_formatter ()) ^ msg

exception Parsing_Error of string
exception Unsatisfiability_Error of string

let unsat_error msg =
  raise (Unsatisfiability_Error (error msg))
  
let parse_error msg start finish =
  let open Location in
  raise (Parsing_Error (error ~loc:(Some (pos_loc start finish)) msg))

let constraint_missing bound set =
  let set_string s =
    String.concat ~sep:", "
      (List.map (String.Set.to_list s) ~f:(fun x -> "$" ^ x)) in
  let ending, ending' =
    match String.Set.length set, bound with
    | 1, `Upper -> "an upper bound", "a term variable"
    | 1, `Lower -> "a lower bound", "a term variable"
    | 0, _ -> assert false
    | _, `Upper -> "upper bounds", "term variables"
    | _, `Lower -> "lower bounds", "term variables"
    | _, _ -> assert false in
  sprintf "missing a constraint that specifies %s for %s %s"
    ending ending' (set_string set)
