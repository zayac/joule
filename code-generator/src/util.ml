open Core.Std

let switch_of_alist_exn l =
  let open Core.Std in
  let l = List.map l ~f:(fun (el, t) -> Cnf.from_logic el, t) in
  let multi_map = Cnf.Map.of_alist_multi l in
  let map = Cnf.Map.fold multi_map ~init:Cnf.Map.empty
    ~f:(fun ~key ~data acc ->
      match data with
      | hd :: [] -> Cnf.Map.add acc ~key ~data:hd
      | hd :: tl -> acc
      | [] -> failwith "invalid argument"
    ) in
  Term.Switch map

(* convert a label-term key-value pairs into a correct map structure with
   label conflict resolution. *)
let map_of_alist_exn l =
  let multi_map = String.Map.of_alist_multi l in
  let rec f (gacc: Logic.t list) (vacc: Term.t list) = function
  (* unreachable state due to the explicit check for empty records/choices in
     the parser below *)
  | [] -> failwith "a list must contain at least one element"
  | (g, v) :: [] ->
    if not (List.is_empty gacc) then
      let gacc, vacc = g :: gacc, v :: vacc in
      let logic = Logic.list_of_disjuncts gacc in
      let lst =  List.map2_exn ~f:(fun g t -> g, t) gacc vacc in
      (Cnf.from_logic logic), switch_of_alist_exn lst
    else
      (Cnf.from_logic g), v
  | (g, v) :: tl -> f (g :: gacc) (v :: vacc) tl in
  String.Map.map ~f:(f [] []) multi_map
