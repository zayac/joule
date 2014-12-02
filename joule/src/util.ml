open Core.Std

module LLog = Log.Make(struct let section = "logic:" end)

let log_bool set =
  if not (Cnf.CSet.is_empty set) then
    LLog.logf "adding boolean constraints {%s}" (Cnf.to_string set)

let switch_of_alist_exn l bool_constrs =
  let open Core.Std in
  let l = List.map l ~f:(fun (el, t) -> Cnf.from_logic el, t) in
  let multi_map = Cnf.Map.of_alist_multi l in
  let map = Cnf.Map.fold multi_map ~init:Cnf.Map.empty
    ~f:(fun ~key ~data acc ->
      match data with
      | hd :: [] -> Cnf.Map.add acc ~key ~data:hd
      | hd :: tl ->
        let _ = bool_constrs := Cnf.(CSet.union !bool_constrs ~-key) in
        acc
      | [] -> failwith "invalid argument"
    ) in
  let keys = Cnf.Map.keys map in
  let _ = if List.length keys > 1 then
    let l = Cnf.pairwise_not_and keys in
    let _ = log_bool l in
    bool_constrs := Cnf.CSet.union !bool_constrs l in
  (* add constraints asserting that at least one logical expression must be
     satisfiable *)
  let singleton = Cnf.list_of_disjuncts keys in
  let _ = log_bool singleton in
  let _ = bool_constrs := Cnf.CSet.union !bool_constrs singleton in
  Term.Switch map

(* convert a label-term key-value pairs into a correct map structure with
   label conflict resolution. New boolean constraints are added to
   [bool_constrs]. *)
let map_of_alist_exn l bool_constrs =
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
      (Cnf.from_logic logic), switch_of_alist_exn lst bool_constrs
    else
      (Cnf.from_logic g), v
  | (g, v) :: tl -> f (g :: gacc) (v :: vacc) tl in
  String.Map.map ~f:(f [] []) multi_map
