open Core.Std

module T = struct
  type t =
    | Nil
    | Int of int
    | Symbol of string
    | Tuple of t list
    | List of t list * string option
    | Record of (Logic.t * t) String.Map.t * string option
    | Choice of (Logic.t * t) String.Map.t * string option
    | Var of string
    | Switch of t Logic.Map.t
  with sexp, compare
end
include T
include Comparable.Make(T)
exception Non_Ground of t with sexp

exception Incomparable_Terms of t * t with sexp

let hash = Hashtbl.hash

let rec to_string t =
  let module L = List in
  let module S = String in
  let module SM = String.Map in
  let tail_to_string = function
  | None -> ""
  | Some v -> " | $" ^ v in
  let dict_el_to_string (l, (g, t)) =
    let guard = if Logic.(g = True) then "" else
      Printf.sprintf "(%s)" (Logic.to_string g) in
    if Logic.(g <> False) then
      Printf.sprintf "%s%s: %s" l guard (to_string t)
    else "" in
  let print_dict x tail lsep rsep =
    let element_strs = L.map ~f:dict_el_to_string (SM.to_alist x) in
    S.concat [lsep;
      S.concat ~sep:", " element_strs; tail_to_string tail; rsep] in
  match t with
  | Nil -> "nil"
  | Int x -> string_of_int x
  | Symbol x -> x
  | Tuple x -> S.concat ["("; S.concat ~sep:" " (L.map ~f:to_string x); ")"]
  | List (x, tail) ->
    S.concat ["["; S.concat ~sep:", " (L.map ~f:to_string x);
      tail_to_string tail; "]"]
  | Record (x, tail) -> print_dict x tail "{" "}"
  | Choice (x, tail) -> print_dict x tail "(:" ":)"
  | Var x -> "$" ^ x
  | Switch x ->
    let alist = Logic.Map.to_alist x in
    let sl = List.map
      ~f:(fun (l, t) -> Printf.sprintf "%s : %s" (Logic.to_string l)
      (to_string t)) alist in
    Printf.sprintf "<%s>" (String.concat ~sep:", " sl)

let rec get_vars t =
  let module SS = String.Set in
  let module SM = String.Map in
  let module L = List in
  match t with
  | Var v -> String.Set.singleton v
  | Nil | Int _ | Symbol _ -> SS.empty
  | Tuple x -> L.fold_left ~init:SS.empty ~f:SS.union (L.map ~f:get_vars x)
  | List (x, tail) ->
    let tl = Option.value_map ~default:SS.empty ~f:SS.singleton tail in
    SS.union tl (L.fold_left ~init:SS.empty ~f:SS.union (L.map ~f:get_vars x))
  | Record (map, v)
  | Choice (map, v) ->
    let f ~key:_ ~data:(_, t) acc = SS.union acc (get_vars t) in
    let tl = Option.value_map ~default:SS.empty ~f:SS.singleton v in
    SS.union tl (SM.fold ~init:SS.empty ~f:f map)
  | Switch x ->
    let f ~key:_ ~data acc = SS.union acc (get_vars data) in
    Logic.Map.fold ~init:SS.empty ~f:f x


let rec is_nil = function
  | Nil -> Some true
  | List (x, None) ->
    let nil_list = List.map ~f:is_nil x in
    if List.exists ~f:is_none nil_list then None
    else if List.exists ~f:(Poly.(=) (Some false)) nil_list then
      Some false
    else Some true
  | Record (x, None) as t ->
    if String.Map.is_empty x then Some true
    else if String.Set.is_empty (get_vars t) then Some false
    else None
  | Var _
  | List (_, Some _)
  | Record (_, Some _) -> None
  | Int _ | Symbol _ | Tuple _ | Choice (_, _) -> Some false
  | Switch x ->
    let is_nil_flag = ref true in
    let _ = Logic.Map.iter
      ~f:(fun ~key ~data ->
        if not Logic.(key = False) &&
          not (Option.value ~default:false (is_nil data)) then
          is_nil_flag := false) x in
    if !is_nil_flag then Some true else None

let rec is_nil_exn t =
  try
    match t with
    | Nil -> true
    | List (x, None) -> List.for_all ~f:is_nil_exn x
    | Record (x, None) as t ->
      if String.Map.is_empty x then true
      else if String.Set.is_empty (get_vars t) then false
      else raise (Non_Ground t)
    | Var _
    | List (_, Some _)
    | Record (_, Some _) -> raise (Non_Ground t)
    | Int _ | Symbol _ | Tuple _ | Choice (_, _)  -> false
    | Switch x ->
      let _ = Logic.Map.iter
        ~f:(fun ~key ~data ->
          if not Logic.(key = False) && not (is_nil_exn data) then
            raise (Non_Ground t)) x in
      true
  with Non_Ground _ -> raise (Non_Ground t)

let canonize t =
  let rec trim_list_rev = function
  | [] -> []
  | hd :: tl as el ->
    match is_nil hd with
    | Some true -> trim_list_rev tl
    | Some false | None -> el in
  match t with
  | List (x, None) -> List (List.rev (trim_list_rev (List.rev x)), None)
  | x -> x

let rec is_ground = function
  | Nil | Int _ | Symbol _ -> true
  | List (x, None)
  | Tuple x -> List.for_all ~f:is_ground x
  | Record (x, None)
  | Choice (x, None) ->
    String.Map.for_all ~f:(fun (g, t) -> Logic.is_ground g && is_ground t) x
  | Choice (_, _)
  | Record (_, _)
  | List (_, _)
  | Var _ -> false
  | Switch x -> Logic.Map.fold ~init:true
    ~f:(fun ~key ~data acc ->
      if Logic.(key = False) then acc else acc && is_ground data) x

let rec seniority_exn t1 t2 =
  try
    let seniority_lists_exn l l' =
    let lhead = List.take l (List.length l') in
    let comp_res = List.map2_exn ~f:seniority_exn lhead l' in
    if List.exists ~f:(Int.(=) 1) comp_res then
        raise (Incomparable_Terms (t1, t2))
    else if Int.equal (List.length l) (List.length l')
        && List.for_all ~f:(Int.(=) 0) comp_res then 0
    else 1 in
    match t1, t2 with
    | t1, t2 when t1 = t2 -> 0
    | Nil, x when is_nil_exn x -> 0
    | x, Nil when is_nil_exn x -> 0
    | _, Nil -> 1
    | Nil, _ -> -1
    | Tuple x, Tuple x' ->
      if not (Int.equal (List.length x) (List.length x')) then
        raise (Incomparable_Terms (t1, t2))
      else
        let comp_res = List.map2_exn ~f:seniority_exn x x' in
        let less = List.exists ~f:(Int.(=) (-1)) comp_res in
        let more = List.exists ~f:(Int.(=) 1) comp_res in
        if less && more then raise (Incomparable_Terms (t1, t2))
        else if less then -1
        else if more then 1
        else 0
    | List (x, None), List (x', None) ->
      if Int.(>) (List.length x) (List.length x') then seniority_lists_exn x x'
        else -1 * (seniority_lists_exn x' x)
    | Record (x, None), Record (x', None) -> seniority_maps_exn `Record x x'
    | Choice (x, None), Choice (x', None) -> seniority_maps_exn `Choice x x'
    | _ -> raise (Incomparable_Terms (t1, t2))
  with Incomparable_Terms _
     | Non_Ground _ ->
     raise (Incomparable_Terms (t1, t2))

(* [seniority_maps exn x x'] resolves the seniority relation for two 'map'
    terms, where [x] and [x'] has types [(logic * term) String.Map] each.
    Basically, this is a seniority relation resolution for record and choice
    terms. *)
and seniority_maps_exn typ x x' =
  let module SM = String.Map in
  let error_s = "wrong seniority relation" in
  let comp_res = ref [] in
  let validate map ~key ~data:(guard, term) =
    match SM.find map key with
    | None ->
      if Poly.(typ = `Record) then
        raise (Incomparable_Terms (Record (x, None), Record (x', None)))
      else
        raise (Incomparable_Terms (Choice (x, None), Choice (x', None)))
    (* ignore guards *)
    | Some (_, term') -> seniority_exn term' term in
  if Int.((SM.length x) > (SM.length x')) then
    SM.iter ~f:(fun ~key ~data ->
      comp_res := (validate x ~key ~data) :: !comp_res
    ) x'
  else
    SM.iter ~f:(fun ~key ~data ->
      comp_res := (validate x' ~key ~data) :: !comp_res
    ) x;
  let less = List.exists ~f:(Int.(=) (-1)) !comp_res in
  let more = List.exists ~f:(Int.(=) 1) !comp_res in
  let multiplier = if Poly.(typ = `Record) then 1 else -1 in
  if less && more then invalid_arg error_s
  else if less then multiplier * -1
  else if more then multiplier * 1
  else 0

let logic_seniority_exn lm lm' =
  let sat_disjunctive_terms = ref Logic.Set.empty in
  let _ = Map.iter lm ~f:(fun ~key ~data ->
    let term, logic = key, data in
    Map.iter lm' ~f:(fun ~key ~data ->
      let term', logic' = key, data in
      try
        if Int.(seniority_exn term term' > -1) then
          sat_disjunctive_terms :=
            Logic.Set.add !sat_disjunctive_terms Logic.(logic * logic')
      with Incomparable_Terms _ ->
        ())) in
    !sat_disjunctive_terms

let canonize_switch lmap =
  (* remove all false expressions *)
  let lmap = Logic.Map.fold ~init:Logic.Map.empty
    ~f:(fun ~key ~data acc ->
      match Logic.simplify key with
      | Logic.False -> acc
      | _ -> Logic.Map.add ~key ~data acc) lmap in
  if Logic.Map.mem lmap Logic.True then
    let bool_constrs = Logic.Map.fold ~init:Logic.Set.empty
      ~f:(fun ~key ~data acc ->
        if Logic.(key = Logic.True) then acc
        else Logic.Set.add acc Logic.(~-key)) lmap in
    lmap, bool_constrs
  else lmap, Logic.Set.empty

let logic_map_to_term_map lm =
  Logic.Map.fold lm ~init:Map.empty ~f:(fun ~key ~data acc ->
    Map.change acc data (function
    | None -> Some key
    | Some value -> Some (Logic.And (value, key))))

(*let unify ctx model term =*)
  (*let transform map =*)
    (*String.Map.map map ~f:(fun (logic, data) ->*)
      (*(if Z3Solver.evaluate ctx model logic then Logic.True else Logic.False),*)
      (*data) in*)
  (*match term with*)
  (*| Record (map, v) -> Record (transform map, v)*)
  (*| Choice (map, v) -> Choice (transform map, v)*)
  (*| x -> x*)
