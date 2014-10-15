open Core.Std

module T = struct
  type t =
    | Nil
    | OrdinalInt of int
    | NominalInt of int
    | Symbol of string
    | Tuple of t list
    | List of t list * string option
    | Record of (Cnf.t * t) String.Map.t * string option
    | Choice of (Cnf.t * t) String.Map.t * string option
    | Var of string
    | Switch of t Cnf.Map.t
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
    (* remove quotation marks if label consists of a single word *)
    let l =
      if not (String.contains l ' ') && Int.(String.length l > 2) then
        String.strip ~drop:(Char.(=) '"') l
      else l
    in
    if Cnf.is_true g then
      sprintf "%s: %s" l (to_string t)
    else
      sprintf "%s(%s): %s" l (Cnf.to_string g) (to_string t) in
  let print_dict x tail lsep rsep =
    let lst = List.filter ~f:(fun (l, (g, t)) -> not (Cnf.is_false g)) (SM.to_alist x) in
    let element_strs = L.map ~f:dict_el_to_string lst in
    S.concat [lsep; S.concat ~sep:", " element_strs; tail_to_string tail; rsep] in
  match t with
  | List ([], None)
  | Nil -> "nil"
  | NominalInt x -> "~" ^ (string_of_int x)
  | OrdinalInt x -> string_of_int x
  | Symbol x ->
    if not (String.contains x ' ') && Int.(String.length x > 2) then
      String.strip ~drop:(Char.(=) '"') x
    else x
  | Tuple x -> S.concat ["("; S.concat ~sep:" " (L.map ~f:to_string x); ")"]
  | List (x, tail) ->
    S.concat ["["; S.concat ~sep:", " (L.map ~f:to_string x);
      tail_to_string tail; "]"]
  | Record (x, None)
      when String.Map.for_all x ~f:(fun (l, _) -> Cnf.is_false l) -> "nil"
  | Record (x, tail) -> print_dict x tail "{" "}"
  | Choice (x, None) when String.Map.is_empty x -> "none"
  | Choice (x, tail) -> print_dict x  tail "(:" ":)"
  | Var x -> "$" ^ x
  | Switch x ->
    let alist = Cnf.Map.to_alist x in
    let sl = List.map
      ~f:(fun (l, t) -> Printf.sprintf "%s: %s" (Cnf.to_string l)
      (to_string t)) alist in
    Printf.sprintf "<%s>" (String.concat ~sep:", " sl)

let rec get_vars t =
  let module SS = String.Set in
  let module SM = String.Map in
  let module L = List in
  match t with
  | Var v -> String.Set.singleton v
  | Nil | NominalInt _ | OrdinalInt _ | Symbol _ -> SS.empty
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
    Cnf.Map.fold ~init:SS.empty ~f:f x


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
  | OrdinalInt _ | NominalInt _ | Symbol _ | Tuple _ | Choice (_, _) ->
    Some false
  | Switch x ->
    let is_nil_flag = ref true in
    let _ = Cnf.Map.iter
      ~f:(fun ~key ~data ->
        if not (Cnf.is_false key) &&
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
    | OrdinalInt _ | NominalInt _ | Symbol _ | Tuple _ | Choice (_, _) ->
      false
    | Switch x ->
      let _ = Cnf.Map.iter
        ~f:(fun ~key ~data ->
          if not (Cnf.is_false key) && not (is_nil_exn data) then
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
  | Nil | OrdinalInt _ | NominalInt _ | Symbol _ ->
    true
  | List (x, None)
  | Tuple x -> List.for_all ~f:is_ground x
  | Record (x, None)
  | Choice (x, None) ->
    String.Map.for_all ~f:(fun (g, t) -> Cnf.is_ground g && is_ground t) x
  | Choice (_, _)
  | Record (_, _)
  | List (_, _)
  | Var _ -> false
  | Switch x -> Cnf.Map.fold ~init:true
    ~f:(fun ~key ~data acc ->
      if Cnf.is_false key then acc else acc && is_ground data) x

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
    | OrdinalInt i, OrdinalInt i' -> Int.compare i' i
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

let rec to_wff bools term =
  let transform map =
    String.Map.map 
      ~f:(fun (logic, data) ->
        Cnf.(from_logic (evaluate bools logic)), data
      ) map in
  match term with
  | Record (map, v) -> Record (transform map, v)
  | Choice (map, v) -> Choice (transform map, v)
  | List (x, v) -> List (List.map ~f:(to_wff bools) x, v)
  | Tuple x -> Tuple (List.map ~f:(to_wff bools) x)
  | x -> x

let rec join t t' =
  let join_lst l l' =
    let s = ref Cnf.Set.empty in
    List.map2_exn l l'
      ~f:(fun t t' ->
        match join t t' with
        | None -> raise (Incomparable_Terms (t, t'))
        | Some (jt, c) ->
          s := Cnf.Set.union !s c;
          jt
      ), !s in
  match t, t' with
  | Var _, _ -> raise (Non_Ground t)
  | _, Var _ -> raise (Non_Ground t')
  | Choice _, Nil
  | Nil, Choice _ -> Some (Nil, Cnf.Set.empty)
  | t, Nil
  | Nil, t -> Some (t, Cnf.Set.empty)
  | Symbol s, Symbol s' when Poly.(s = s') -> Some (Symbol s, Cnf.Set.empty)
  | OrdinalInt i, OrdinalInt i' ->
    Some (OrdinalInt (Pervasives.min i i'), Cnf.Set.empty)
  | NominalInt i, NominalInt i' when Poly.(i = i') ->
    Some (NominalInt i, Cnf.Set.empty)
  | Tuple l, Tuple l' when Poly.(List.length l = List.length l') ->
    begin
      try
        let lst, s = join_lst l l' in
        Some (Tuple lst, s)
      with Incomparable_Terms (t, t') -> None
    end
  | List (l, None), List (l', None) ->
    begin
      try
        if Poly.(List.length l = List.length l') then
          let lst, s = join_lst l l' in
          Some (List (lst, None), s)
        else if Poly.(List.length l > List.length l') then
          let reduced = List.take l (List.length l') in
          let lst, s = join_lst reduced l' in
          let lst' = lst @ (List.drop l (List.length l')) in
            Some (List (lst', None), s)
        else
          let reduced = List.take l' (List.length l) in
          let lst, s = join_lst reduced l in
          let lst' = lst @ (List.drop l' (List.length l)) in
            Some (List (lst', None), s)
      with Incomparable_Terms (t, t') -> None
    end
  | List (_, Some _), _ -> raise (Non_Ground t')
  | _, List (_, Some _) -> raise (Non_Ground t)
  | Record (map, None), Record (map', None) ->
    begin
      try
        let s = ref Cnf.Set.empty in
        let join_map = String.Map.merge map map' ~f:(fun ~key data ->
          match data with
          | `Left v
          | `Right v -> Some v
          | `Both ((l, t), (l', t')) ->
            begin
              match join t t' with
              | None -> raise (Incomparable_Terms (t, t'))
              | Some (jt, c) ->
                s := Cnf.Set.union !s c;
                let _ = if not (Cnf.equal l l') then
                  s := Cnf.Set.add !s Cnf.(l <=> l') in
                Some (Cnf.(l + l'), jt)
            end) in
        Some (Record (join_map, None), !s)
      with Incomparable_Terms (t, t') -> None
    end
  | Record (_, Some _), _ -> raise (Non_Ground t')
  | _, Record (_, Some _) -> raise (Non_Ground t)
  | Choice (map, None), Choice (map', None) ->
    begin
      try
        let s = ref Cnf.Set.empty in
        let join_map = String.Map.merge map map' ~f:(fun ~key data ->
          match data with
          | `Left v
          | `Right v -> (* None *) Some v
          | `Both ((l, t), (l', t')) ->
            begin
              match join t t' with
              | None -> raise (Incomparable_Terms (t, t'))
              | Some (jt, c) ->
                s := Cnf.Set.union !s c;
                if not (Cnf.equal l l') then
                  s := Cnf.Set.add !s Cnf.(l <=> l');
                Some (Cnf.(l * l'), jt)
            end) in
        Some (Choice (join_map, None), !s)
      with Incomparable_Terms (t, t') -> None
    end
  | Choice (_, Some _), _ -> raise (Non_Ground t')
  | _, Choice (_, Some _) -> raise (Non_Ground t)
  | _, _ -> None

let none = Choice (String.Map.empty, None)

let is_choice = function
  | Choice _ -> true
  | _ -> false
