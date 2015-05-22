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
    | UpVar of string
    | DownVar of string
    | Switch of t Cnf.Map.t
  with sexp, compare
end
include T
include Comparable.Make(T)
exception Non_Ground of t with sexp

exception Incomparable_Terms of t * t with sexp

let hash = Hashtbl.hash

let tail_to_string = function
  | None -> ""
  | Some v -> " | $" ^ v

let rec to_string t =
  let module L = List in
  let module S = String in
  let module SM = String.Map in
  let tail_to_string = function
  | None -> ""
  | Some v -> " | $" ^ v in
  let dict_el_to_string (l, (g, t)) =
    let replace input output =
      Str.global_replace (Str.regexp_string input) output in
    let l = S.slice l 1 (S.length l - 1) in
    let l = replace "\"" "\\\"" l in
    let l = S.concat ["\""; l; "\""] in
    if Cnf.is_true g then
      sprintf "%s: %s" l (to_string t)
    else
      sprintf "%s(%s): %s" l (Cnf.to_string g) (to_string t) in
  let print_dict x tail lsep rsep =
    let lst = L.filter ~f:(fun (l, (g, t)) -> not (Cnf.is_false g)) (SM.to_alist x) in
    let element_strs = L.map ~f:dict_el_to_string lst in
    S.concat [lsep; S.concat ~sep:", " element_strs; tail_to_string tail; rsep] in
  match t with
  | List ([], None)
  | Nil -> "nil"
  | NominalInt x -> "~" ^ (string_of_int x)
  | OrdinalInt x -> string_of_int x
  | Symbol x ->
    let s_without_quotes = S.strip ~drop:(Char.(=) '"') x in
    if S.for_all ~f:(fun x -> Char.is_alpha x) s_without_quotes then
      s_without_quotes
    else
      String.concat ["\""; s_without_quotes; "\""]
  | Tuple x -> S.concat ["("; S.concat ~sep:" " (L.map ~f:to_string x); ")"]
  | List (x, tail) ->
    S.concat ["["; S.concat ~sep:", " (L.map ~f:to_string x);
      tail_to_string tail; "]"]
  | Record (x, None)
      when SM.for_all x ~f:(fun (l, _) -> Cnf.is_false l) -> "nil"
  | Record (x, tail) -> print_dict x tail "{" "}"
  | Choice (x, None) when SM.is_empty x -> "none"
  | Choice (x, None) when SM.for_all x ~f:(fun (g, _) -> Cnf.is_false g) -> "none"
  | Choice (x, tail) -> print_dict x  tail "(:" ":)"
  | UpVar x
  | DownVar x -> "$" ^ x
  | Switch x ->
    let alist = Cnf.Map.to_alist x in
    let sl = L.map
      ~f:(fun (l, t) -> Printf.sprintf "%s: %s" (Cnf.to_string l)
      (to_string t)) alist in
    Printf.sprintf "<%s>" (S.concat ~sep:", " sl)

let rec to_formatted_string ?(id=0) t =
  let module L = List in
  let module S = String in
  let module SM = String.Map in
  let indent depth = String.make (2 * depth) ' ' in
  let dict_el_to_string (l, (g, t)) =
    let replace input output =
      Str.global_replace (Str.regexp_string input) output in
    let l = S.slice l 1 (S.length l - 1) in
    let l = replace "\"" "\\\"" l in
    let l = S.concat ["\""; l; "\""] in
    if Cnf.is_true g then
      sprintf "%s: %s" l (to_string t)
    else
      sprintf "%s(%s): %s" l (Cnf.to_string g) (to_string t) in
  let print_dict x tail lsep rsep =
    let lst = L.filter ~f:(fun (l, (g, t)) -> not (Cnf.is_false g)) (SM.to_alist x) in
    let element_strs = L.map ~f:dict_el_to_string lst in
    S.concat
      [
       "\n";
       indent id;
       lsep;
       "\n";
       indent (id + 1);
       S.concat ~sep:(",\n" ^ (indent (id + 1))) element_strs;
       tail_to_string tail;
       "\n";
       indent id;
       rsep]
  in
  match t with
  (* basic cases *)
  | List ([], None) | Nil | NominalInt _ | OrdinalInt _ | Symbol _ | Tuple _
  | UpVar _
  | DownVar _ -> to_string t
  | Record (x, None)
      when SM.for_all x ~f:(fun (l, _) -> Cnf.is_false l) -> to_string t
  | Choice (x, None) when SM.is_empty x -> to_string t
  (* cases that need formatting *)
  | List (x, tail) ->
    S.concat
      ["\n";
       indent id;
      "[\n";
       indent (id + 1);
       S.concat ~sep:(",\n" ^ (indent (id + 2))) (L.map ~f:(to_formatted_string ~id:(id + 1)) x);
       tail_to_string tail;
       "\n";
       indent id;
       "]"]
  | Record (x, tail) -> print_dict x tail "{" "}"
  | Choice (x, tail) -> print_dict x  tail "(:" ":)"
  | Switch x ->
    let alist = Cnf.Map.to_alist x in
    let sl = L.map
      ~f:(fun (l, t) -> Printf.sprintf "%s: %s" (Cnf.to_string l) (to_formatted_string ~id:(id + 1) t)) alist in
    let to_print = S.concat
      ["\n";
       indent id;
       "<\n";
       indent (id + 1);
       S.concat ~sep:(",\n" ^ (indent (id + 2))) sl;
       "\n";
       indent id;
       ">"] in
    Printf.sprintf "%s" to_print

let rec get_vars t =
  let module SS = String.Set in
  let module SM = String.Map in
  let module L = List in
  match t with
  | UpVar v -> String.Set.singleton v
  | DownVar v -> String.Set.singleton v
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
  | UpVar _
  | DownVar _
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
    | UpVar _
    | DownVar _
    | List (_, Some _)
    | Record (_, Some _) -> raise (Non_Ground t)
    | OrdinalInt _ | NominalInt _ | Symbol _ | Tuple _ | Choice (_, _) ->
      false
    | Switch x ->
      Cnf.Map.fold x ~init:true
        ~f:(fun ~key ~data acc ->
          match acc with
          | false -> false
          | true -> Cnf.is_false key || is_nil_exn data
        )
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

let rec is_semiground = function
  | Nil | OrdinalInt _ | NominalInt _ | Symbol _ ->
    true
  | List (x, None)
  | Tuple x -> List.for_all ~f:is_semiground x
  | Record (x, None)
  | Choice (x, None) ->
    String.Map.for_all ~f:(fun (g, t) -> is_semiground t) x
  | Choice (_, _)
  | Record (_, _)
  | List (_, _)
  | UpVar _
  | DownVar _ -> false
  | Switch x -> Cnf.Map.fold ~init:true
    ~f:(fun ~key ~data acc ->
      if Cnf.is_false key then acc else acc && is_semiground data) x

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
  | UpVar _
  | DownVar _ -> false
  | Switch x -> Cnf.Map.fold ~init:true
    ~f:(fun ~key ~data acc ->
      if Cnf.is_false key then acc else acc && is_ground data) x

let rec equivalent_exn t1 t2 =
  match t1, t2 with
  | t1, t2 when t1 = t2 -> Some Cnf.make_true
  | Symbol s, Symbol s' ->
    begin
      match String.equal s s' with
      | true -> Some Cnf.make_true
      | false -> None
    end
  | OrdinalInt i, OrdinalInt i'
  | NominalInt i, NominalInt i' ->
    begin
      match Int.equal i i' with
      | true -> Some Cnf.make_true
      | false -> None
    end
  | Tuple x, Tuple x'
  | List (x, None), List (x', None) ->
    begin
      match List.zip x x' with
      | Some l ->
        List.fold l ~init:(Some Cnf.make_true)
          ~f:(fun acc (t, t') ->
            match acc with
            | Some old_cnf ->
              begin
                match equivalent_exn t t' with
                | Some cnf -> Some Cnf.(old_cnf * cnf)
                | None -> None
              end
            | None -> None
          )
      | None -> None
    end
  | Choice (map, None), Choice (map', None)
  | Record (map, None), Record (map', None) ->
    begin
      let ret = ref (Some Cnf.make_true) in
      String.Map.iter2 map map'
        ~f:(fun ~key ~data ->
          match data with
          | `Left (g, t)
          | `Right (g, t) ->
            ret := None
          | `Both ((g, t), (g', t')) ->
            match equivalent_exn t t' with
            | Some cnf ->
              begin
                match !ret with
                | Some cnf_old ->
                  ret := Some Cnf.(cnf_old * ((g * g') ==> cnf))
                | None -> ()
              end
            | None ->
              ret := None
        );
      !ret
    end
  | _ ->
    None

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
        Cnf.(from_logic (evaluate bools logic)), (to_wff bools data)
      ) map in
  match term with
  | Record (map, v) -> Record (transform map, v)
  | Choice (map, v) -> Choice (transform map, v)
  | List (x, v) -> List (List.map ~f:(to_wff bools) x, v)
  | Tuple x -> Tuple (List.map ~f:(to_wff bools) x)
  | Switch x ->
    begin
      let ret =
        Cnf.Map.fold x ~init:None
          ~f:(fun ~key ~data result ->
            match result with
            | None when Logic.equal Cnf.(evaluate bools key) Logic.True ->
              Some (to_wff bools data)
            | x -> x
          )
      in
      match ret with
      | Some x -> x
      | None -> term
    end
  | x -> x

let flatten_map_of_switches map =
  Cnf.Map.fold map ~init:Cnf.Map.empty
    ~f:(fun ~key ~data acc ->
      let logic = key in
      match data with
      | Switch s' ->
        Cnf.Map.fold s' ~init:acc
          ~f:(fun ~key ~data acc ->
            if not Cnf.(is_false (key * logic)) then
              Cnf.Map.add acc ~key:Cnf.(key * logic) ~data
            else
              acc
          )
      | _ ->
        Cnf.Map.add acc ~key ~data
    )

let reduce_switch s =
  if Int.(Cnf.Map.length s = 1) then
    let _, data = List.hd_exn (Cnf.Map.to_alist s) in
    data
  else
    match Cnf.Map.find s Cnf.make_true with
    | Some x -> x
    | None -> Switch (flatten_map_of_switches s)


let rec join t t' =
  let join_lst l l' =
    List.map2_exn l l'
      ~f:(fun t t' ->
        match join t t' with
        | None -> raise (Incomparable_Terms (t, t'))
        | Some jt -> jt
      )
  in
  match t, t' with
  | UpVar _, _ -> raise (Non_Ground t)
  | DownVar _, _ -> raise (Non_Ground t)
  | _, UpVar _ -> raise (Non_Ground t')
  | _, DownVar _ -> raise (Non_Ground t')
  | Choice _, Nil
  | Nil, Choice _ -> Some Nil
  | t, Nil
  | Nil, t -> Some t
  | Symbol s, Symbol s' when Poly.(s = s') -> Some (Symbol s)
  | OrdinalInt i, OrdinalInt i' ->
    Some (OrdinalInt (Pervasives.min i i'))
  | NominalInt i, NominalInt i' when Poly.(i = i') ->
    Some (NominalInt i)
  | Tuple l, Tuple l' when Poly.(List.length l = List.length l') ->
    begin
      try
        let lst = join_lst l l' in
        Some (Tuple lst)
      with Incomparable_Terms (t, t') -> None
    end
  | List (l, None), List (l', None) ->
    begin
      try
        if Poly.(List.length l = List.length l') then
          let lst = join_lst l l' in
          Some (List (lst, None))
        else if Poly.(List.length l > List.length l') then
          let reduced = List.take l (List.length l') in
          let lst = join_lst reduced l' in
          let lst' = lst @ (List.drop l (List.length l')) in
            Some (List (lst', None))
        else
          let reduced = List.take l' (List.length l) in
          let lst = join_lst reduced l in
          let lst' = lst @ (List.drop l' (List.length l)) in
            Some (List (lst', None))
      with Incomparable_Terms (t, t') -> None
    end
  | List (_, Some _), _ -> raise (Non_Ground t')
  | _, List (_, Some _) -> raise (Non_Ground t)
  | Record (map, None), Record (map', None) ->
    begin
      try
        let join_map = String.Map.merge map map' ~f:(fun ~key data ->
          match data with
          | `Left v
          | `Right v -> (* None *) Some v
          | `Both ((l, t), (l', t')) ->
            begin
              let map = ref Cnf.Map.empty in
              if not Cnf.(is_false (l * ~-l')) then
                map := Cnf.Map.add !map ~key:Cnf.(l * ~-l') ~data:t;
              if not Cnf.(is_false (~-l * l')) then
                map := Cnf.Map.add !map ~key:Cnf.(~-l * l') ~data:t';
              match join t t' with
              | None ->
                Some (Cnf.((l + l') * ~-(l * l')), reduce_switch !map)
              | Some jt ->
                begin
                  if not Cnf.(is_false (l * l')) then
                    map := Cnf.Map.add !map ~key:Cnf.(l * l') ~data:jt;
                  Some (Cnf.(l + l'), reduce_switch !map)
                end
            end)
        in
        Some (Record (join_map, None))
      with Incomparable_Terms (t, t') -> None
    end
  | Record (_, Some _), _ -> raise (Non_Ground t')
  | _, Record (_, Some _) -> raise (Non_Ground t)
  | Choice (map, None), Choice (map', None) ->
    begin
      try
        let join_map = String.Map.merge map map' ~f:(fun ~key data ->
          match data with
          | `Left v
          | `Right v -> (* None *) Some v
          | `Both ((l, t), (l', t')) ->
            begin
              let map = ref Cnf.Map.empty in
              if not Cnf.(is_false (l * ~-l')) then
                map := Cnf.Map.add !map ~key:Cnf.(l * ~-l') ~data:t;
              if not Cnf.(is_false (~-l * l')) then
                map := Cnf.Map.add !map ~key:Cnf.(~-l * l') ~data:t';
              match join t t' with
              | None ->
                Some (Cnf.((l + l') * ~-(l * l')), reduce_switch !map)
              | Some jt ->
                begin
                  if not Cnf.(is_false (l * l')) then
                    map := Cnf.Map.add !map ~key:Cnf.(l * l') ~data:jt;
                  Some (Cnf.(l + l'), reduce_switch !map)
                end
            end)
        in
        Some (Choice (join_map, None))
      with Incomparable_Terms (t, t') -> None
    end
  | Choice (_, Some _), _ -> raise (Non_Ground t')
  | _, Choice (_, Some _) -> raise (Non_Ground t)
  | Switch s, Switch s' ->
    begin
      let map = ref Cnf.Map.empty in
      Cnf.Map.iter s
        ~f:(fun ~key ~data ->
          let l, t = key, data in
          Cnf.Map.iter s'
            ~f:(fun ~key ~data ->
              let l', t' = key, data in
              if not Cnf.(is_false (l * ~-l')) then
                map := Cnf.Map.add !map ~key:Cnf.(l * ~-l') ~data:t;
              if not Cnf.(is_false (~-l * l')) then
                map := Cnf.Map.add !map ~key:Cnf.(~-l * l') ~data:t';
              match join t t' with
              | None -> ()
              | Some jt ->
                if not Cnf.(is_false (l * l')) then
                  map := Cnf.Map.add !map ~key:Cnf.(l * l') ~data:jt
            )
        );
      Some (reduce_switch !map)
    end
  | Switch s, t
  | t, Switch s ->
    begin
      let map = ref Cnf.Map.empty in
      Cnf.Map.iter s
        ~f:(fun ~key ~data ->
          let l', t' = key, data in
          match join t t' with
          | None -> ()
          | Some jt ->
            if not Cnf.(is_false l') then
              map := Cnf.Map.add !map ~key:l' ~data:jt
        );
      Some (reduce_switch !map)
    end
  | _, _ -> None

let cnf_map_of_alist lst =
  let multi_result = Cnf.Map.of_alist_multi lst in
  let cnf_constrs = ref Cnf.make_true in
  let reduced =
    Cnf.Map.fold ~init:Cnf.Map.empty
      ~f:(fun ~key ~data acc ->
        match data with
        | [] -> failwith "unreachable"
        | hd :: [] -> Cnf.Map.add acc ~key ~data:hd
        | hd :: tl ->
          let jn =
            List.fold ~init:(Some hd)
              ~f:(fun acc t ->
                match acc with
                | None -> None
                | Some acc ->
                  try
                    match join acc t with
                    | None -> None
                    | Some term ->
                      cnf_constrs := Cnf.(!cnf_constrs * key);
                      Some term
                  with _ -> None
              ) tl
          in
          match jn with
          | None ->
            cnf_constrs := Cnf.(!cnf_constrs * ~-key);
            acc
          | Some value ->
            Cnf.Map.add acc ~key ~data:value
      ) multi_result in
  reduced, !cnf_constrs



let none = Choice (String.Map.empty, None)

let is_choice = function
  | Choice _ -> true
  | _ -> false

let get_map_exn = function
  | Choice (m, _) -> m
  | _ -> assert false

let is_up_var s = String.is_prefix s ~prefix:"^"
