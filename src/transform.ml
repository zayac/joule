open Core.Std

module Log = Log.Make(struct let section = "transform:" end)

let initial_bool_variables = ref String.Set.empty
let additional_bool_variables = ref String.Set.empty
let bool_var_candidate = ref "a"

let initial_term_variables = ref String.Set.empty
let additional_term_variables = ref String.Set.empty
let term_var_candidate = ref "a"

let union_variables = ref String.Map.empty

let union_cache = ref GuardedVar.Map.empty

let next_var_name s =
  let s' = ref "" in
  let flag = ref true in
  for i = String.length s - 1 downto 0 do
    if !flag then
      if Char.to_int s.[i] >= Char.to_int 'z' then
      begin
        s' := "a" ^ !s';
        if i = 0 then s' := "a" ^ !s'
      end
      else
        let _ = s' := String.make 1 (Char.of_int_exn ((Char.to_int s.[i]) + 1)) ^ !s' in
        flag := false
    else
      s' := String.make 1 s.[i] ^ !s'
  done;
  !s'

let get_free_bool_var () =
  while String.Set.mem !initial_bool_variables !bool_var_candidate ||
        String.Set.mem !additional_bool_variables !bool_var_candidate do
    bool_var_candidate := next_var_name !bool_var_candidate
  done;
  let result = !bool_var_candidate in
  let _ = bool_var_candidate := next_var_name !bool_var_candidate in
  additional_bool_variables := String.Set.add !additional_bool_variables result;
  result

let get_free_term_var () =
  while String.Set.mem !initial_term_variables !term_var_candidate ||
        String.Set.mem !additional_term_variables !term_var_candidate do
    term_var_candidate := next_var_name !term_var_candidate
  done;
  let result = !term_var_candidate in
  let _ = term_var_candidate := next_var_name !term_var_candidate in
  additional_term_variables := String.Set.add !additional_term_variables result;
  result

let union_terms_to_vars union_var t t' =
  let constrs = ref [] in
  let open Term in
  let transform t =
    match t with
    | Var v -> v
    | t ->
      let v = get_free_term_var () in
      let _ = constrs := !constrs @ [Var v, t; t, Var v] in
      v in
  let t =
    match t with
    | Var _ -> t
    | _ -> (Var (transform t)) in
  let t' =
    match t' with
    | Var _ -> t'
    | _ -> (Var (transform t')) in
  match t, t' with
  | Var v, Var v' ->
    union_variables :=
      String.Map.add !union_variables ~key:union_var ~data:(v, v');
    t, t', !constrs
  | _ -> assert false (* unreachable state *)
  
let to_union logic term =
  let constrs = ref [] in
  let rec transform term =
    let open Term in
    match term with
    | Tuple [Symbol "\"union\""; t; t'] ->
      Log.logf "transforming a term %s" (to_string term);
      let t = transform t in
      let t' = transform t' in
      let new_bool_var = get_free_bool_var () in
      let new_term_var = get_free_term_var () in
      (* make sure that union term subterms are only variables *)
      let t, t', new_constrs = union_terms_to_vars new_term_var t t' in
      let _ = constrs := !constrs @ new_constrs in
      let return_switch = Util.switch_of_alist_exn
        [ Logic.(Var new_bool_var), Term.(Var new_term_var);
          Logic.(~-(Var new_bool_var)), Tuple [Symbol "union"; t; t']
        ] logic in
      let switch1 = Util.switch_of_alist_exn
        [ Logic.(Var new_bool_var), Term.(Var new_term_var);
          Logic.(~-(Var new_bool_var)), t
        ] logic in
      let switch2 = Util.switch_of_alist_exn
        [ Logic.(Var new_bool_var), Term.(Var new_term_var);
          Logic.(~-(Var new_bool_var)), t' ] logic in
      constrs := !constrs @ [switch1, t; switch2, t'; Term.Var new_term_var, Term.Nil];
      return_switch
    | Tuple l ->
      Tuple (List.map l ~f:transform)
    | List (l, x) ->
      List (List.map l ~f:transform, x)
    | Record (map, x) ->
      Record (String.Map.map map ~f:(fun (b, t) -> b, transform t), x)
    | Choice (map, x) ->
      Choice (String.Map.map map ~f:(fun (b, t) -> b, transform t), x)
    | Switch map ->
      Switch (Cnf.Map.map map ~f:transform)
    | t -> t
  in
  transform term, !constrs

let union constrs logic =
  let additional_constrs = ref [] in
  let constrs, logic = List.fold constrs ~init:([], logic)
    ~f:(fun (constrs, logic) (left, right) ->
      let l = ref logic in
      let left', constrs' = to_union l left in
      let right', constrs'' = to_union l right in
      additional_constrs := !additional_constrs @ constrs' @ constrs'';
      constrs @ [left', right'], !l
    ) in
    constrs @ !additional_constrs, logic
