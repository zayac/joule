open Core.Std

module SLog = Log.Make(struct let section = "solver:" end)
module LLog = Log.Make(struct let section = "logic:" end)

exception No_Solution of string

let iteration_limit = ref 100

let verbose_output = ref false

let boolean_constraints = ref Cnf.CSet.empty

(* stores term variables that are choices *)
let choice_vars = ref String.Map.empty

let may_be_choice s logic =
  match String.Map.find !choice_vars s with
  | None -> false
  | Some v ->
    if Cnf.(v = logic) then true
    else if Option.is_some (Sat.solve Cnf.(v * logic)) then true
    else false

let log_bool_constr depth b =
  let indent = String.make depth ' ' in
  LLog.logf "%sadding a boolean constraint '%s'" indent (Cnf.to_string b)

let add_bool_constr depth (b: Cnf.t) =
  if not (Cnf.is_true b) && not (Cnf.CSet.subset b !boolean_constraints) then
    let _ = log_bool_constr depth b in
    boolean_constraints := Cnf.CSet.union !boolean_constraints b

let print_map tm =
  let tl = Cnf.Map.to_alist tm in
  let sl = List.map tl ~f:(fun (l, t) ->
    Printf.sprintf "%s: %s" (Cnf.to_string l) (Term.to_string t)) in
  String.concat ~sep:", " sl

let get_bound constrs var =
  match String.Map.find constrs var with
  | None -> Cnf.Map.singleton Cnf.make_true Term.Nil
  | Some upper when Cnf.Map.is_empty upper ->
    Cnf.Map.singleton Cnf.make_true Term.Nil
  | Some upper -> upper

let add_to_map depth map logic term =
  if Option.is_some (Sat.solve Cnf.(~-(logic <=> Cnf.make_false))) then
    let added = ref false in
    let map = Cnf.Map.fold map ~init:Cnf.Map.empty
                ~f:(fun ~key ~data acc ->
                  if Option.is_some (Sat.solve Cnf.(~-(key <=> logic))) then (* FIXME bottleneck *)
                    Cnf.Map.add map ~key ~data
                  else
                    begin
                      let _ = added := true in
                      match Term.join data term with
                      | None ->
                        let _ = add_bool_constr depth Cnf.(~-logic) in
                        acc
                      | Some (glb, c) ->
                        (*Cnf.Set.iter c*)
                          (*~f:(fun el ->*)
                            (*add_bool_constr depth Cnf.((logic * key) ==> el)*)
                           (* );*)
                        Cnf.Map.add map ~key ~data:glb
                    end
                ) in
    if not !added then (Cnf.Map.add map ~key:logic ~data:term)
    else map
  else map

let merge_bounds depth old_terms new_terms =
  if Cnf.Map.is_empty old_terms && Cnf.Map.is_empty new_terms then
    Cnf.Map.singleton Cnf.make_true Term.Nil
  else if Cnf.Map.is_empty old_terms then new_terms
  else if Cnf.Map.is_empty new_terms then old_terms
  else if Cnf.Map.equal Term.equal old_terms new_terms then old_terms
  else
    begin
      let new_map = ref Cnf.Map.empty in
      Cnf.Map.iter old_terms
        ~f:(fun ~key ~data ->
          let logic', term' = key, data in
          Cnf.Map.iter new_terms
            ~f:(fun ~key ~data ->
              let logic, term = key, data in
              begin
                (* Notice that two terms that correspond to the same Boolean
                   guards may be added to the map *)
                new_map := add_to_map depth !new_map Cnf.(logic * ~-logic') term;
                new_map := add_to_map depth !new_map Cnf.(~-logic * logic') term';
                match Term.join term term' with
                | None ->
                  begin
                    add_bool_constr depth Cnf.(~-(logic * logic'))
                  end
                | Some (join_term, log) ->
                  (*Cnf.Set.iter log ~f:(fun el -> add_bool_constr depth Cnf.((logic * logic') ==> el));*)
                  new_map := add_to_map depth !new_map Cnf.(logic * logic') join_term
              end
            );
        );
    let all_terms = ref Term.Set.empty in
    Cnf.Map.fold !new_map ~init:Cnf.Map.empty
      ~f:(fun ~key ~data acc ->
        let data' = data in
        if Term.Set.mem !all_terms data then acc
        else
          let combined =
            Cnf.Map.fold !new_map ~init:key
              ~f:(fun ~key ~data acc ->
                if Term.equal data data' then Cnf.(acc + key)
                else acc
              ) in
          all_terms := Term.Set.add !all_terms data;
          let result = Cnf.Map.change acc combined (function
            | None -> Some data
            | Some old ->
              match Term.join data old with
              | None -> add_bool_constr depth Cnf.(~-combined); None
              | Some (join_term, l) ->
                (*Cnf.Set.iter l ~f:(fun el -> add_bool_constr depth Cnf.(key ==> el));*)
                Some join_term
          ) in
          result
      )
  end

let combine_bounds old_terms new_terms =
  Cnf.Map.fold new_terms ~init:old_terms
    ~f:(fun ~key ~data acc ->
      let new_term = data in
      Cnf.Map.change acc key (function
        | None -> Some data
        | Some old_term ->
          try
            let relation = Term.seniority_exn old_term new_term in
            if relation < 1 then Some new_term else Some old_term
          with Term.Incomparable_Terms (t1, t2) ->
            Errors.unsat_error (Printf.sprintf "constraint violation between old bound
           (%s) and new one (%s)" (Term.to_string old_term)
                                  (Term.to_string new_term)))
    )

let rec bound_combinations_list = function
  | [] -> []
  | hd :: [] ->
    Cnf.Map.fold hd ~init:[]
      ~f:(fun ~key ~data acc -> (key, [data]) :: acc)
  | hd :: tl ->
    let tail_bounds = bound_combinations_list tl in
    Cnf.Map.fold hd ~init:[]
      ~f:(fun ~key ~data acc ->
        let logic, term = key, data in
        List.fold tail_bounds ~init:acc
          ~f:(fun acc (logic', lst) ->
            (Cnf.(logic * logic'), term :: lst) :: acc)
      )

let rec bound_combinations_alist = function
  | [] -> []
  | (s, (l, map)) :: [] ->
    Cnf.Map.fold map ~init:[]
      ~f:(fun ~key ~data acc -> (key, [s, (l, data)]) :: acc)
  | (s, (l, map)) :: tl ->
    let tail_bounds = bound_combinations_alist tl in
    Cnf.Map.fold map ~init:[]
      ~f:(fun ~key ~data acc ->
        let logic, term = key, data in
        List.fold tail_bounds ~init:acc
          ~f:(fun acc (logic', lst) ->
            (Cnf.(logic * logic'), (s, (l, term)) :: lst) :: acc
          )
      )

let rec merge_maps depth left right logic =
  let result = ref [logic, String.Map.empty] in
  String.Map.iter2 left right
    ~f:(fun ~key ~data ->
      let add l t =
        List.map !result
          ~f:(fun (logic, map) ->
            logic, String.Map.add map ~key ~data:(l, t)
          ) in
      match data with
      | `Left (l, t)
      | `Right (l, t) ->
        result := add l t
      | `Both ((l, t), (l', t')) ->
        (* add two alternatives *)
        result := add Cnf.(l' + l) t;
        add_bool_constr depth Cnf.(~-(l * l'))
    );
  !result

let map_of_alist_safe l =
  let multi = Cnf.Map.of_alist_multi l in
  Cnf.Map.fold multi ~init:Cnf.Map.empty
    ~f:(fun ~key ~data acc ->
      match data with
      | hd :: [] -> Cnf.Map.add acc ~key ~data:hd
      | lst ->
        let result = List.fold lst ~init:(Some Term.Nil)
          ~f:(fun acc el ->
            match acc with
            | None -> None
            | Some acc ->
              begin
                match Term.join acc el with
                | None -> None
                | Some (jt, c) -> Some jt
              end) in
        match result with
        | None -> acc
        | Some data -> Cnf.Map.add acc ~key ~data)


let rec bound_terms_exn depth constrs logic term =
  let open Term in
  let module CM = Cnf.Map in
  match term with
  | Nil | NominalInt _ | OrdinalInt _ | Symbol _ -> CM.singleton logic term
  | Switch x ->
    CM.fold x ~init:CM.empty
      ~f:(fun ~key ~data acc ->
        let logic' = key in
        let bounds = bound_terms_exn depth constrs logic data in
        let bounds = CM.fold bounds ~init:acc
                       ~f:(fun ~key ~data acc ->
                         add_to_map depth acc Cnf.(logic * key * logic') data
                       ) in
        bounds
      )
  | Tuple x ->
    let l = List.map x ~f:(fun x -> bound_terms_exn depth constrs logic x) in
    let term_list =
      List.map (bound_combinations_list l) ~f:(fun (l, t) -> l, Tuple t) in
    (* TODO Problem here *)
    (*List.iter term_list*)
      (*~f:(fun (c, t) ->*)
        (*printf "%s -> %s\n" (Cnf.to_string c) (Term.to_string t)*)
      (* );*)
    (*CM.of_alist_exn term_list*)
    map_of_alist_safe term_list
  | List (x, s) ->
    let l = List.map x ~f:(fun x -> bound_terms_exn depth constrs logic x) in
    let term_list = bound_combinations_list l in
    begin
      match s with
      | None ->
        (*CM.of_alist_exn*)
          (*(List.map term_list ~f:(fun (l, t) -> l, List (t, None)))*)
        map_of_alist_safe (List.map term_list ~f:(fun (l, t) -> l, List (t, None)))
      | Some var ->
        let bounds = bound_terms_exn depth constrs logic (Var var) in
        let lst = List.fold term_list ~init:[]
                    ~f:(fun acc (logic', head) ->
                      CM.fold bounds ~init:acc
                        ~f:(fun ~key ~data acc ->
                          match data with
                          | Term.List (l, None) ->
                            (Cnf.(logic * key * logic'), head @ l) :: acc
                          | Term.Nil ->
                            (Cnf.(logic * key * logic'), head) :: acc
                          | Term.List (l, _) ->
                            (* unreachable state: all terms must be ground *)
                            failwith
                              (Printf.sprintf "expected a ground list term, but %s found"
                                 (Term.to_string data))
                          | _ -> acc
                        )
                    ) in  (* TODO fix here *)
        List.fold lst ~init:CM.empty
          ~f:(fun acc (l, t) ->
            add_to_map depth acc l (List(t, None))
          )
    end
  | Record (map, s) ->
    let b = String.Map.map map
              ~f:(fun (l, t) -> l, bound_terms_exn depth constrs logic t) in
    let term_map = bound_combinations_alist (String.Map.to_alist b) in
    begin
      match s with
      | None ->
        let combined = List.map term_map
                         ~f:(fun (logic, lst) ->
                           logic, Record (String.Map.of_alist_exn lst, None)) in
        (*CM.of_alist_exn combined*)
        map_of_alist_safe combined
      | Some var ->
        let bounds = bound_terms_exn depth constrs logic (Var var) in
        let data = List.map term_map
                     ~f:(fun (logic, lst) ->
                       logic, String.Map.of_alist_exn lst
                     ) in
        let combined = List.fold data ~init:[]
                         ~f:(fun acc (logic', head) ->
                           CM.fold bounds ~init:acc
                             ~f:(fun ~key ~data acc ->
                               match data with
                               | Term.Record (map, None) ->
                                 let lst = merge_maps depth head map Cnf.(key * logic') in
                                 lst @ acc
                               | Term.Nil ->
                                 let lst = merge_maps depth head String.Map.empty
                                             Cnf.(key * logic') in
                                 lst @ acc
                               | Term.Record (map, _) ->
                                 (* unreachable state: all terms must be ground *)
                                 failwith
                                   (Printf.sprintf "expected a ground list term, but %s found"
                                      (Term.to_string data))
                               | _ -> acc
                             )
                         ) in
        (* if tail variable does not have bounding term that is a record,
           throw error *)
        let _ = if List.is_empty combined then
            Errors.unsat_error 
              (Printf.sprintf "Missing record as a upper bound for variable $%s"
                 var) in
        (* verify the consistency of generated bounds *)
        let l = List.map combined
                  ~f:(fun (logic, lst) -> logic, Record (lst, None)) in
        (*CM.of_alist_exn l*)
        map_of_alist_safe l
    end
  (* TODO fix copy/paste *)
  | Choice (map, s) ->
    let b = String.Map.map map
              ~f:(fun (l, t) -> l, bound_terms_exn depth constrs logic t) in
    let term_map = bound_combinations_alist (String.Map.to_alist b) in
    (* list of values for the left term *)
    begin
      match s with
      | None ->
        let combined = List.map term_map
                         ~f:(fun (logic, lst) ->
                           logic, Choice (String.Map.of_alist_exn lst, None)
                         ) in
        (*CM.of_alist_exn combined*)
        map_of_alist_safe combined
      | Some var ->
        (* list of values for the left term *)
        let data = List.map term_map
                     ~f:(fun (logic, lst) ->
                       logic, String.Map.of_alist_exn lst
                     ) in
        (* list of values for the right term *)
        let bounds = bound_terms_exn depth constrs logic (Var var) in
        let combined = List.fold data ~init:[]
                         ~f:(fun acc (logic', head) ->
                           CM.fold bounds ~init:acc
                             ~f:(fun ~key ~data acc ->
                               match data with
                               | Term.Choice (map, None) ->
                                 let lst = merge_maps depth head map Cnf.(key * logic') in
                                 lst @ acc
                               | Term.Nil ->
                                 let lst = merge_maps depth head String.Map.empty
                                             Cnf.(key * logic') in
                                 lst @ acc
                               | Term.Choice (map, _) ->
                                 (* unreachable state: all terms must be ground *)
                                 failwith
                                   (Printf.sprintf "expected a ground list term, but %s found"
                                      (Term.to_string data))
                               | _ -> acc
                             )
                         ) in
        (* if tail variable does not have bounding term that is a choice,
           throw error *)
        let _ = if List.is_empty combined then
            Errors.unsat_error
              (Printf.sprintf "Missing record as a upper bound for variable $%s"
                 var) in
        (* verify the consistency of generated bounds *)
        let l = List.map combined
                  ~f:(fun (logic, lst) -> logic, Choice (lst, None)) in
        (*CM.of_alist_exn l*)
        map_of_alist_safe l
    end
  | Var x ->
    get_bound constrs x

let prepare_record_term cr cr' logic map =
  let new_map, new_map' =
    String.Map.fold map ~init:(String.Map.empty, String.Map.empty)
      ~f:(fun ~key ~data acc ->
        let map, map' = acc in
        let g, t = data in
        let contains, contains' =
          String.Set.mem !cr key, String.Set.mem !cr' key
        in
        if contains || contains' then
          map, map'
        (* TODO check whether the code fragment below is required *)
        else if contains then
          let _ = cr' := String.Set.add !cr' key in
          map, String.Map.add map' ~key ~data:(Cnf.(~-g), t)
        else if contains' then
          let _ = cr := String.Set.add !cr key in
          String.Map.add map ~key ~data:(Cnf.(~-g), t), map'
        else
          let _ = cr := String.Set.add !cr key in
          let _ = cr' := String.Set.add !cr' key in
          let v = Cnf.from_logic (Logic.Var (Transform.get_free_bool_var ())) in
          String.Map.add map ~key ~data:(Cnf.(g * v), t),
          String.Map.add map' ~key ~data:(Cnf.(g * ~-v), t)
      ) in
  (Cnf.Map.singleton logic (Term.Record (new_map, None))),
  (Cnf.Map.singleton logic (Term.Record (new_map', None)))

let rec set_bound_exn depth constrs var terms =
  let cstrs = ref constrs in
  let simplify t =
    let t = Term.canonize t in
    if Term.is_nil_exn t then Term.Nil else t in
  let union_variables = String.Map.find !Transform.union_variables var in
  let terms = Cnf.Map.fold terms ~init:Cnf.Map.empty
                ~f:(fun ~key ~data acc ->
                  let term = simplify data in
                  let _ = match union_variables with
                    | None -> ()
                    | Some (v, v') ->
                      let r, r' = GuardedVar.({ var = v; logic = key}, { var = v'; logic = key}) in
                      let var_cache, var_cache' =
                        Option.value (GuardedVar.Map.find !Transform.union_cache r)
                          ~default:String.Set.empty,
                        Option.value (GuardedVar.Map.find !Transform.union_cache r')
                          ~default:String.Set.empty
                      in
                      let cr, cr' = ref var_cache, ref var_cache' in
                      match term with
                      | Term.Record (map, None) ->
                        let left, right = prepare_record_term cr cr' key map in
                        cstrs := set_bound_exn (depth+1) !cstrs v left;
                        cstrs := set_bound_exn (depth+1) !cstrs v' right;
                        Transform.union_cache :=
                          GuardedVar.Map.change !Transform.union_cache r (fun _ -> Some !cr);
                        Transform.union_cache :=
                          GuardedVar.Map.change !Transform.union_cache r' (fun _ -> Some !cr')
                      | _ -> ()
                  in
                  add_to_map depth acc key term
                ) in
  let b = (String.make depth ' ') ^ "setting the least upper bound" in
  String.Map.change !cstrs var (fun v ->
    match v with
    | None ->
      SLog.logf "%s for variable $%s to <%s>" b var (print_map terms);
      Some terms
    | Some u ->
      let merged = merge_bounds (depth + 1) u terms in
      if Cnf.Map.is_empty merged then
        Errors.unsat_error
          (Printf.sprintf "the upper bounds for variable $%s are inconsistent"
             var)
      else if not (Cnf.Map.equal Term.equal merged u) then
        SLog.logf "%s for variable $%s to <%s>" b var (print_map merged);
      Some merged
  )

let poly_var_to_list depth constrs var logic_constr =
  match var with
  | None -> constrs, []
  | Some v ->
    let bounds = bound_terms_exn depth constrs logic_constr (Term.Var v) in
    Cnf.Map.fold bounds ~init:(constrs, [])
      ~f:(fun ~key ~data (constrs, acc) ->
        match data with
        | Term.List (x, None) -> constrs, (key, x) :: acc
        | Term.List (_, _) -> assert false
        | Term.Nil -> constrs, acc
        | _ ->
          add_bool_constr depth Cnf.(~-key * logic_constr);
          constrs, acc
      )

let set_list_constraints depth constrs map =
  Cnf.Map.fold map ~init:constrs
    ~f:(fun ~key ~data constrs ->
      match data with
      | Term.List _
      | Term.Nil -> constrs
      | _ ->
        add_bool_constr depth Cnf.(~-key);
        constrs
    )

let set_list_bound depth constrs v lst =
  let map = List.fold lst ~init:Cnf.Map.empty
              ~f:(fun acc (logic, lst) ->
                add_to_map depth acc logic (Term.List (lst, None))
              ) in
  let constrs = set_bound_exn (depth + 1) constrs v map in
  constrs

let assert_choice depth constrs s logic =
  choice_vars := String.Map.change !choice_vars s
                   (function None -> Some logic
                           | Some v -> Some Cnf.(v + logic)
                   );
  set_bound_exn depth constrs s (Cnf.Map.singleton logic Term.none)

let rec solve_senior depth constrs left right =
  let logic_left, term_left = left in
  let logic_right, term_right = right in
  let logic_combined = Cnf.(logic_left * logic_right) in
  let open Term in
  SLog.logf "%ssolving a constraint %s" (String.make depth ' ')
    (Constr.to_string (term_left, term_right));
  try
    match term_left, term_right with
    | _, Nil
    (* extra rule for overriding class member functions *)
    | _, Tuple [Symbol "override"; _] ->
      constrs
    | Var s, Var s' ->
      let left_choice = may_be_choice s logic_combined in
      let right_choice = may_be_choice s' logic_combined in
      if left_choice || right_choice then
        let constrs =
          if not left_choice then
            assert_choice depth constrs s logic_combined
          else constrs in
        let leftm = bound_terms_exn depth constrs logic_combined term_left in
        let constrs = set_bound_exn (depth + 1) constrs s' leftm in
        constrs
      else
        let rightm = bound_terms_exn depth constrs logic_combined term_right in
        let constrs = set_bound_exn (depth + 1) constrs s rightm in
        constrs
    (* atomic terms *)
    | Nil, Var s ->
      if may_be_choice s logic_combined then
        set_bound_exn (depth + 1) constrs s (Cnf.Map.singleton logic_combined Nil)
      else
        let leftm = bound_terms_exn depth constrs logic_combined term_left in
        let rightm = bound_terms_exn depth constrs logic_combined term_right in
        solve_senior_multi_exn (depth + 1) constrs leftm rightm
    (* TODO add "| List _" below *)
    | (OrdinalInt _ | NominalInt _ | Symbol _ | Tuple _ | Record _), Var s ->
      let leftm = bound_terms_exn depth constrs logic_combined term_left in
      let rightm = bound_terms_exn depth constrs logic_combined term_right in
      solve_senior_multi_exn (depth + 1) constrs leftm rightm
    | Var s, (OrdinalInt _ | NominalInt _ | Symbol _ | Tuple _ | List _ | Record _) ->
      let rightm = bound_terms_exn depth constrs logic_combined term_right in
      let constrs = set_bound_exn (depth + 1) constrs s rightm in
      constrs
    (* tuple processing *)
    | Tuple t, Tuple t' when Int.(List.length t = List.length t') ->
      if Int.(List.length t = List.length t') then
        List.fold2_exn t t' ~init:constrs
          ~f:(fun context t t' ->
            let left, right = (logic_left, t), (logic_right, t') in
            solve_senior (depth + 1) context left right
          )
      else
        raise (Incomparable_Terms (term_left, term_right))
    (* list processing *)
    (* TODO remove this case. See above *)
    | List _, Var s ->
      let bounds = bound_terms_exn depth constrs logic_combined term_right in
      Cnf.Map.fold bounds ~init:constrs
        ~f:(fun ~key ~data acc ->
          solve_senior (depth + 1) acc left (key, data)
        )
    | Nil, List (t, var) ->
      begin
        let constrs = List.fold t ~init:constrs
                        ~f:(fun acc el ->
                          let right = logic_right, el in
                          solve_senior (depth + 1) acc left right
                        ) in
        match var with
        | None -> constrs
        | Some v ->
          solve_senior (depth + 1) constrs left (logic_left, Var v)
      end
    | List (t, var), List (t', var') ->
      begin
        (* validate the common head of the list and return remaining elements
           (depending on lists lengths) *)
        let rec validate_head constrs l1 l2 = match l1, l2 with
          | [], _ -> constrs, [], l2
          | _, [] -> constrs, l1, []
          | hd :: tl, hd' :: tl' ->
            let left, right = (logic_left, hd), (logic_right, hd') in
            let constrs = solve_senior (depth +1) constrs left right in
            validate_head constrs tl tl' in
        let constrs, reml, remr = validate_head constrs t t' in
        (* an error if the tail of the right term is not nil *)
        if Poly.(var = None && remr <> [] &&
                 Term.is_nil (List (remr, None)) <> Some true) then
          raise (Term.Incomparable_Terms (term_left, term_right));
        match reml, var, var' with
        | [], None, None ->
          if List.is_empty remr then constrs
          (* the list to the right has higher arity *)
          else raise (Term.Incomparable_Terms (term_left, term_right))
        | [], None, Some v' ->
          let bounds = bound_terms_exn depth constrs logic_combined (Var v') in
          Cnf.Map.fold bounds ~init:constrs
            ~f:(fun ~key ~data constrs ->
              let l = Cnf.(logic_combined * key) in
              if may_be_choice v' l then
                set_bound_exn depth constrs v' (Cnf.Map.singleton l Term.Nil)
              else if Poly.(Term.is_nil data <> Some true) then
                let _ = add_bool_constr depth Cnf.(~-l) in
                constrs
              else
                constrs
            )
        | [], Some v, _ ->
          let tail_bounds = List.map remr
                              ~f:(fun x ->
                                bound_terms_exn depth constrs logic_combined x
                              ) in
          let tail_list = bound_combinations_list tail_bounds in
          let constrs, var_list =
            poly_var_to_list depth constrs var' logic_right in
          let merged = ref [] in
          if List.is_empty var_list then
            merged := tail_list
          else
            List.iter tail_list
              ~f:(fun (logic, lst) ->
                List.iter var_list
                  ~f:(fun (logic', lst') ->
                    merged := (Cnf.(logic * logic'), lst @ lst') :: !merged
                  )
              );
          set_list_bound depth constrs v !merged
        | reml, _, _ -> (* remr is empty *)
          begin
            let constrs, var_list =
              poly_var_to_list depth constrs var' logic_right in
            let heads lst =
              List.fold lst ~init:(Cnf.Map.empty, [])
                ~f:(fun (hds, tls) (logic, lst) ->
                  match lst with
                  | hd :: tl ->
                    (add_to_map depth hds logic hd), (logic, tl) :: tls
                  | [] ->
                    (add_to_map depth hds logic Term.Nil), (logic, []) :: tls
                ) in
            let constrs, tail_list =
              List.fold reml ~init:(constrs, var_list)
                ~f:(fun (constrs, tail_list) x ->
                  let head, tail_list = heads tail_list in
                  let head = if Cnf.Map.is_empty head then
                      Cnf.Map.singleton logic_right Term.Nil
                    else head in
                  let left = Cnf.Map.singleton logic_left term_left in
                  let constrs =
                    solve_senior_multi_exn (depth + 1) constrs left head in
                  constrs, tail_list
                ) in
            match var with
            | Some v -> set_list_bound depth constrs v tail_list
            | None -> constrs
          end
      end
    (* record/choice processing *)
    | Choice _, Var s ->
      let constrs = assert_choice depth constrs s logic_combined in
      let bounds = bound_terms_exn depth constrs logic_combined term_left in
      let constrs = set_bound_exn (depth + 1) constrs s bounds in
      constrs
    | Var s, Choice _ ->
      let constrs = assert_choice depth constrs s logic_combined in
      let bounds = bound_terms_exn depth constrs logic_combined term_left in
      Cnf.Map.fold bounds ~init:constrs ~f:(fun ~key ~data acc ->
        solve_senior (depth + 1) acc (key, data) right)
    | Nil, Choice (map, var)
    | Nil, Record (map, var) ->
      begin
        String.Map.iter map
          ~f:(fun ~key ~data ->
            let g, _ = data in
            add_bool_constr depth Cnf.(logic_combined * ~-g)
          );
        match var with
        | None -> constrs
        | Some v ->
          solve_senior (depth + 1) constrs left (logic_left, Var v)
      end
    (* RECORDS *)
    | Record (r, v), Record (r', v') ->
      begin
        let cstrs = ref constrs in
        let left_values = ref String.Map.empty in
        let right_values = ref String.Map.empty in
        String.Map.iter2 r r'
          ~f:(fun ~key ~data ->
            match data with
            | `Both ((g, t), (g', t')) ->
              begin
                try
                  cstrs := solve_senior (depth + 1) !cstrs (g, t) (g', t');
                  add_bool_constr depth Cnf.(logic_combined ==> (g' ==> g))
                with Errors.Unsatisfiability_Error _ ->
                  add_bool_constr depth Cnf.(~-(logic_combined * g'))
              end
            | `Left el ->
              left_values := String.Map.add !left_values ~key ~data:el
            | `Right el ->
              right_values := String.Map.add !right_values ~key ~data:el
          );
        (* PROCESSING LEFT TAIL VARIABLE *)
        let _ = match v with
          | None ->
            if not (String.Map.is_empty !right_values) then
              String.Map.iter !right_values
                ~f:(fun ~key ~data ->
                  let g, _ = data in
                  add_bool_constr depth Cnf.(logic_combined ==> ~-g)
                )
          | Some s ->
            (* possible values of the left tail variable *)
            let bounds = bound_terms_exn depth !cstrs logic_combined (Var s) in
            let result = ref Cnf.(Map.singleton logic_combined (Term.Record(!right_values, None))) in
            Cnf.Map.iter bounds
              ~f:(fun ~key ~data ->
                let logic = key in
                match data with
                | Nil -> ()
                | Record (tail_map, None) ->
                  begin
                    (* eliminate labels that duplicate in the record body and the tail *)
                    String.Map.iter2 r tail_map
                      ~f:(fun ~key ~data ->
                        match data with
                        | `Both ((g, _), (g', _)) ->
                          add_bool_constr depth Cnf.(~-(logic * logic_combined * g * g'))
                        | _ -> ()
                      );
                    (* compare with corresponding elements in the right term body *)
                    let missing_elements = ref String.Map.empty in
                    String.Map.iter2 tail_map !right_values
                      ~f:(fun ~key ~data ->
                        match data with
                        | `Left _ -> ()
                        | `Right data ->
                          missing_elements := String.Map.add !missing_elements ~key ~data
                        | `Both ((g, t), (g', t')) ->
                          begin
                            try
                              cstrs := solve_senior (depth + 1) !cstrs (g, t) (g', t');
                              add_bool_constr depth Cnf.(logic_combined ==> (logic ==> (g' ==> g)))
                            with Errors.Unsatisfiability_Error _ ->
                              add_bool_constr depth Cnf.(~-(logic_combined * logic * g'))
                          end
                      );
                    let missing_elements' = ref Cnf.Map.empty in
                    (* compare with corresponding elements in the right term tail variable *)
                    match v' with
                    | None -> ()
                    | Some s' ->
                      let bounds = bound_terms_exn depth !cstrs logic_combined (Var s') in
                      Cnf.Map.iter bounds
                        ~f:(fun ~key ~data ->
                          match data with
                          | Record (tail_map', None) ->
                            let record = ref String.Map.empty in
                            String.Map.iter2 tail_map tail_map'
                              ~f:(fun ~key ~data ->
                                match data with
                                | `Left _ -> ()
                                | `Right data ->
                                  record := String.Map.add !record ~key ~data
                                | `Both ((g, t), (g', t')) ->
                                  begin
                                    try
                                      cstrs := solve_senior (depth + 1) !cstrs (g, t) (g', t');
                                      add_bool_constr depth Cnf.(logic_combined ==> (logic ==> (g' ==> g)))
                                    with Errors.Unsatisfiability_Error _ ->
                                      add_bool_constr depth Cnf.(~-(logic_combined * logic * g'))
                                  end
                              );
                            missing_elements' := Cnf.Map.add !missing_elements' ~key ~data:!record
                          | _ -> ()
                        );
                      let value = Cnf.Map.fold !missing_elements' ~init:[(Cnf.make_true, String.Map.empty)]
                                    ~f:(fun ~key ~data acc ->
                                      let result = ref acc in
                                      result := List.map ~f:(fun (l, m) -> Cnf.(l * key), m) !result;
                                      String.Map.iter2 !missing_elements data
                                        ~f:(fun ~key ~data ->
                                          match data with
                                          | `Left el
                                          | `Right el ->
                                            result := List.map ~f:(fun (cond, map) -> cond, String.Map.add map ~key ~data:el) !result
                                          | `Both ((g, t), (g', t')) ->
                                            let l = List.map !result
                                                      ~f:(fun (cond, map) -> Cnf.(cond * ~-g'), String.Map.add map ~key ~data:(g, t)) in
                                            let l' = List.map !result
                                                       ~f:(fun (cond, map) -> Cnf.(cond * ~-g), String.Map.add map ~key ~data:(g', t')) in
                                            result := l @ l'
                                        );
                                      !result
                                    ) in
                      let value = List.map value ~f:(fun (l, m) -> l, Record (m, None)) in
                      let map, logic = Term.cnf_map_of_alist value in
                      add_bool_constr depth logic;
                      result := map
                  end
                | Choice _ ->
                  cstrs := set_bound_exn (depth + 1) constrs s Cnf.(Map.singleton (logic * logic_combined) Nil)
                | _ ->
                  add_bool_constr depth Cnf.(~-(logic * logic_combined))
              );
            cstrs := set_bound_exn (depth + 1) !cstrs s !result in
        (* PROCESSING RIGHT TAIL VARIABLE *)
        let _ = match v' with
          | None -> ()
          | Some s' ->
            (* possible values of the right tail variable *)
            let bounds = bound_terms_exn depth !cstrs logic_combined (Var s') in
            Cnf.Map.iter bounds
              ~f:(fun ~key ~data ->
                let logic = key in
                match data with
                | Nil -> ()
                | Record (tail_map, None) ->
                  begin
                    (* eliminate labels that duplicate in the record body and the tail *)
                    String.Map.iter2 r' tail_map
                      ~f:(fun ~key ~data ->
                        match data with
                        | `Both ((g, _), (g', _)) ->
                          add_bool_constr depth Cnf.(~-(logic * logic_combined * g * g'))
                        | _ -> ()
                      );
                    (* seniority relation with terms to the left *)
                    String.Map.iter2 r tail_map
                      ~f:(fun ~key ~data ->
                        match data with
                        | `Both ((g, t), (g', t')) ->
                          begin
                            try
                              cstrs := solve_senior (depth + 1) !cstrs (g, t) (g', t');
                              add_bool_constr depth Cnf.(logic_combined * (logic ==> (g' ==> g)))
                            with Errors.Unsatisfiability_Error _ ->
                              add_bool_constr depth Cnf.(~-(logic_combined * logic * g'))
                          end
                        | `Right (g, t) ->
                          begin
                            (* [key] label is in the right tail variable of the
                               right record.  If there is a tail of the left
                               term, we have to make [key] is included to the
                               corresponding record.  *)
                            match v with
                            | None ->
                              add_bool_constr depth Cnf.(~-(logic_combined * logic * g))
                            | Some tv ->
                              let bounds = bound_terms_exn depth !cstrs g t in
                              let bounds = Cnf.Map.map bounds
                                ~f:(fun t -> Record(String.Map.singleton key (g, t), None)) in
                              cstrs := set_bound_exn (depth + 1) !cstrs tv bounds
                          end
                        | _ -> ()
                      )
                  end
                | Choice _ ->
                  cstrs := set_bound_exn (depth + 1) constrs s' Cnf.(Map.singleton (logic * logic_combined) Nil)
                | _ ->
                  add_bool_constr depth Cnf.(~-(logic * logic_combined))
              ) in
        !cstrs
      end
    (* CHOICES *)
    | Choice (r, v), Choice (r', v') ->
      begin
        let cstrs = ref constrs in
        let left_values = ref String.Map.empty in
        let right_values = ref String.Map.empty in
        String.Map.iter2 r r'
          ~f:(fun ~key ~data ->
            match data with
            | `Both ((g, t), (g', t')) ->
              begin
                try
                  cstrs := solve_senior (depth + 1) !cstrs (g, t) (g', t');
                  add_bool_constr depth Cnf.(logic_combined ==> (g ==> g'))
                with Errors.Unsatisfiability_Error _ ->
                  add_bool_constr depth Cnf.(~-(logic_combined * g))
              end
            | `Left el ->
              left_values := String.Map.add !left_values ~key ~data:el
            | `Right el ->
              right_values := String.Map.add !right_values ~key ~data:el
          );
        (* PROCESSING RIGHT TAIL VARIABLE *)
        let _ = match v' with
          | None ->
            if not (String.Map.is_empty !left_values) then
              String.Map.iter !left_values
                ~f:(fun ~key ~data ->
                  let g, _ = data in
                  add_bool_constr depth Cnf.(logic_combined ==> ~-g)
                )
          | Some s' ->
            (* possible values of the right tail variable *)
            (* if [s'] is not a choice, then we need to drop all the current
               values to [none] and to start computations again from the bottom
               of the semilattice *)
            if not (may_be_choice s' logic_combined) then
              cstrs := assert_choice depth !cstrs s' logic_combined;
            let bounds = bound_terms_exn depth !cstrs logic_combined (Var s') in
            let result = ref Cnf.(Map.singleton logic_combined (Term.Choice(!left_values, None))) in
            Cnf.Map.iter bounds
              ~f:(fun ~key ~data ->
                let logic = key in
                match data with
                | Choice (tail_map, None) ->
                  begin
                    (* eliminate labels that duplicate in the record body and the tail *)
                    String.Map.iter2 r' tail_map
                      ~f:(fun ~key ~data ->
                        match data with
                        | `Both ((g, _), (g', _)) ->
                          add_bool_constr depth Cnf.(~-(logic * logic_combined * g * g'))
                        | _ -> ()
                      );
                    (* compare with corresponding elements in the left term body *)
                    let missing_elements = ref String.Map.empty in
                    String.Map.iter2 !left_values tail_map
                      ~f:(fun ~key ~data ->
                        match data with
                        | `Right _ -> ()
                        | `Left data ->
                          missing_elements := String.Map.add !missing_elements ~key ~data
                        | `Both ((g, t), (g', t')) ->
                          begin
                            try
                              cstrs := solve_senior (depth + 1) !cstrs (g, t) (g', t');
                              add_bool_constr depth Cnf.(logic_combined ==> (logic ==> (g ==> g')))
                            with Errors.Unsatisfiability_Error _ ->
                              add_bool_constr depth Cnf.(~-(logic_combined * logic * g))
                          end
                      );
                    let missing_elements' = ref Cnf.Map.empty in
                    (* compare with corresponding elements in the right term tail variable *)
                    match v with
                    | None -> ()
                    | Some s ->
                      (* if [s] is not a choice, then we need to drop all the
                         current values to [none] and to start computations
                         again from the bottom of the semilattice *)
                      if not (may_be_choice s logic_combined) then
                        cstrs := assert_choice depth !cstrs s logic_combined;
                      let bounds = bound_terms_exn depth !cstrs logic_combined (Var s) in
                      Cnf.Map.iter bounds
                        ~f:(fun ~key ~data ->
                          match data with
                          | Choice (tail_map', None) ->
                            let choice = ref String.Map.empty in
                            String.Map.iter2 tail_map' tail_map
                              ~f:(fun ~key ~data ->
                                match data with
                                | `Left _ -> ()
                                | `Right data ->
                                  choice := String.Map.add !choice ~key ~data
                                | `Both ((g, t), (g', t')) ->
                                  begin
                                    try
                                      cstrs := solve_senior (depth + 1) !cstrs (g, t) (g', t');
                                      add_bool_constr depth Cnf.(logic_combined ==> (logic ==> (g ==> g')))
                                    with Errors.Unsatisfiability_Error _ ->
                                      add_bool_constr depth Cnf.(~-(logic_combined * logic * g))
                                  end
                              );
                            missing_elements' := Cnf.Map.add !missing_elements' ~key ~data:!choice
                          | _ -> ()
                        );
                      let value = Cnf.Map.fold !missing_elements' ~init:[(Cnf.make_true, String.Map.empty)]
                                    ~f:(fun ~key ~data acc ->
                                      let result = ref acc in
                                      result := List.map ~f:(fun (l, m) -> Cnf.(l * key), m) !result;
                                      String.Map.iter2 !missing_elements data
                                        ~f:(fun ~key ~data ->
                                          match data with
                                          | `Left el
                                          | `Right el ->
                                            result := List.map ~f:(fun (cond, map) -> cond, String.Map.add map ~key ~data:el) !result
                                          | `Both ((g, t), (g', t')) ->
                                            let l = List.map !result
                                                      ~f:(fun (cond, map) -> Cnf.(cond * ~-g'), String.Map.add map ~key ~data:(g, t)) in
                                            let l' = List.map !result
                                                       ~f:(fun (cond, map) -> Cnf.(cond * ~-g), String.Map.add map ~key ~data:(g', t')) in
                                            result := l @ l'
                                        );
                                      !result
                                    ) in
                      let value = List.map value ~f:(fun (l, m) -> l, Choice (m, None)) in
                      let map, logic = Term.cnf_map_of_alist value in
                      add_bool_constr depth logic;
                      result := map
                  end
                | Nil ->
                  if may_be_choice s' Cnf.(logic * logic_combined) then
                    add_bool_constr depth Cnf.(~-(logic * logic_combined))
                  else
                    cstrs := assert_choice depth !cstrs s' Cnf.(logic * logic_combined)
                | _ ->
                  add_bool_constr depth Cnf.(~-(logic * logic_combined))
              );
            (*cstrs := assert_choice depth !cstrs s' Cnf.(logic_combined);*)
            cstrs := set_bound_exn (depth + 1) !cstrs s' !result in
        (* PROCESSING LEFT TAIL VARIABLE *)
        let _ = match v with
          | None -> ()
          | Some s ->
            (* possible values of the right tail variable *)
            (* if [s] is not a choice, then we need to drop all the current
               values to [none] and to start computations again from the bottom
               of the semilattice *)
            if not (may_be_choice s logic_combined) then
              cstrs := assert_choice depth !cstrs s logic_combined;
            let bounds = bound_terms_exn depth !cstrs logic_combined (Var s) in
            Cnf.Map.iter bounds
              ~f:(fun ~key ~data ->
                let logic = key in
                match data with
                | Choice (tail_map, None) ->
                  begin
                    (* eliminate labels that duplicate in the record body and the tail *)
                    String.Map.iter2 r tail_map
                      ~f:(fun ~key ~data ->
                        match data with
                        | `Both ((g, _), (g', _)) ->
                          add_bool_constr depth Cnf.(~-(logic * logic_combined * g * g'))
                        | _ -> ()
                      );
                    (* seniority relation with terms to the right *)
                    String.Map.iter2 tail_map r'
                      ~f:(fun ~key ~data ->
                        match data with
                        | `Both ((g, t), (g', t')) ->
                          begin
                            try
                              cstrs := solve_senior (depth + 1) !cstrs (g, t) (g', t');
                              add_bool_constr depth Cnf.(logic_combined * (logic ==> (g ==> g')))
                            with Errors.Unsatisfiability_Error _ ->
                              add_bool_constr depth Cnf.(~-(logic_combined * logic * g))
                          end
                        | `Left (g, t) ->
                          begin
                            (* [key] label is in the tail variable of the
                               left choice.  If there is a tail of the right
                               term, we have to make [key] is included to the
                               corresponding choice.  *)
                            match v' with
                            | None ->
                              add_bool_constr depth Cnf.(~-(logic_combined * logic * g))
                            | Some tv ->
                              let bounds = bound_terms_exn depth !cstrs g t in
                              let bounds = Cnf.Map.map bounds
                                             ~f:(fun t -> Choice(String.Map.singleton key (g, t), None)) in
                              cstrs := set_bound_exn (depth + 1) !cstrs tv bounds
                          end
                        | _ -> ()
                      )
                  end
                | Nil ->
                  if may_be_choice s Cnf.(logic * logic_combined) then
                    add_bool_constr depth Cnf.(~-(logic * logic_combined))
                  else
                    cstrs := assert_choice depth !cstrs s Cnf.(logic * logic_combined)
                | _ ->
                  add_bool_constr depth Cnf.(~-(logic * logic_combined))
              ) in
        !cstrs
      end
    (* switch processing *)
    | Switch leftm, Var s ->
      let rightm = bound_terms_exn depth constrs logic_combined term_right in
      solve_senior_multi_exn (depth + 1) constrs leftm rightm
    | Var s, Switch rightm ->
      let rightm = bound_terms_exn depth constrs logic_combined term_right in
      let constrs = set_bound_exn (depth + 1) constrs s rightm in
      constrs
    | Switch leftm, Switch rightm ->
      solve_senior_multi_exn (depth + 1) constrs leftm rightm
    | t, Switch right_map ->
      let left_map = Cnf.Map.singleton logic_left term_left in
      solve_senior_multi_exn (depth + 1) constrs left_map right_map
    | Switch left_map, t ->
      let right_map = Cnf.Map.singleton logic_right term_right in
      solve_senior_multi_exn (depth + 1) constrs left_map right_map
    | t, t' ->
      if Int.(Term.seniority_exn t t' = -1) then
        raise (Term.Incomparable_Terms (t, t'))
      else constrs
  with Term.Incomparable_Terms (t1, t2) ->
    let logic = Cnf.(~-(logic_left * logic_right)) in
    if Cnf.is_false logic then
      Errors.unsat_error (sprintf "the seniority relation %s <= %s does not hold"
                            (Term.to_string term_left) (Term.to_string term_right))
    else begin
      add_bool_constr depth logic;
      constrs
    end

and solve_senior_multi_exn depth context leftm rightm =
  Cnf.Map.fold leftm ~init:context
    ~f:(fun ~key ~data acc ->
      let logic_left, term_left = key, data in
      Cnf.Map.fold rightm ~init:acc
        ~f:(fun ~key ~data acc ->
          let logic_right, term_right = key, data in
          solve_senior depth acc (logic_left, term_left)
            (logic_right, term_right)
        )
    )

let apply constraints (left, right) =
  let l_map = Cnf.Map.singleton Cnf.make_true left in
  let r_map = Cnf.Map.singleton Cnf.make_true right in
  constraints := solve_senior_multi_exn 0 !constraints l_map r_map

(* a helper function to compare two context structures *)
let ctx_equal (constrs, logic) (constrs', logic') =
  (* For sets of solutions [logic] and [logic'],
     |logic| > |logic'| and
     The set of solutions for [logic] includes the set of solutions for
     [logic']. *)
  let log_expr = Cnf.(logic ==> logic') in
  if not (Cnf.CSet.equal logic logic') &&
     Option.is_some (Sat.solve log_expr) then false
  else
    let eq = ref true in
    String.Map.iter2 constrs constrs'
      ~f:(fun ~key ~data ->
        if !eq then
          match data with
          | `Left v
          | `Right v -> eq := false
          | `Both (v1, v2) ->
            Cnf.Map.iter2 v1 v2
              ~f:(fun ~key ~data ->
                if !eq then
                  match data with
                  | `Left v
                  | `Right v -> eq := false
                  | `Both (v1, v2) ->
                    if Term.(v1 <> v2) then eq := false
              )
      );
    !eq

let resolve_bound_constraints topo =
  SLog.logf "setting the least upper bounds for constraints";
  let fixed_point = ref false in
  let iter_counter = ref 1 in
  let constrs, bools = ref String.Map.empty, boolean_constraints in
  (* Set the initial upper bound, which is nil, for all term variables *)
  (*let union =*)
  (*String.Set.union !Transform.initial_term_variables !Transform.additional_term_variables in*)
  (*String.Set.iter union*)
  (*~f:(fun el ->*)
  (*constrs := String.Map.add !constrs ~key:el ~data:(Cnf.Map.singleton Cnf.make_true Term.Nil)*)
  (* );*)
  while not !fixed_point do
    SLog.logf "iteration #%d" !iter_counter;
    let constrs', bools' = ref !constrs, ref !bools in
    List.iter ~f:(apply constrs) topo;
    (* The solver terminates when either the bounds stop to change or the
       number of iterations exceeds the iteration limit. *)
    if ctx_equal (!constrs, !bools) (!constrs', !bools') || !iter_counter > !iteration_limit then
      fixed_point := true
    else
      iter_counter := !iter_counter + 1
  done;
  !constrs

let add_boolean_constraints constrs =
  String.Map.iter constrs
    ~f:(fun ~key ~data ->
      let expr =
        Cnf.Map.fold data ~init:Cnf.make_false
          ~f:(fun ~key ~data acc -> Cnf.(key + acc)) in
      add_bool_constr 0 expr
    )

let add_union_boolean_constraints constrs =
  String.Map.iter !Transform.union_variables
    ~f:(fun ~key ~data ->
      let var, var' = data in
      let default_map = Cnf.Map.singleton Cnf.make_true Term.Nil in
      let map = Option.value ~default:default_map (String.Map.find constrs var) in
      let map' = Option.value ~default:default_map (String.Map.find constrs var') in
      Cnf.Map.iter map
        ~f:(fun ~key ~data ->
          let condition, term = key, data in
          match term with
          | Term.Record (map, None) ->
            Cnf.Map.iter map'
              ~f:(fun ~key ~data ->
                let condition', term' = key, data in
                match term' with
                | Term.Record (map', None) ->
                  begin
                  if Option.is_some (Sat.solve Cnf.(condition <=> condition')) then
                    String.Map.iter2 map map'
                      ~f:(fun ~key ~data ->
                        match data with
                        | `Both ((g, t), (g', t')) ->
                          (*printf "%s or %s\n" (Cnf.to_string g) (Cnf.to_string g');*)
                          add_bool_constr 1 Cnf.(~-(g * g'))
                        | _ -> ()
                      )
                  end
                | _ -> ()
              )
          | _ -> ()
        )
    )


let solve_exn lst logic verbose limit =
  let _ = verbose_output := verbose in
  let _ = match limit with
    | None -> ()
    | Some limit -> iteration_limit := limit in
  boolean_constraints := logic;
  let constrs = resolve_bound_constraints lst in
  add_boolean_constraints constrs;
  add_union_boolean_constraints constrs;
  if !verbose_output then
    begin
      printf "Upper bounds for term variables:\n";
      String.Map.iter constrs
        ~f:(fun ~key ~data ->
          printf "$%s:\n" key;
          Cnf.Map.iter data
            ~f:(fun ~key ~data ->
              printf "  %s -> %s\n" (Cnf.to_string key) (Term.to_string data)
            )
        );
      printf "Boolean constraints:\n  %s\n" Cnf.(to_string (simplify !boolean_constraints));
    end;
  try
    match Sat.solve_max !boolean_constraints with
    | None -> None
    | Some bool_map ->
      let union =
        String.Set.union !Transform.initial_bool_variables !Transform.additional_bool_variables in
      let bool_map = String.Set.fold union ~init:bool_map
                       ~f:(fun acc el ->
                         if not (String.Map.mem bool_map el) then
                           String.Map.add acc ~key:el ~data:true
                         else acc
                       ) in
      Some (bool_map, (Constr.substitute constrs bool_map))
  with No_Solution t ->
    Errors.unsat_error t
