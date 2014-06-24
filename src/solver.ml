open Core.Std

module SLog = Log.Make(struct let section = "solver:" end)
module LLog = Log.Make(struct let section = "logic:" end)

exception Unsatisfiability_Error of string

let unsat_error msg =
  let open Errors in
  raise (Unsatisfiability_Error (error msg))

let iteration_limit = ref 100

let verbose_output = ref false

let boolean_constraints = ref Cnf.CSet.empty

let log_bool_constr depth b =
  let indent = String.make depth ' ' in
  LLog.logf "%sadding boolean constraint '%s'" indent (Cnf.to_string b)

let add_bool_constr depth b =
  log_bool_constr depth b;
  boolean_constraints := Cnf.CSet.union !boolean_constraints b

let print_map tm =
  let tl = Cnf.Map.to_alist tm in
  let sl = List.map tl ~f:(fun (l, t) ->
    Printf.sprintf "[%s]%s" (Cnf.to_string l) (Term.to_string t)) in
  String.concat ~sep:", " sl

let get_bound constrs var =
  match String.Map.find constrs var with
  | None -> Cnf.Map.singleton Cnf.make_true Term.Nil
  | Some upper when Cnf.Map.is_empty upper ->
    Cnf.Map.singleton Cnf.make_true Term.Nil
  | Some upper -> upper

let add_to_map depth map logic term =
  match Sat.solve Cnf.(~-(logic <=> Cnf.make_false)) with
  | None -> map
  | Some _ ->
    let added = ref false in
    let map = Cnf.Map.fold map ~init:Cnf.Map.empty
      ~f:(fun ~key ~data acc ->
        (*print_endline (Cnf.to_string key);*)
        (*print_endline (Cnf.to_string logic);*)
        (*let _ = Cnf.(~-(key <=> logic)) in*)
        match Sat.solve Cnf.(~-(key <=> logic)) with  (* FIXME bottleneck *)
        | None ->
          begin
          let _ = added := true in
          match Term.join data term with
          | None ->
            let _ = add_bool_constr depth Cnf.(~-logic) in
            acc
          | Some glb ->
            Cnf.Map.add map ~key ~data:glb
          end
        | Some _ ->
          Cnf.Map.add map ~key ~data
      ) in
    if not !added then Cnf.Map.add map ~key:logic ~data:term
    else map

let merge_bounds depth old_terms new_terms =
  if Cnf.Map.equal Term.equal old_terms new_terms then old_terms
  else
    let map = ref Cnf.Map.empty in
        let iter ~key ~data =
      let logic, term = key, data in
      let iter' ~key ~data =
        let logic', term' = key, data in
        match Sat.solve Cnf.(~-(logic <=> logic')) with
        (* logic expressions are equal *)
        | None ->
          begin
            match Term.join term term' with
            (* terms are incomparable *)
            | None -> add_bool_constr depth logic
            | Some join_term ->
                map := add_to_map depth !map logic join_term
          end
        | Some _ ->
          map := add_to_map depth !map Cnf.(logic * ~-logic') term;
          map := add_to_map depth !map Cnf.(~-logic * logic') term';
          match Term.join term term' with
          | None ->
            add_bool_constr depth Cnf.(~-(logic * logic'))
          | Some join_term ->
            map := add_to_map depth !map Cnf.(logic * logic') join_term
      in
      Cnf.Map.iter new_terms ~f:iter' in
    if Cnf.Map.is_empty old_terms && Cnf.Map.is_empty new_terms then
      Cnf.Map.singleton Cnf.make_true Term.Nil
    else if Cnf.Map.is_empty old_terms then new_terms
    else if Cnf.Map.is_empty new_terms then old_terms
    else begin
      Cnf.Map.iter old_terms ~f:iter;
      !map
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
          unsat_error (Printf.sprintf "constraint violation between old bound
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

let rec bound_terms_exn depth constrs logic term =
  let open Term in
  let module CM = Cnf.Map in
  match term with
  | Nil | Int _ | Symbol _ -> CM.singleton logic term
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
    let result = CM.of_alist_exn term_list in
    result
  | List (x, s) ->
    let l = List.map x ~f:(fun x -> bound_terms_exn depth constrs logic x) in
    let term_list = bound_combinations_list l in
    begin
      match s with
      | None ->
        CM.of_alist_exn
          (List.map term_list ~f:(fun (l, t) -> l, List (t, None)))
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
            (*printf "%s -> %s" (Cnf.to_string l) (Term.to_string (List(t,*)
            (*None)));*)
            (*print_newline ();*)
            add_to_map depth acc l (List(t, None))
          )
        (*List.iter lst ~f:(fun (l, t) -> printf "%s -> %s\n" (Cnf.to_string l) (Term.to_string (List(t, None))));*)
        (*CM.of_alist_exn (List.map lst ~f:(fun (l, t) -> l, List (t, None)))*)
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
        CM.of_alist_exn combined
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
          unsat_error 
            (Printf.sprintf "Missing record as a upper bound for variable $%s"
              var) in
        (* vefify the consistency of generated bounds *)
        let l = List.map combined
          ~f:(fun (logic, lst) -> logic, Record (lst, None)) in
        CM.of_alist_exn l
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
        CM.of_alist_exn combined
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
          unsat_error
            (Printf.sprintf "Missing record as a upper bound for variable $%s"
            var) in
        (* vefify the consistency of generated bounds *)
        let l = List.map combined
          ~f:(fun (logic, lst) -> logic, Choice (lst, None)) in
        CM.of_alist_exn l
    end
  | Var x ->
    get_bound constrs x

let set_bound_exn depth constrs var terms =
  let simplify t =
    let t = Term.canonize t in
    if Term.is_nil_exn t then Term.Nil else t in
  let terms = Cnf.Map.fold ~init:Cnf.Map.empty
    ~f:(fun ~key ~data acc -> add_to_map depth acc key (simplify data))
  terms in
  let b = (String.make depth ' ') ^ "setting least upper bound" in
  String.Map.change constrs var (fun v ->
    match v with
    | None ->
      SLog.logf "%s for variable $%s to '%s'" b var (print_map terms);
      Some terms
    | Some u ->
      let merged = merge_bounds (depth + 1) u terms in
      if Cnf.Map.is_empty merged then
        unsat_error
          (Printf.sprintf "the upper bounds for variable $%s are inconsistent"
            var)
      else
        SLog.logf "%s for variable $%s to {%s}" b var (print_map merged);
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

let rec solve_senior depth constrs left right =
  let logic_left, term_left = left in
  let logic_right, term_right = right in
  let logic_combined = Cnf.(logic_left * logic_right) in
  let open Term in
  try
    SLog.logf "%ssolving constraint %s <= %s" (String.make depth ' ')
      (to_string term_left) (to_string term_right);
    match term_left, term_right with
    | Var s, Var s' ->
      let rightm = bound_terms_exn depth constrs logic_combined term_right in
      let constrs = set_bound_exn (depth + 1) constrs s rightm in
      constrs
    (* atomic terms *)
    | (Nil | Int _ | Symbol _), Var s ->
      let leftm = bound_terms_exn depth constrs logic_combined term_left in
      let rightm = bound_terms_exn depth constrs logic_combined term_right in
      solve_senior_multi_exn (depth + 1) constrs leftm rightm
    | Var s, (Nil | Int _ | Symbol _) ->
      let rightm = bound_terms_exn depth constrs logic_combined term_right in
      let constrs = set_bound_exn (depth + 1) constrs s rightm in
      constrs
    (* tuple processing *)
    | Tuple t, Nil ->
      List.fold t ~init:constrs
        ~f:(fun acc el ->
          let left = logic_left, el in
          solve_senior (depth + 1) acc left right
        )
    | Nil, Tuple t ->
        List.fold t ~init:constrs
          ~f:(fun acc el ->
            let right = logic_right, el in
            solve_senior (depth + 1) acc left right
          )
    | Tuple t, Tuple t' when Int.(List.length t = List.length t') ->
      if Int.(List.length t = List.length t') then
        List.fold2_exn t t' ~init:constrs
          ~f:(fun context t t' ->
            let left, right = (logic_left, t), (logic_right, t') in
            solve_senior (depth + 1) context left right
          )
      else
        raise (Incomparable_Terms (term_left, term_right))
    | Tuple t, Var s ->
      let solve ~key ~data acc =
        let right = key, data in
        solve_senior (depth + 1) constrs left right in
      let rightm = bound_terms_exn depth constrs logic_combined term_right in
      Cnf.Map.fold rightm ~init:constrs ~f:solve
    | Var s, Tuple t ->
      let bounds = bound_terms_exn depth constrs logic_combined term_right in
      let constrs = set_bound_exn (depth + 1) constrs s bounds in
      constrs
    (* list processing *)
    | List _, Var v ->
      let bounds = bound_terms_exn depth constrs logic_combined term_right in
      Cnf.Map.fold bounds ~init:constrs
        ~f:(fun ~key ~data acc ->
          solve_senior (depth + 1) constrs left (key, data)
        )
    | Var v, List _ ->
      let bounds = bound_terms_exn depth constrs logic_combined term_right in
      let constrs = set_bound_exn (depth + 1) constrs v bounds in
      constrs
    | List (t, var), Nil ->
      begin
        let constrs = List.fold t ~init:constrs
          ~f:(fun acc el ->
            let left = logic_left, el in
            solve_senior (depth + 1) acc left right
          ) in
        match var with
        | None -> constrs
        | Some v ->
          solve_senior (depth + 1) constrs (logic_left, Var v) right
      end
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
        constrs
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
      | reml, _, _ ->
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
    | Choice _, Var s
    | Record _, Var s ->
      let bounds = bound_terms_exn depth constrs logic_combined term_right in
      Cnf.Map.fold bounds ~init:constrs ~f:(fun ~key ~data acc ->
        solve_senior (depth + 1) constrs left (key, data))
    | Var s, Choice _
    | Var s, Record _ ->
      let bounds = bound_terms_exn depth constrs logic_combined term_right in
      let constrs = set_bound_exn (depth + 1) constrs s bounds in
      constrs
    | Choice (map, var), Nil
    | Record (map, var), Nil ->
      begin
        let constrs = String.Map.fold map ~init:constrs
          ~f:(fun ~key ~data acc ->
            let l, t = data in
            solve_senior (depth + 1) acc (l, t) right
          ) in
        match var with
        | None -> constrs
        | Some v ->
          solve_senior (depth + 1) constrs (logic_left, Var v) right
      end
    | Nil, Choice (map, var)
    | Nil, Record (map, var) ->
      begin
        String.Map.iter map
          ~f:(fun ~key ~data ->
            let g, _ = data in
            add_bool_constr depth Cnf.(~-g)
          );
        match var with
        | None -> constrs
        | Some v ->
          solve_senior (depth + 1) constrs left (logic_left, Var v)
      end
    | Record (r, v), Record (r', v') ->
      begin
        let bounds = bound_terms_exn depth constrs logic_combined term_right in
        (* set bounds for all terms from the left to nil *)
        let constrs = String.Map.fold r ~init:constrs
          ~f:(fun ~key ~data acc ->
            let (guard, term) = data in
              solve_senior (depth + 1) acc (Cnf.(guard * logic_left), term)
                (logic_right, Nil)
          ) in
        let constrs_ref = ref constrs in
        let var_bounds = ref Cnf.Map.empty in
        Cnf.Map.iter bounds 
          ~f:(fun ~key ~data ->
            let logic = Cnf.(key * logic_right) in
            match data with
            | Record (r', None) ->
              begin
              let var_rec = ref String.Map.empty in
              let var_logic = ref logic in
              String.Map.iter r'
                ~f:(fun ~key ~data ->
                  let label, (guard, term) = key, data in
                  match String.Map.find r key with
                  | None ->
                    var_rec := String.Map.add !var_rec ~key:label
                      ~data:(Cnf.(guard * logic), term);
                    var_logic := Cnf.(!var_logic * guard)
                  | Some (guard', term') ->
                    try
                      let constrs = solve_senior (depth + 1) !constrs_ref
                      (guard', term') (guard, term) in
                      add_bool_constr depth Cnf.(guard ==> guard');
                      constrs_ref := constrs
                    with Unsatisfiability_Error _ ->
                      var_logic := Cnf.(!var_logic * ~- guard * ~-guard'));
                  if not (String.Map.is_empty !var_rec) then
                    var_bounds := add_to_map depth !var_bounds !var_logic
                      (Record(!var_rec, None))
              end
            | _ -> assert false
          );
          let constrs = !constrs_ref in
          match v with
          | None ->
            Cnf.Map.iter !var_bounds
              ~f:(fun ~key ~data -> add_bool_constr depth Cnf.(~-key));
            constrs
          | Some var ->
            let var_bounds =
              if Cnf.Map.is_empty !var_bounds then
                ref (Cnf.Map.singleton logic_left Term.Nil)
              else var_bounds in
            let constrs = set_bound_exn (depth + 1) constrs var !var_bounds in
            constrs
      end
    | Choice (r, v), Choice (r', v') ->
      begin
        let bounds = bound_terms_exn depth constrs logic_combined term_right in
        let constrs_ref = ref constrs in
        let var_bounds = ref Cnf.Map.empty in
        Cnf.Map.iter bounds
          ~f:(fun ~key ~data ->
            let logic = Cnf.(key * logic_right) in
            match data with
            | Choice (r', None) ->
              begin
              let var_rec = ref String.Map.empty in
              let var_logic = ref logic in
              String.Map.iter r' ~f:(fun ~key ~data ->
                let label, (guard, term) = key, data in
                match String.Map.find r label with
                | None ->
                  var_rec := String.Map.add !var_rec ~key:label
                    ~data:(Cnf.(guard * logic), term);
                  var_logic := Cnf.(!var_logic * guard)
                | Some (guard', term') ->
                  try
                    let constrs = solve_senior (depth + 1) !constrs_ref
                      (guard', term') (guard, term) in
                    add_bool_constr depth Cnf.(guard' ==> guard);
                    constrs_ref := constrs
                  with Unsatisfiability_Error _ ->
                    var_logic := Cnf.(!var_logic * ~-guard * ~-guard'));
              if not (String.Map.is_empty !var_rec) then
                var_bounds := add_to_map depth !var_bounds !var_logic
                  (Choice(!var_rec, None))
              end
            | _ -> assert false
          );
        let constrs = !constrs_ref in
        match v with
        | None ->
          constrs
        | Some var ->
          set_bound_exn (depth + 1) constrs var !var_bounds
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
      unsat_error (sprintf "the seniority relation %s <= %s does not hold"
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
  SLog.logf "setting least upper bounds for constraints";
  let fixed_point = ref false in
  let iter_counter = ref 1 in
  let constrs, bools = ref String.Map.empty, boolean_constraints in
  while not !fixed_point do
    SLog.logf "iteration #%d" !iter_counter;
    let constrs', bools' = ref !constrs, ref !bools in
    List.iter ~f:(apply constrs) topo;
    (* The solver terminates when either the bounds stop to change or the
       number of iterations exceeds the iteration limit. *)
    if ctx_equal (!constrs, !bools) (!constrs', !bools')
      || !iter_counter > !iteration_limit then
      fixed_point := true
    else
      iter_counter := !iter_counter + 2;
  done;
  !constrs

let add_boolean_constraints constrs =
  String.Map.iter constrs
    ~f:(fun ~key ~data ->
      let expr =
        Cnf.Map.fold data ~init:Cnf.make_false
          ~f:(fun ~key ~data acc -> Cnf.(acc + key)) in
      add_bool_constr 0 expr
      (* predicate pairwise exclusion is disabled *)
      (*let lst = Cnf.Map.keys data in*)
      (*if List.length lst > 1 then*)
        (*let expr' = Cnf.pairwise_not_and (Cnf.Map.keys data) in*)
        (*add_bool_constr 0 expr'*)
    )

let solve_exn lst logic verbose limit =
  let _ = verbose_output := verbose in
  let _ = match limit with
  | None -> ()
  | Some limit -> iteration_limit := limit in
  boolean_constraints := logic;
  let constrs = resolve_bound_constraints lst in
  add_boolean_constraints constrs;
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
    printf "Boolean constraints:\n  %s\n" (Cnf.to_string !boolean_constraints);
    end;
  try
    match Sat.solve !boolean_constraints with
    | None -> None
    | Some bool_map ->
      Some (bool_map, (Constr.substitute constrs bool_map))
  with Constr.No_Solution t ->
    unsat_error t
