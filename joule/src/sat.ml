open Core.Std

type solution =
  | Any
  | Solutions of bool Int.Map.t list

 (* get a model from the solver.
   NOTE: [solve] function must be called before. *)
let get_solution psat =
  let values = ref [] in
  let max_idx = Picosat.variables !psat in
  for i = 1 to max_idx do
    values := (Picosat.deref !psat i) :: !values
  done;
  if List.mem !values 0 then None
  else Some (List.rev !values)

(* add constraints provided in the CNF form to PicoSat *)
let cnf_to_psat cnf =
  let module P = Picosat in
  let psat = ref (P.init ()) in
  let cnf = [cnf] :: (Cnf.get_cnf ()) in
  List.iter
    ~f:(fun lst ->
      if not (List.is_empty lst) then
      begin
        (*List.iter ~f:(fun el -> ignore (P.add !psat el); printf "%d " el) lst;*)
        (*printf "\n";*)
        List.iter ~f:(fun el -> ignore (P.add !psat el)) lst;
        ignore (P.add !psat 0)
      end
    ) cnf;
  psat

let find_models cnf single_model =
  if Cnf.is_false cnf then Solutions []
  else if Cnf.is_true cnf then Any
  else
    let psat = cnf_to_psat cnf in
    let result = ref Set.Poly.empty in
    let loop = ref true in
    while !loop do
      if Poly.(Picosat.sat !psat Int.(~-1) = Picosat.Satisfiable) then
        match get_solution psat with
        | Some x ->
          let assignment = ref Int.Map.empty in
          let counter = ref 1 in
          List.iter
            ~f:(fun v ->
              ignore (Picosat.add !psat Int.(~-1 * !counter * v));
              (* add a variable value to the assignment *)
              assignment := Int.Map.add ~key:!counter ~data:Int.(v > 0) !assignment;
              counter := Int.(!counter + 1)
            ) x;
          ignore (Picosat.add !psat 0);
          (*printf "\n";*)
          result := Set.Poly.add !result !assignment;
          if single_model then loop := false
        | None -> loop := false
      else
        loop := false
    done;
    Solutions (Set.Poly.to_list !result)

let solve cnf =
  match find_models cnf true with
  | Any -> Some Int.Map.empty
  | Solutions [] -> None
  | Solutions (hd :: tl) -> Some hd

let solve_max ?(verbose=false) cnf =
  match find_models cnf false with
  | Any -> Some Int.Map.empty
  | Solutions [] -> None
  | Solutions (hd :: tl) ->
    let counter = ref 1 in
    let print_solution map =
      printf "%d | " !counter;
      Int.Map.iter map
        ~f:(fun ~key ~data ->
          match Cnf.int_to_var key with
          | Some _ -> printf "%s " (if data then "true" else "false")
          | None -> ()
        );
      printf "\n";
      counter := !counter + 1
    in
    let _ =
      if verbose then
        let _ = printf "\nList of SAT solutions:\n" in
        let _ = Int.Map.iter hd
          ~f:(fun ~key ~data ->
            match Cnf.int_to_var key with
            | Some s -> printf "%s " s
            | None -> ()
          )
        in
        let _ = printf "\n" in
        print_solution hd
    in
    let falses x =
      Int.Map.fold x ~init:0
        ~f:(fun ~key ~data acc ->
          if not data then acc + 1 else acc
        ) in
    let count, result = List.fold tl ~init:((falses hd), hd)
      ~f:(fun (count, bools) el ->
        let _ = if verbose then
            print_solution el
        in
        let count' = falses el in
        if count' > count then count', el
        else count, bools
      ) in
    Some result

let equal l l' =
  Option.is_some (solve Cnf.(l <=> l'))

let evaluate bools l =
  let lst = Int.Map.fold ~init:[]
              ~f:(fun ~key ~data acc ->
                let el = if data then key else Int.(~-1 * key) in
                el :: acc
              ) bools
  in
  Option.is_some (solve Cnf.(c_land (l :: lst)))

let is_not_false l = Option.is_some (solve l)
let is_false l = Option.is_none (solve l)
