open Core.Std

type solution =
  | Any
  | Solutions of bool String.Map.t list

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

(* associate variables with successive integer terms and return two associative
   lists *)
(*
let index_variables set =
  let module Perv = Pervasives in
  let module IM = Int.Map in
  let module SM = String.Map in
  let rec index ((imap, smap, counter) as acc) x =
    let open Logic in
    match x with
      | Var v ->
        if SM.mem smap v then acc
        else
          (
            IM.add ~key:counter ~data:v imap,
            SM.add ~key:v ~data:counter smap,
            Perv.(counter + 1)
          )
      | Not t -> index acc t
      | Or (t, t')
      | And (t, t') -> index (index acc t) t'
      | _ -> acc in
  let imap, smap, _ =
    Set.Poly.fold set ~init:(IM.empty, SM.empty, 1)
      ~f:(fun acc x ->
        Set.fold ~f:index ~init:acc x
      ) in
  imap, smap
*)

(* add constraints provided in the CNF form to PicoSat *)
let cnf_to_psat cnf =
  let module P = Picosat in
  let psat = ref (P.init ()) in
  let cnf = [cnf] :: (Cnf.get_cnf ()) in
  List.iter
    ~f:(fun lst ->
      if not (List.is_empty lst) then
      begin
        List.iter ~f:(fun el -> ignore (P.add !psat el)) lst;
        (*List.iter ~f:(fun el -> ignore (P.add !psat el); printf "%d " el) lst;*)
        (*printf "\n";*)
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
    let module IM = Int.Map in
    let module SM = String.Map in
    while !loop do
      if Poly.(Picosat.sat !psat Int.(~-1) = Picosat.Satisfiable) then
        match get_solution psat with
        | Some x ->
          (*print_endline (Sexp.to_string (List.sexp_of_t Int.sexp_of_t x));*)
          let assignment = ref SM.empty in
          let counter = ref 1 in
          List.iter
            ~f:(fun v ->
              ignore (Picosat.add !psat Int.(~-1 * !counter * v));
              (*printf "%d " Int.(~-1 * !counter * v);*)
              (* add a variable value to the assignment *)
              let _ =
                match Cnf.int_to_var !counter with
                | None -> ()
                | Some s ->
                  assignment := SM.add ~key:s ~data:Int.(v > 0) !assignment
              in
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
  | Any -> Some String.Map.empty
  | Solutions [] -> None
  | Solutions (hd :: tl) -> Some hd

let solve_max ?(verbose=false) cnf =
  match find_models cnf false with
  | Any -> Some String.Map.empty
  | Solutions [] -> None
  | Solutions (hd :: tl) ->
    let counter = ref 1 in
    let print_solution map =
      printf "%d | " !counter;
      String.Map.iter map
        ~f:(fun ~key ~data ->
          printf "%s " (if data then "true" else "false");
        );
      printf "\n";
      counter := !counter + 1
    in
    let _ =
      if verbose then
        let _ = printf "\nList of SAT solutions:\n" in
        let _ = String.Map.iter hd
          ~f:(fun ~key ~data ->
            printf "%s " key
             )
        in
        let _ = printf "\n" in
        print_solution hd
    in
    let falses x =
      String.Map.fold x ~init:0
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
