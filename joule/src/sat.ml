
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
    Cnf.CSet.fold set ~init:(IM.empty, SM.empty, 1)
      ~f:(fun acc x ->
        Set.fold ~f:index ~init:acc x
      ) in
  imap, smap

(* add constraints provided in the CNF form to PicoSat *)
let cnf_to_psat cnf smap =
  let module P = Picosat in
  let psat = ref (P.init ()) in
  Cnf.CSet.iter
    ~f:(fun lst ->
      let open Logic in
      Set.iter
        ~f:(function
          | Var v -> ignore (P.add !psat (String.Map.find_exn smap v))
          | Not (Var v) ->
            ignore (P.add !psat Int.(~-1 * (String.Map.find_exn smap v)))
          | v -> assert false
        ) lst;
      ignore (P.add !psat 0)
    ) cnf;
  psat

let find_models set single_model =
  let imap, smap = index_variables set in
  let set = Cnf.simplify set in
  if Cnf.is_false set then Solutions []
  else if Cnf.CSet.is_empty set then Any
  else
    let psat = cnf_to_psat set smap in
    let result = ref [] in
    let loop = ref true in
    let module IM = Int.Map in
    let module SM = String.Map in
    while !loop do
      if Poly.(Picosat.sat !psat Int.(~-1) = Picosat.Satisfiable) then
        match get_solution psat with
        | Some x ->
          let assignment = ref SM.empty in
          let counter = ref 1 in
          List.iter
            ~f:(fun v ->
              ignore (Picosat.add !psat Int.(~-1 * !counter * v));
              (* add a variable value to the assignment *)
              assignment :=
                SM.add
                ~key:(IM.find_exn imap !counter)
                ~data:(if Int.(v > 0) then true else false)
                !assignment;
              counter := Int.(!counter + 1)
            ) x;
          ignore (Picosat.add !psat 0);
          result :=  !assignment :: !result;
          if single_model then loop := false
        | None -> loop := false
      else
        loop := false
    done;
    Solutions !result

let solve set =
  match find_models set true with
  | Any -> Some String.Map.empty
  | Solutions [] -> None
  | Solutions (hd :: tl) -> Some hd

let solve_max set =
  match find_models set false with
  | Any -> Some String.Map.empty
  | Solutions [] -> None
  | Solutions (hd :: tl) ->
    let falses x =
      String.Map.fold x ~init:0
        ~f:(fun ~key ~data acc ->
          if not data then acc + 1 else acc
        ) in
    let count, result = List.fold tl ~init:((falses hd), hd)
      ~f:(fun (count, bools) el ->
        let count' = falses el in
        if count' > count then count', el
        else count, bools
      ) in
    Some result

let equal l l' =
  match solve Cnf.(~-(l <=> l')) with
  | None -> true
  | _ -> false
