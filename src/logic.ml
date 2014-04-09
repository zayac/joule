open Core.Std

module Log = Log.Make(struct let section = "logic:" end)

module T = struct
  type t =
    | False
    | True
    | Not of t
    | Or of t * t
    | And of t * t
    | Var of string
  with sexp, compare
end
include T
include Comparable.Make(T)

type cnf =
  | Valid
  | Unsatisfiable
  | Satisfiable of t list list

type solution =
  | Any
  | Solutions of bool String.Map.t list

let rec to_string = function
  | False -> "false"
  | True -> "true"
  | Not t -> Printf.sprintf "¬%s" (to_string t)
  | And (t, t') -> Printf.sprintf "(%s ∧ %s)" (to_string t) (to_string t')
  | Or (t, t') -> Printf.sprintf "(%s ∨ %s)" (to_string t) (to_string t')
  | Var v -> v

let set_to_string s =
  let sl = Set.to_list s in
  let sstring = List.map ~f:to_string sl in
  String.concat ~sep:", " sstring
  
let rec is_ground = function
  | False | True -> true
  | Not t -> is_ground t
  | Or (t, t')
  | And (t, t') -> is_ground t && is_ground t'
  | Var _ -> false

let list_of_disjuncts lst =
  match lst with
  | [] -> True
  | hd :: [] -> hd
  | hd :: tl -> List.fold ~f:(fun acc x -> Or (x, acc)) ~init:hd tl

let rec simplify = function
  | Not t ->
    begin
      match simplify t with
      | True -> False
      | False -> True
      | Not x -> x
      | x -> Not x
    end
  | Or (t, t') ->
    begin
      match simplify t, simplify t' with
      | True, _ -> True
      | _, True -> True
      | False, False -> False
      | x, x' when x = x' -> x
      | x, x' -> Or (x, x')
    end
  | And (t, t') ->
    begin
      match simplify t, simplify t' with
      | False, _ -> False
      | _, False -> False
      | True, True -> True
      | x, x' when x = x' -> x
      | x, x' -> And (x, x')
    end
  | t -> t

let (+) t t' = simplify (Or (t, t'))
let ( * ) t t' = simplify (And (t, t'))
let (~-) t = simplify (Not t)
let (==>) t t' = simplify (~-t + t')
let (<==) t t' = simplify (t + -t')
let (<=>) t t' = simplify ((t ==> t') * (t' ==> t))


let to_cnf t =
  (* explicit formula simplification *)
  let t = simplify t in
  let rec to_cnf' = function
  | And (p, q) -> (to_cnf' p) @ (to_cnf' q)
  | Or (p, q) ->
    let cnf, cnf' = to_cnf' p, to_cnf' q in
    let module L = List in
    (* cartesian product of left and right terms *)
    L.fold
      ~f:(fun acc x ->
        L.fold ~f:(fun acc x' ->
          (x @ x') :: acc
        ) ~init:acc cnf'
      ) ~init:[] cnf
  | Not (Not x) -> to_cnf' x
  | Not (And (p, q)) -> to_cnf' (~-p + ~-q)
  | Not (Or (p, q)) -> (to_cnf' ~-p) @ (to_cnf' ~-q)
  | Not True -> [[False]]
  | Not False -> [[True]]
  | x -> [[x]] in
  let cnf = to_cnf' t in
  (* convert to cnf type *)
  List.fold cnf ~init:Valid
    ~f:(fun acc x ->
      let result =
        if List.is_empty (List.filter x ~f:(fun el -> Poly.(el <> True && el <> False))) then
          if List.mem x True then Valid
          else Unsatisfiable
        else
          let lst = List.fold x ~init:[]
            ~f:(fun acc el ->
              match el with
              | Not (Var _)
              | Var _ -> el :: acc
              | _ -> acc
            ) in
          Satisfiable [lst] in
      match acc, result with
      | _, Unsatisfiable
      | Unsatisfiable, _ -> Unsatisfiable
      | Valid, cnf
      | cnf, Valid -> cnf
      | Satisfiable x, Satisfiable x' -> Satisfiable (x @ x')
    )

(* add constraints provided in the CNF form to PicoSat *)
let cnf_to_psat cnf smap =
  let module L = List in
  let module P = Picosat in
  let psat = ref (P.init ()) in
  L.iter
    ~f:(fun lst ->
      L.iter
        ~f:(function
          | Var v -> ignore (P.add !psat (String.Map.find_exn smap v))
          | Not (Var v) ->
            ignore (P.add !psat Int.(~-1 * (String.Map.find_exn smap v)))
          | v -> failwith
            ("wrong logical expression: " ^ (to_string v))
        ) lst;
      ignore (P.add !psat 0)
    ) cnf;
  psat

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

let set_to_cnf lst =
  let logic = Set.fold ~f:(fun acc x -> acc * x) ~init:True lst in
  to_cnf logic

(* associate variables with successive integer terms and return two associative
   lists *)
let index_variables set =
  let module Perv = Pervasives in
  let module IM = Int.Map in
  let module SM = String.Map in
  let rec index ((imap, smap, counter) as acc) x =
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
  let imap, smap, _ = Set.fold ~f:index ~init:(IM.empty, SM.empty, 1) set in
  imap, smap

let find_models t_set single_model =
  let imap, smap = index_variables t_set in
  let cnf = set_to_cnf t_set in
  (* if cnf contains False clause, then a model doesn't exist *)
  match cnf with
  | Unsatisfiable -> Solutions []
  | Valid -> Any
  | Satisfiable cnf ->
    let psat = cnf_to_psat cnf smap in
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
            List.iter ~f:(fun v ->
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

let all_solutions t_set =
  match find_models t_set false with
  | Any -> Some [String.Map.empty]
  | Solutions [] -> None
  | Solutions el -> Some el

let solve t_set =
  match find_models t_set true with
  | Any -> Some String.Map.empty
  | Solutions [] -> None
  | Solutions (hd :: _) -> Some hd

let rec evaluate bools = function
  | Var t ->
    begin
      match String.Map.find bools t with
      | Some true -> True
      | Some false
      | None -> False
    end
  | Not t ->
    begin
      match evaluate bools t with
      | False -> True
      | True -> False
    end
  | Or (t, t') ->
    begin
      match evaluate bools t, evaluate bools t' with
      | True, _
      | _, True -> True
      | False, False -> False
    end
  | And (t, t') ->
    begin
      match evaluate bools t, evaluate bools t' with
      | True, True -> True
      | False, _ -> False
      | _, False -> False
    end
  | t -> t

