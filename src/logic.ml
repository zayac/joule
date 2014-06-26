open Core.Std

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

let rec to_string ?(sand=" ∧ ") ?(sor=" ∨ ") ?(snot="¬") ?vprefix = function
  | False -> "false"
  | True -> "true"
  | Not t -> Printf.sprintf "%s%s" snot (to_string t)
  | And (t, t') -> Printf.sprintf "(%s%s%s)" (to_string t) sand (to_string t')
  | Or (t, t') -> Printf.sprintf "(%s%s%s)" (to_string t) sor (to_string t')
  | Var v -> (Option.value ~default:"" vprefix) ^ v

let rec is_ground = function
  | False | True -> true
  | Not t -> is_ground t
  | Or (t, t')
  | And (t, t') -> is_ground t && is_ground t'
  | Var _ -> false

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
      | False, t
      | t, False -> t
      | x, x' when x = x' -> x
      | x, x' -> Or (x, x')
    end
  | And (t, t') ->
    begin
      match simplify t, simplify t' with
      | False, _ -> False
      | _, False -> False
      | True, t
      | t, True -> t
      | x, x' when x = x' -> x
      | x, x' -> And (x, x')
    end
  | t -> t

let (+) t t' = simplify (Or (t, t'))
let ( * ) t t' = simplify (And (t, t'))
let (~-) t = simplify (Not t)
let (==>) t t' = simplify (~-t + t')
let (<==) t t' = simplify (t + ~-t')
let (<=>) t t' = simplify ((t ==> t') * (t' ==> t))

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
      | _ -> assert false
    end
  | Or (t, t') ->
    begin
      match evaluate bools t, evaluate bools t' with
      | True, _
      | _, True -> True
      | False, False -> False
      | _ -> assert false
    end
  | And (t, t') ->
    begin
      match evaluate bools t, evaluate bools t' with
      | True, True -> True
      | False, _ -> False
      | _, False -> False
      | _ -> assert false
    end
  | t -> t

let list_of_disjuncts lst =
  match lst with
  | [] -> True
  | hd :: [] -> hd
  | hd :: tl -> List.fold ~f:(fun acc x -> Or (x, acc)) ~init:hd tl
