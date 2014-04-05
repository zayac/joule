open Core.Std

module T = struct
  type t = Term.t list * Term.t list with sexp, compare
end
include T
include Comparable.Make(T)

(** A constraint on variable *)
type var_bounds = Term.t Logic.Map.t

let hash = Hashtbl.hash

let default = [], []

let to_string (l, r) =
  let f x = String.concat ~sep:", " (List.map ~f:Term.to_string x) in
  String.concat ~sep:" <= " [f l; f r]

let get_vars (l, r) =
  let f = List.fold_left ~init:String.Set.empty
    ~f:(fun s x -> String.Set.union s (Term.get_vars x)) in
  f l, f r

let print_constraints map =
  let constr_to_string c =
    let l = Logic.Map.to_alist c in
    let sl = List.map l
      ~f:(fun (l, t) ->
        if Logic.(l <> Logic.True) then
          Printf.sprintf "[%s]%s" (Logic.to_string l) (Term.to_string t)
        else Printf.sprintf "%s" (Term.to_string t)) in
    String.concat ~sep:", " sl in
  let print_bound ~key ~data =
    let u = data in
    Printf.printf "$%s <= %s\n" key (constr_to_string u) in
  String.Map.iter ~f:print_bound map

exception No_Solution of string

let substitute constrs bools =
  let f x =
    let f' ~key ~data = function
      | None ->
        if Logic.(evaluate bools key = True) then
          Some (Term.to_wff bools data)
        else None
      | x -> x in
    let term = Logic.Map.fold ~init:None ~f:f' x in
    match term with
    | None -> raise (No_Solution "Solution doesn't exist")
    | Some x -> x in
  String.Map.map constrs ~f

