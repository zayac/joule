open Core.Std

module T = struct
  type t = Term.t * Term.t with sexp, compare
end
include T
include Comparable.Make(T)

(** A constraint on variable *)
type var_bounds = Term.t Cnf.Map.t

let hash = Hashtbl.hash

let default = Term.(Nil, Nil)

let to_string (l, r) =
  sprintf "%s <= %s" (Term.to_string l) (Term.to_string r)

let get_vars (l, r) = Term.get_vars l, Term.get_vars r

let print_constraints map =
  let constr_to_string c =
    let l = Cnf.Map.to_alist c in
    let sl = List.map l
      ~f:(fun (l, t) ->
        Printf.sprintf "[%s]%s" (Cnf.to_string l) (Term.to_string t)) in
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
        if Logic.(Cnf.evaluate bools key = True) then
          Some (Term.to_wff bools data)
        else None
      | x -> x in
    let term = Cnf.Map.fold ~init:None ~f:f' x in
    match term with
    | None -> raise (No_Solution "Solution doesn't exist")
    | Some x -> x in
  String.Map.map constrs ~f

