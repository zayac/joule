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

let to_string ?(rel=" ⊑ ") (l, r) =
  sprintf "%s%s%s" (Term.to_string l) rel (Term.to_string r)

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
  let f ~key ~data acc =
    (* store all terms that match a Boolean condition *)
    let candidates = ref Term.Set.empty in
    let f' ~key ~data =
      if Cnf.evaluate bools key then
        candidates := Term.Set.add !candidates (Term.to_wff bools data) in
    Cnf.Map.iter ~f:f' data;
    let result = Term.Set.fold !candidates ~init:None
      ~f:(fun acc el ->
        match acc with
        | None -> Some el
        | Some old_el ->
          Term.join old_el el
      ) in
    match result with
    | None ->
      Errors.unsat_error (sprintf "A solution for variable $%s does not exist" key)
    | Some term -> String.Map.add acc ~key ~data:term in
    String.Map.fold constrs ~init:String.Map.empty ~f

