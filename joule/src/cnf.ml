
open Core.Std

module T = struct
type t = int
  with sexp, compare
end
include T
include Comparable.Make(T)

type cnf_key =
  | Ktrue
  | Kfalse
  | Knot of t
  | Kor of t * t
  | Klor of t list
  | Kand of t * t
  | Kland of t list
  | Knand of t * t
  | Knor of t * t
  | Kxor of t * t
  | Kimp of t * t
  | Keq of t * t
  | Kvar of int
with sexp, compare

let m = ref 1
let new_var () =
  m := !m + 1;
  !m - 1
let num_vars () = !m - 1

let cnf_table = Hashtbl.Poly.create () ~size:100
let logic_table = Hashtbl.Poly.create () ~size:100
let cnf = Hashtbl.Poly.create () ~size:100
let string_int_var_table = Hashtbl.Poly.create () ~size:100
let int_string_var_table = Hashtbl.Poly.create () ~size:100

let var_id_to_string i =
  assert Poly.(i > 0);
  match Hashtbl.Poly.find int_string_var_table i with
  | None -> sprintf "AUX_%i" i
  | Some v -> v

let var_id_to_clause_string ?(snot="not") i =
  assert Poly.(i <> 0);
  if Poly.(i > 0) then var_id_to_string i
  else String.concat ["("; snot; " "; var_id_to_string Poly.(-i); ")"]

(*let from_list =*)
  (*List.fold ~init:Core.Std.Set.Poly.empty*)
    (*~f:(fun acc el ->*)
      (*Core.Std.Set.Poly.add acc (List.fold ~init:Set.empty ~f:(fun acc el -> Set.add acc el) el)*)
    (*)*)

let make_var s =
  match Hashtbl.Poly.find string_int_var_table s with
  | Some v -> v
  | None ->
    let x = new_var () in
    let key = Kvar x in
    let y, c = x, [[]] in
    Hashtbl.Poly.add_exn string_int_var_table ~key:s ~data:x;
    Hashtbl.Poly.add_exn int_string_var_table ~key:x ~data:s;
    Hashtbl.Poly.add_exn cnf_table ~key ~data:y;
    Hashtbl.Poly.add_exn cnf ~key:y ~data:c;
    y

let make_true =
  let key = Ktrue in
  match Hashtbl.Poly.find cnf_table key with
  | Some v -> v
  | None ->
    let x = new_var () in
    let y, c = x, [[x]] in
    Hashtbl.Poly.add_exn cnf_table ~key ~data:y;
    Hashtbl.Poly.add_exn cnf ~key:y ~data:c;
    y

let is_true t =
  let key = Ktrue in
  match Hashtbl.Poly.find cnf_table key with
  | None -> false
  | Some v -> Poly.(v = t)

let is_false t =
  let key = Kfalse in
  match Hashtbl.Poly.find cnf_table key with
  | None -> false
  | Some v -> Poly.(v = t)

let make_false =
  let key = Kfalse in
  match Hashtbl.Poly.find cnf_table key with
  | Some v -> v
  | None ->
    let x = new_var() in
    let y, c = x, [[-x]] in
    Hashtbl.Poly.add_exn cnf_table ~key ~data:y;
    Hashtbl.Poly.add_exn cnf ~key:y ~data:c;
    y

let is_ground t = Poly.(is_true t || is_false t)

let to_string t =
  if is_true t then "true"
  else if is_false t then "false"
  else
    var_id_to_clause_string t
    (*let lst = Hashtbl.Poly.find_exn cnf t in*)
    (*let l = List.map lst*)
              (*~f:(fun x ->*)
                (*if List.is_empty x then ""*)
                (*else if Poly.(List.length x = 1) then var_id_to_clause_string (List.hd_exn x)*)
                (*else*)
                  (*String.concat ["(or ";*)
                                 (*String.concat ~sep:" " (List.map ~f:var_id_to_clause_string x);*)
                                 (*")" ]*)
              (*)*)
    (*in*)
    (*let l = List.filter l ~f:(String.(<>) "") in*)
    (*let lst = (var_id_to_clause_string t) :: l in*)
    (*if Poly.(List.length lst = 1) then List.hd_exn lst*)
    (*else*)
      (*sprintf "(and %s)" (String.concat ~sep:" " lst)*)

let to_friendly_string ?(sand=" ∧ ") ?(sor=" ∨ ") ?(snot="¬") ?vprefix t =
  if is_true t then "true"
  else if is_false t then "false"
  else
    let lst = Hashtbl.Poly.find_exn cnf t in
    let l = List.map lst
              ~f:(fun x ->
                if List.is_empty x then ""
                else if Poly.(List.length x = 1) then var_id_to_clause_string ~snot (List.hd_exn x)
                else
                  String.concat ["(";
                                 String.concat ~sep:sor (List.map ~f:(fun el -> var_id_to_clause_string ~snot el) x);
                                 ")" ]
              )
    in
    let l = List.filter l ~f:(String.(<>) "") in
    let lst = (var_id_to_clause_string t) :: l in
    if Poly.(List.length lst = 1) then List.hd_exn lst
    else
      sprintf "(and %s)" (String.concat ~sep:" " lst)

let reset () =
  m := 1;
  Hashtbl.Poly.clear cnf_table;
  Hashtbl.Poly.clear logic_table;
  Hashtbl.Poly.clear cnf;
  Hashtbl.Poly.clear string_int_var_table;
  Hashtbl.Poly.clear int_string_var_table

(*let simplify t =*)
  (*CSet.fold t ~init:make_true*)
    (*~f:(fun acc x ->*)
      (*let result =*)
        (*let open Logic in*)
        (*if Set.is_empty (Set.filter x ~f:(fun el -> Poly.(el <> True && el <> False))) then*)
          (*if Set.mem x True then Logic.true_set*)
          (*else Logic.false_set*)
        (*else*)
          (*let disj = Set.fold x ~init:Set.empty*)
            (*~f:(fun acc el ->*)
              (*match el with*)
              (*| Not (Var v) ->*)
                (*begin*)
                  (*match Set.find acc ~f:(Logic.(=) (Var v)) with*)
                  (*| None -> Set.add acc el*)
                  (*| Some el' -> Set.add (Set.remove acc el') True*)
                (*end*)
              (*| Var v ->*)
                (*begin*)
                  (*match Set.find acc ~f:(Logic.(=) (Not (Var v))) with*)
                  (*| None -> Set.add acc el*)
                  (*| Some el' -> Set.add (Set.remove acc el') True*)
                (*end*)
              (*| _ -> acc*)
            (*) in*)
          (*if Set.mem disj True then Logic.true_set else disj in*)
      (*let to_add = ref true in*)
      (*let acc = CSet.filter acc*)
        (*~f:(fun x ->*)
          (*if !to_add && Logic.Set.subset x result then*)
            (*let _ = to_add := false in true*)
          (*else if !to_add && Logic.Set.subset result x then*)
            (*false*)
          (*else*)
            (*true*)
        (*) in*)
      (*if !to_add then*)
        (*if Logic.Set.equal result Logic.true_set then CSet.union acc make_true*)
        (*else if Logic.Set.equal result Logic.false_set then CSet.union acc make_false*)
        (*else CSet.add acc result*)
      (*else acc*)
      (*[>if is_false acc || is_false result then make_false<]*)
      (*[>else if is_true acc then result<]*)
      (*[>else if is_true result then acc<]*)
      (*[>else CSet.union acc result<]*)
    (*)*)

(*let rec from_logic = function*)
  (*| Logic.And (p, q) ->*)
    (*let p, q = Logic.(simplify p, simplify q) in*)
    (*simplify (CSet.union (from_logic p) (from_logic q))*)
  (*| Logic.Or (p, q) ->*)
    (*let p, q = Logic.(simplify p, simplify q) in*)
    (*let cnf, cnf' = (from_logic p), (from_logic q) in*)
    (*let result =*)
      (*CSet.fold*)
        (*~f:(fun acc x ->*)
          (*CSet.fold ~f:(fun acc x' ->*)
            (*CSet.add acc (Logic.Set.union x x')*)
          (*) ~init:acc cnf'*)
        (*) ~init:CSet.empty cnf in*)
    (*simplify result*)
  (*| Logic.Not (Logic.Not x) -> from_logic (Logic.simplify x)*)
  (*| Logic.Not (Logic.And (p, q)) ->*)
    (*let p, q = Logic.(simplify p, simplify q) in*)
    (*from_logic Logic.(~-p + ~-q)*)
  (*| Logic.Not (Logic.Or (p, q)) ->*)
    (*let p, q = Logic.(simplify p, simplify q) in*)
    (*simplify (CSet.union (from_logic Logic.(~-p)) (from_logic Logic.(~-q)))*)
  (*| Logic.Not Logic.True -> make_false*)
  (*| Logic.Not Logic.False -> make_true*)
(*| x -> simplify (CSet.singleton (Logic.Set.singleton (Logic.simplify x)))*)

let evaluate bools t =
  let lst = Hashtbl.Poly.find_exn cnf t in
  List.fold lst ~init:true
    ~f:(fun acc x ->
      let result =
        List.fold x ~init:false
          ~f:(fun acc i ->
            if Poly.(acc = true) then true
            else
              match Hashtbl.Poly.find int_string_var_table i with
              | None -> true (* convention: priority goes to True *)
              | Some s ->
                begin
                  match String.Map.find bools s with
                  | Some true
                  | None -> true (* convention: priority goes to True *)
                  | Some false -> false
                end
          )
      in
      match acc, result with
      | false, _
      | _, false -> false
      | _ -> true)

let c_or x1 x2 =
  if x1 = x2 then x1
  else if is_true x1 || is_true x2 then make_true
  else
    let key = Kor (x1, x2) in
    match Hashtbl.Poly.find cnf_table key with
    | Some v -> v
    | None ->
      let x = new_var() in
      let y, c = x, [[-x; x1; x2]; [x; -x1]; [x; -x2]] in
      Hashtbl.Poly.add_exn cnf_table ~key ~data:y;
      Hashtbl.Poly.add_exn cnf ~key:y ~data:c;
      y

let c_not x =
  if is_true x then make_false
  else if is_false x then make_true
  else
    let key = Knot x in
    match Hashtbl.Poly.find cnf_table key with
    | Some v -> v
    | None ->
      if is_false x then make_true
      else if is_true x then make_false
      else
        let x' = new_var () in
        let y, c = x', [[-x'; -x]; [x'; x]] in
        Hashtbl.Poly.add_exn cnf_table ~key ~data:y;
        Hashtbl.Poly.add_exn cnf ~key:y ~data:c;
        y

let c_and x1 x2 =
  if x1 = x2 then x1
  else if is_true x1 then x2
  else if is_true x2 then x1
  else if is_false x1 || is_false x2 then make_false
  else
    let key = Kand (x1, x2) in
    match Hashtbl.Poly.find cnf_table key with
    | Some v -> v
    | None ->
      let x = new_var () in
      let y, c = x, [[-x; x1]; [-x; x2]; [x; -x1; -x2]] in
      (*List.iter ~f:(fun el ->*)
        (*print_endline (Sexp.to_string (List.sexp_of_t Int.sexp_of_t el))*)
      (*) c;*)
      Hashtbl.Poly.add_exn cnf_table ~key ~data:y;
      Hashtbl.Poly.add_exn cnf ~key:y ~data:c;
      y

let c_land xl =
  if Poly.(List.length xl = 0) then make_true
  else if Poly.(List.length xl = 1) then List.hd_exn xl
  else
    let key = Kland xl in
    match Hashtbl.Poly.find cnf_table key with
    | Some v -> v
    | None ->
      let x = new_var () in
      let y = x in
      let c = [x :: List.map xl ~f:(fun f -> -f)] @ List.map xl ~f:(fun f -> [-x; f]) in
      Hashtbl.Poly.add_exn cnf_table ~key ~data:y;
      Hashtbl.Poly.add_exn cnf ~key:y ~data:c;
      y

let c_nand x1 x2 =
  if x1 = x2 then x1
  else
    let key = Knand (x1, x2) in
    match Hashtbl.Poly.find cnf_table key with
    | Some v -> v
    | None ->
      let x = new_var () in
      let y, c = x, [[x; x1]; [x; x2]; [-x; -x1; -x2]] in
      Hashtbl.Poly.add_exn cnf_table ~key ~data:y;
      Hashtbl.Poly.add_exn cnf ~key:y ~data:c;
      y

let c_lor xl =
  if Poly.(List.length xl = 0) then make_true
  else if Poly.(List.length xl = 1) then List.hd_exn xl
  else
    let key = Klor xl in
    match Hashtbl.Poly.find cnf_table key with
    | Some v -> v
    | None ->
      let x = new_var () in
      let y, c = x, [-x::xl] @ List.map xl ~f:(fun f -> [x; -f]) in
      Hashtbl.Poly.add_exn cnf_table ~key ~data:y;
      Hashtbl.Poly.add_exn cnf ~key:y ~data:c;
      y

let c_nor x1 x2 =
  if x1 = x2 then x1
  else
    let key = Knor (x1, x2) in
    match Hashtbl.Poly.find cnf_table key with
    | Some v -> v
    | None ->
      let x = new_var () in
      let y, c = x, [[x; x1; x2]; [-x; -x1]; [-x; -x2]] in
      Hashtbl.Poly.add_exn cnf_table ~key ~data:y;
      Hashtbl.Poly.add_exn cnf ~key:y ~data:c;
      y

let c_xor x1 x2 =
  if x1 = x2 then x1
  else
    let key = Kxor (x1, x2) in
    match Hashtbl.Poly.find cnf_table key with
    | Some v -> v
    | None ->
      let x = new_var () in
      let y = x in
      let c = [[-x; x1; x2]; [-x; -x1; -x2]; [x; x1; -x2]; [x; -x1; x2]] in
      Hashtbl.Poly.add_exn cnf_table ~key ~data:y;
      Hashtbl.Poly.add_exn cnf ~key:y ~data:c;
      y

let c_eq x1 x2 =
  if x1 = x2 then x1
  else
    let key = Keq (x1, x2) in
    match Hashtbl.Poly.find cnf_table key with
    | Some v -> v
    | None ->
      let x = new_var () in
      let y = x in
      let c = [[x; x1; x2]; [x; -x1; -x2]; [-x; x1; -x2]; [-x; -x1; x2]] in
      Hashtbl.Poly.add_exn cnf_table ~key ~data:y;
      Hashtbl.Poly.add_exn cnf ~key:y ~data:c;
      y

let (+) = c_or
let ( * ) = c_and
let (~-) = c_not
let (<=>) = c_eq
let (==>) t t' = (~- t + t')

let rec from_logic f =
  match Hashtbl.Poly.find logic_table f with
  | Some x -> x
  | None ->
    let open Logic in
    match f with
    | Var s ->
      let x = make_var s in
      ignore (Hashtbl.add logic_table ~key:f ~data:x);
      x
    | True ->
      let x = make_true in
      ignore (Hashtbl.add logic_table ~key:f ~data:x);
      x
    | False ->
      let x = make_false in
      ignore (Hashtbl.add logic_table ~key:f ~data:x);
      x
    | And (f1, f2) ->
      let x1 = from_logic f1 in
      let x2 = from_logic f2 in
      let x = c_and x1 x2 in
      ignore (Hashtbl.add logic_table ~key:f ~data:x);
      x
    | Or (f1, f2) ->
      let x1 = from_logic f1 in
      let x2 = from_logic f2 in
      let x = c_or x1 x2 in
      ignore (Hashtbl.add logic_table ~key:f ~data:x);
      x
    | Not f ->
      let x = from_logic f in
      let x = c_not x in
      ignore (Hashtbl.add logic_table ~key:f ~data:x);
      x

let list_of_disjuncts lst =
  match lst with
  | [] -> make_true
  | hd :: [] -> hd
  | hd :: tl -> List.fold ~f:(fun acc x -> x + acc) ~init:hd tl

let pairwise_not_and l =
  if List.is_empty l then make_true
  else
    let generate_pairs l =
      let rec apply acc el = function
      | [] -> acc
      | hd :: tl -> apply ((el, hd) :: acc) el tl in
      let rec iter_left acc = function
      | [] -> acc
      | hd :: tl -> iter_left (apply acc hd tl) tl in
      iter_left [] l in
    List.fold ~init:make_true
      ~f:(fun acc (x, y) -> acc * (~-(x * y)))
      (generate_pairs l)

let set_to_t set = c_land (Core.Std.Set.Poly.to_list set)

let get_cnf () =
  Hashtbl.Poly.fold ~init:[]
    ~f:(fun ~key ~data acc -> List.append acc data)
    cnf

let int_to_var = Hashtbl.Poly.find int_string_var_table

