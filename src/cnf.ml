
open Core.Std

module CSet = Set.Poly

module T = struct
  type t = Logic.Set.t CSet.t
  with sexp, compare
end
include T
include Comparable.Make(T)


let make_false : t = CSet.singleton (Logic.Set.singleton Logic.False)
let make_true : t= CSet.empty
let is_false t = CSet.mem t Logic.(Set.singleton False)
let is_true = CSet.is_empty

let rec to_string t =
  if is_false t then "false"
  else if is_true t then "true"
  else
    let lst = CSet.to_list t in
    let lst = List.map ~f:(fun x -> Logic.Set.to_list x) lst in
    String.concat ~sep:" ∧ "
    (List.map lst
        ~f:(fun x ->
        "(" ^
        String.concat ~sep:" ∨ " (List.map x ~f:(fun y -> Logic.to_string y))
        ^ ")"
        )
    )

let is_ground t =
  CSet.fold t ~init:true
    ~f:(fun acc v ->
      Logic.Set.fold v ~init:acc ~f:(fun acc el -> acc && Logic.is_ground el)
    )

let simplify t =
  CSet.fold t ~init:make_true
    ~f:(fun acc x ->
      let result =
        let open Logic in
        if Set.is_empty (Set.filter x ~f:(fun el -> Poly.(el <> True && el <> False))) then
          if Set.mem x True then make_true
          else make_false
        else
          let lst = Set.fold x ~init:Set.empty
            ~f:(fun acc el ->
              match el with
              | Not (Var _)
              | Var _ -> Set.add acc el
              | _ -> acc
            ) in
            CSet.singleton lst in
      if is_false acc || is_false result then make_false
      else if CSet.is_empty acc then result
      else if CSet.is_empty result then acc
      else CSet.union acc result
    )

let rec from_logic = function
  | Logic.And (p, q) ->
    let p, q = Logic.(simplify p, simplify q) in
    simplify (CSet.union (from_logic p) (from_logic q))
  | Logic.Or (p, q) ->
    let p, q = Logic.(simplify p, simplify q) in
    let cnf, cnf' = (from_logic p), (from_logic q) in
    let result =
      CSet.fold
        ~f:(fun acc x ->
          CSet.fold ~f:(fun acc x' ->
            CSet.add acc (Logic.Set.union x x')
          ) ~init:acc cnf'
        ) ~init:CSet.empty cnf in
    simplify result
  | Logic.Not (Logic.Not x) -> from_logic (Logic.simplify x)
  | Logic.Not (Logic.And (p, q)) ->
    let p, q = Logic.(simplify p, simplify q) in
    from_logic Logic.(~-p + ~-q)
  | Logic.Not (Logic.Or (p, q)) ->
    let p, q = Logic.(simplify p, simplify q) in
    simplify (CSet.union (from_logic Logic.(~-p)) (from_logic Logic.(~-q)))
  | Logic.Not Logic.True -> make_false
  | Logic.Not Logic.False -> make_true
  | x -> simplify (CSet.singleton (Logic.Set.singleton (Logic.simplify x)))

let evaluate bools t =
  let open Logic in
  CSet.fold t ~init:True
    ~f:(fun acc x ->
      let result = Logic.Set.fold x ~init:False
        ~f:(fun acc x ->
            match acc, Logic.evaluate bools x with
            | True, _
            | _, True -> True
            | _ -> False) in
      match acc, result with
      | False, _
      | _, False -> False
      | _ -> True)

let ( * ) t t' = simplify (CSet.union t t')
let (+) t t' =
  let result =
    CSet.fold t ~init:CSet.empty
      ~f:(fun acc set ->
        CSet.fold t' ~init:acc
          ~f:(fun acc set' ->
            CSet.add acc (Logic.Set.union set set')
        )
    ) in
  simplify result
let (~-) t =
  if is_false t then make_true
  else if is_true t then make_false
  else
    let result = CSet.fold t ~init:Logic.Set.empty
      ~f:(fun acc set ->
        Logic.(Set.union acc (Set.map set ~f:(~-)))
      ) in
    simplify (CSet.singleton result)
let (==>) t t' = simplify (~-t + t')
let (<==) t t' = simplify (t + ~-t')
let (<=>) t t' = simplify ((t ==> t') * (t' ==> t))

let list_of_disjuncts lst =
  match lst with
  | [] -> make_true
  | hd :: [] -> hd
  | hd :: tl -> List.fold ~f:(fun acc x -> x + acc) ~init:hd tl


