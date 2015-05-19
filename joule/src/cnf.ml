
open Core.Std

module CSet = Set.Poly

module T = struct
  type t = Logic.Set.t CSet.t
  with sexp, compare
end
include T
include Comparable.Make(T)

let make_false = CSet.singleton (Logic.Set.singleton Logic.False)
let make_true = CSet.empty
let make_var s = CSet.singleton Logic.(Set.singleton (Var s))
let is_false t = CSet.mem t Logic.(Set.singleton False)
let is_true = CSet.is_empty

let to_string t =
  if is_false t then "false"
  else if is_true t then "true"
  else
    let lst = CSet.to_list t in
    let lst = List.map ~f:(fun x -> Logic.Set.to_list x) lst in
    let l = 
      List.map lst
        ~f:(fun x ->
          match x with
          | hd :: [] ->
            Logic.to_string hd
          | lst ->
            String.concat ["(or ";
                           String.concat ~sep:" " (List.map lst ~f:(fun y -> Logic.to_string y));
                          ")"
                          ]
        )
    in
    match l with
    | hd :: [] -> hd
    | lst -> sprintf "(and %s)" (String.concat ~sep:" " lst)


let to_friendly_string ?(sand=" ∧ ") ?(sor=" ∨ ") ?(snot="¬") ?vprefix t =
  if is_false t then "false"
  else if is_true t then "true"
  else
    let lst = CSet.to_list t in
    let lst = List.map ~f:(fun x -> Logic.Set.to_list x) lst in
    String.concat ~sep:sand
    (List.map lst
        ~f:(fun x ->
          String.concat ["(";
           String.concat ~sep:sor (List.map x
             ~f:(fun y ->
               match vprefix with
               | None -> Logic.to_friendly_string ~sand ~sor ~snot y
               | Some vp -> Logic.to_friendly_string ~sand ~sor ~vprefix:vp y
             ));
           ")"
          ]
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
          if Set.mem x True then Logic.true_set
          else Logic.false_set
        else
          let disj = Set.fold x ~init:Set.empty
            ~f:(fun acc el ->
              match el with
              | Not (Var v) ->
                begin
                  match Set.find acc ~f:(Logic.(=) (Var v)) with
                  | None -> Set.add acc el
                  | Some el' -> Set.add (Set.remove acc el') True
                end
              | Var v ->
                begin
                  match Set.find acc ~f:(Logic.(=) (Not (Var v))) with
                  | None -> Set.add acc el
                  | Some el' -> Set.add (Set.remove acc el') True
                end
              | _ -> acc
            ) in
          if Set.mem disj True then Logic.true_set else disj in
      let to_add = ref true in
      let acc = CSet.filter acc
        ~f:(fun x ->
          if !to_add && Logic.Set.subset x result then
            let _ = to_add := false in true
          else if !to_add && Logic.Set.subset result x then
            false
          else
            true
        ) in
      if !to_add then
        if Logic.Set.equal result Logic.true_set then CSet.union acc make_true
        else if Logic.Set.equal result Logic.false_set then CSet.union acc make_false
        else CSet.add acc result
      else acc
      (*if is_false acc || is_false result then make_false*)
      (*else if is_true acc then result*)
      (*else if is_true result then acc*)
      (*else CSet.union acc result*)
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
    let lst = ref [] in
    CSet.iter t
      ~f:(fun disj ->
        let tmp = ref !lst in
        lst := Logic.Set.fold disj ~init:[]
          ~f:(fun acc el ->
            if List.is_empty !tmp then [Logic.(~-el)] :: acc
            else (List.map !tmp ~f:(fun l -> Logic.(~-el) :: l)) @ acc
          );
      );
    let tmp = (List.map ~f:Logic.Set.of_list !lst) in
    let t' = CSet.of_list tmp in
    simplify t'
let ( * ) t t' =
  if t = ~-t' then make_false
  else simplify (CSet.union t t')
let (==>) t t' = simplify (~-t + t')
let (<==) t t' = simplify (t + ~-t')
let (<=>) t t' = simplify ((t ==> t') * (t' ==> t))

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

