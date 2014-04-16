%{

module PLog = Log.Make(struct let section = "parser:" end)
module LLog = Log.Make(struct let section = "logic:" end)

let bool_constrs = ref Cnf.CSet.empty

let log_bool set =
  if not (Cnf.CSet.is_empty set) then
    LLog.logf "adding boolean constraints {%s}" (Cnf.to_string set)

let switch_of_alist_exn (startp, endp) l =
  let open Core.Std in
  let l = List.map l ~f:(fun (el, t) -> Cnf.from_logic el, t) in
  let multi_map = Cnf.Map.of_alist_multi l in
  let map = Cnf.Map.fold multi_map ~init:Cnf.Map.empty
    ~f:(fun ~key ~data acc ->
      match data with
      | hd :: [] -> Cnf.Map.add acc ~key ~data:hd
      | hd :: tl ->
        let _ = bool_constrs := Cnf.(CSet.union !bool_constrs ~-key) in
        acc
      | [] -> failwith "invalid argument"
    ) in
  let keys = Cnf.Map.keys map in
  let _ = if List.length keys > 1 then
    let l = Cnf.pairwise_not_and keys in
    let _ = log_bool l in
    bool_constrs := Cnf.CSet.union !bool_constrs l in
  (* add constraints asserting that at least one logical expression must be
     satisfiable *)
  let singleton = Cnf.list_of_disjuncts keys in
  let _ = log_bool singleton in
  let _ = bool_constrs := Cnf.CSet.union !bool_constrs singleton in
  Term.Switch map

(* convert a label-term key-value pairs into a correct map structure with
   label conflict resolution. New boolean constraints are added to
   [bool_constrs]. *)
let map_of_alist_exn (startp, endp) l =
  let open Core.Std in
  let multi_map = String.Map.of_alist_multi l in
  let rec f (gacc: Logic.t list) (vacc: Term.t list) = function
  (* unreachable state due to the explicit check for empty records/choices in
     the parser below *)
  | [] -> failwith "a list must contain at least one element"
  | (g, v) :: [] ->
    if not (List.is_empty gacc) then
      let gacc, vacc = g :: gacc, v :: vacc in
      let logic = Logic.list_of_disjuncts gacc in
      let lst =  List.map2_exn ~f:(fun g t -> g, t) gacc vacc in
      (Cnf.from_logic logic), switch_of_alist_exn (startp, endp) lst
    else
      (Cnf.from_logic g), v
  | (g, v) :: tl -> f (g :: gacc) (v :: vacc) tl in
  String.Map.map ~f:(f [] []) multi_map

let convert_constrs l r =
  let open Core.Std in
  List.fold l ~init:[]
    ~f:(fun acc x ->
      List.fold r ~init:acc
        ~f:(fun acc y ->
          (x, y) :: acc
        )
    )
%}

%token <int> INT
%token <string> VAR
%token <string> ID
%token NIL TRUE FALSE NOT OR AND
%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET LANGULAR RANGULAR
  LSMILE RSMILE
%token SCOLON COLON COMMA BAR LEQ EQ EOF

%start <Constr.t list * Cnf.t> parse
%%

parse:
  | constrs* EOF
  {
    let open Core.Std in
    List.concat_no_order $1, !bool_constrs 
  }

constrs:
  | term+ LEQ term+ SCOLON { convert_constrs $1 $3 }
  | term+ EQ term+ SCOLON { convert_constrs $1 $3 @ convert_constrs $3 $1 }
  | error
    {
      Errors.parse_error "Invalid term or constraint" $startpos $endpos
    }

term:
  | NIL { Term.Nil }
  | INT { Term.Int $1 }
  | ID { Term.Symbol $1 }
  | VAR { Term.Var $1 }
  | LPAREN term+ RPAREN
    {
      let open Core.Std in
      (* tuple of one element equals to the element itself *)
      if Poly.(List.length $2 = 1) then List.hd_exn $2 else Term.Tuple $2
    }
  | LSMILE RSMILE
    {
      let open Core.Std in
      Term.Choice (String.Map.empty, None)
    }
  | LBRACE RBRACE { Term.Nil }
  | LBRACKET RBRACKET { Term.Nil }
  | LANGULAR separated_nonempty_list(COMMA, switch_entry) RANGULAR
    {
      switch_of_alist_exn ($startpos, $endpos) $2
    }
  | LSMILE separated_nonempty_list(COMMA, rec_entry) rec_list_tail? RSMILE
    {
      Term.Choice (map_of_alist_exn ($startpos, $endpos) $2, $3)
    }
  | LBRACE separated_nonempty_list(COMMA, rec_entry) rec_list_tail? RBRACE
    {
      Term.Record (map_of_alist_exn ($startpos, $endpos) $2, $3)
    }
  | LBRACKET separated_nonempty_list(COMMA, term) rec_list_tail? RBRACKET
    { Term.List ($2, $3) }

switch_entry:
  | logical_term COLON term
    {
      $1, $3
    }

rec_entry:
  | ID guard? COLON term
    {
      let t = match $2 with
      | None -> Logic.True
      | Some x -> x in
      $1, (t, $4)
    }
  | error
    {
      Errors.parse_error "Invalid record entry" $startpos $endpos
    }

guard:
  | LPAREN logical_term RPAREN { $2 }
  | LPAREN NOT logical_term RPAREN { Logic.(~-$3) }
  | LPAREN OR logical_term logical_term RPAREN { Logic.($3 + $4) }
  | LPAREN AND logical_term logical_term RPAREN { Logic.($3 * $4) }

logical_term:
  | TRUE { Logic.True }
  | FALSE { Logic.False }
  | VAR { Logic.Var $1 }
  | LPAREN OR logical_term logical_term RPAREN { Logic.($3 + $4) }
  | LPAREN AND logical_term logical_term RPAREN { Logic.($3 * $4) }
  | LPAREN NOT logical_term RPAREN { Logic.(~-$3) }

rec_list_tail:
  | BAR VAR { $2 }
