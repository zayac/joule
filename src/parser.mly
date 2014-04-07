%{

module Log = Log.Make(struct let section = "parser:" end)

let bool_constrs = ref Logic.Set.empty

let log_bool set =
  if not (Logic.Set.is_empty set) then
    Log.logf "boolean constraints {%s} have been added"
      (Logic.set_to_string set)

let pairwise_not_and l =
  let open Core.Std in
  let open Logic in
  let generate_pairs l =
    let rec apply acc el = function
    | [] -> acc
    | hd :: tl -> apply ((el, hd) :: acc) el tl in
    let rec iter_left acc = function
    | [] -> acc
    | hd :: tl -> iter_left (apply acc hd tl) tl in
    iter_left [] l in
  List.map ~f:(fun (x, y) -> ~-(x * y)) (generate_pairs l)

let switch_of_alist_exn (startp, endp) l =
  let open Core.Std in
  match Logic.Map.of_alist l with
  | `Duplicate_key expr ->
    Errors.parse_error
      ("two or more terms are associated with logical expression " ^ 
        (Logic.to_string expr))
      startp
      endp
  | `Ok map ->
    let map, bool_constrs' = Term.canonize_switch map in
    let keys = Logic.Map.keys map in
    (* add freshly generated constraints from canonization function *)
    log_bool bool_constrs';
    bool_constrs := Logic.Set.union !bool_constrs bool_constrs';
    (* add constraints asserting pairwise logical expressions exclusion *)
    let pairwise_exclusion = Logic.Set.of_list (pairwise_not_and keys) in
    log_bool pairwise_exclusion;
    bool_constrs := Logic.Set.union !bool_constrs pairwise_exclusion;
    (* add constraints asserting that at least one logical expression must be
       satisfiable *)
    let singleton = Logic.Set.singleton (Logic.list_of_disjuncts keys) in
    log_bool singleton;
    bool_constrs := Logic.Set.union !bool_constrs singleton;
    if Logic.Map.is_empty map then
      Errors.parse_error
        "a switch term does not contain any satisfiable logic expression"
        startp endp;
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
      logic, switch_of_alist_exn (startp, endp) lst
    else
      g, v
  | (g, v) :: tl -> f (g :: gacc) (v :: vacc) tl in
  String.Map.map ~f:(f [] []) multi_map
%}

%token <int> INT
%token <string> VAR
%token <string> ID
%token NIL NOT OR AND
%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET LANGULAR RANGULAR
  LSMILE RSMILE
%token SCOLON COLON COMMA BAR LEQ EQ EOF

%start <Constr.t list * Logic.Set.t> parse
%%

parse:
  | constrs* EOF { $1, !bool_constrs }

constrs:
  | term+ LEQ term+ SCOLON { $1, $3 }
  | term+ EQ term+ SCOLON { ($3 @ $1), ($3 @ $1) }
  | error
    {
      Errors.parse_error "Invalid constraint" $startpos $endpos
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
  | LPAREN NOT NIL RPAREN { Logic.False }
  | LPAREN OR logical_term logical_term RPAREN { Logic.Or ($3, $4) }
  | LPAREN AND logical_term logical_term RPAREN { Logic.And ($3, $4) }

logical_term:
  | NIL { Logic.False }
  | VAR { Logic.Var $1 }
  | LPAREN OR logical_term logical_term RPAREN { Logic.Or ($3, $4) }
  | LPAREN AND logical_term logical_term RPAREN { Logic.And ($3, $4) }
  | LPAREN NOT logical_term RPAREN { Logic.Not $3 }

rec_list_tail:
  | BAR VAR { $2 }
