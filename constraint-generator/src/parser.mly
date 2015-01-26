%{
  let additional_constraints = ref []
  let class_hash = ref Core.Std.String.Map.empty

  let index_class lt rt =
    let open Core.Std in
    let open Term in
    (* If left term is a variable, and the right one is a structure, we assume
       that the constraint is the definition of an interface object. *)
    match lt, rt with
    | Var v, Record (r, None) ->
      class_hash := String.Map.add !class_hash ~key:v ~data:r
    | _ -> ()
%}

%token <int> INT
%token <string> VAR ID STRING
%token NONE
%token NIL TRUE FALSE NOT OR AND
%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET LANGULAR RANGULAR LSMILE RSMILE LAUX RAUX
%token COLON COMMA BAR NOMINAL EOF
%token SCOLON LEQ EQ

%start <(Term.t * Term.t) *
        (Term.t * Term.t) list *
        ((Cnf.t * Term.t) Core.Std.String.Map.t) Core.Std.String.Map.t> term_parse
%start <(string * string) list> netlist_parse
%%

netlist_parse:
  | channel+ EOF { $1 }

channel:
  | ID ID { $1, $2 }
  | error
    { Errors.parse_error "invalid connection" $startpos $endpos }


term_parse:
  | constrs? term term EOF
    {
      let terms = $2, $3 in
      terms, !additional_constraints, !class_hash
    }

constrs:
  | LAUX constr* RAUX {}

constr:
  | term LEQ term SCOLON
    {
      additional_constraints := !additional_constraints @ [($1, $3)];
      index_class $1 $3
    }
  | term EQ term SCOLON
    {
      additional_constraints := !additional_constraints @ [($1, $3); ($3, $1)]
    }

term:
  | NIL { Term.Nil }
  | NOMINAL INT { Term.NominalInt $2 }
  | INT { Term.OrdinalInt $1 }
  | STRING { Term.Symbol $1 }
  | VAR
    {
          Term.Var $1
    }
  | ID { Term.Symbol (Core.Std.String.concat ["\""; $1; "\""]) }
  | LPAREN term+ RPAREN
    {
      let open Core.Std in
      (* tuple of one element equals to the element itself *)
      if Poly.(List.length $2 = 1) then List.hd_exn $2 else Term.Tuple $2
    }
  | NONE
  | LSMILE RSMILE
    {
      let open Core.Std in
      Term.Choice (String.Map.empty, None)
    }
  | LBRACE RBRACE { Term.Nil }
  | LBRACKET RBRACKET { Term.Nil }
  | LANGULAR separated_nonempty_list(COMMA, switch_entry) RANGULAR
    {
      Util.switch_of_alist_exn $2
    }
  | LSMILE separated_nonempty_list(COMMA, rec_entry) rec_list_tail? RSMILE
    {
      Term.Choice (Util.map_of_alist_exn $2, $3)
    }
  | LBRACE separated_nonempty_list(COMMA, rec_entry) rec_list_tail? RBRACE
    {
      Term.Record (Util.map_of_alist_exn $2, $3)
    }
  | LBRACKET separated_nonempty_list(COMMA, term) rec_list_tail? RBRACKET
    { Term.List ($2, $3) }
  

switch_entry:
  | logical_term COLON term
    {
      $1, $3
    }

rec_entry:
  | label guard? COLON term
    {
      let t = match $2 with
      | None -> Logic.True
      | Some x -> x in
      $1, (t, $4)
    }
  | error
    {
      Errors.parse_error "invalid record entry" $startpos $endpos
    }

label:
  | ID
    {
      Core.Std.String.concat ["\""; $1; "\""]
    }
  | STRING
    {
      if String.length $1 <= 2 then
        Errors.parse_error "Empty string as a label is not allowed" $startpos $endpos
      else
        $1
    }

guard:
  | LPAREN logical_term RPAREN { $2 }
  | LPAREN NOT logical_term RPAREN { Logic.(~-$3) }
  | LPAREN OR logical_term logical_term+ RPAREN
    {
      Core.Std.List.fold $4 ~init:$3 ~f:(fun a b -> Logic.(a + b))
    }
  | LPAREN AND logical_term logical_term+ RPAREN
    {
      Core.Std.List.fold $4 ~init:$3 ~f:(fun a b -> Logic.(a * b))
    }

logical_term:
  | TRUE { Logic.True }
  | FALSE { Logic.False }
  | ID { Logic.Var $1 }
  | LPAREN OR logical_term logical_term+ RPAREN
    {
      Core.Std.List.fold $4 ~init:$3 ~f:(fun a b -> Logic.(a + b))
    }
  | LPAREN AND logical_term logical_term+ RPAREN
    {
      Core.Std.List.fold $4 ~init:$3 ~f:(fun a b -> Logic.(a * b))
    }
  | LPAREN NOT logical_term RPAREN { Logic.(~-$3) }
  | VAR
    {
      Errors.parse_error "invalid Boolean expression" $startpos $endpos
    }
  | error { Errors.parse_error "wrong format of a flag" $startpos $endpos }

rec_list_tail:
  | BAR VAR { $2 }

