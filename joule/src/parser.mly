%{

module PLog = Log.Make(struct let section = "parser:" end)
module LLog = Log.Make(struct let section = "logic:" end)

let bool_constrs = ref Cnf.CSet.empty

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
%token <string> UP_VAR DOWN_VAR ID STRING
%token NONE
%token NIL TRUE FALSE NOT OR AND
%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET LANGULAR RANGULAR LSMILE RSMILE
%token SCOLON COLON COMMA BAR LEQ EQ NOMINAL EOF

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
      Errors.parse_error "invalid term or constraint" $startpos $endpos
    }

term:
  | NIL { Term.Nil }
  | NOMINAL INT { Term.NominalInt $2 }
  | INT { Term.OrdinalInt $1 }
  | ID { Term.Symbol $1 }
  | STRING { Term.Symbol $1 }
  | UP_VAR
    {
      let module T = Transform in
      T.initial_term_variables := Core.Std.String.Set.add !T.initial_term_variables $1;
      Term.UpVar $1
    }
  | DOWN_VAR
    {
      let module T = Transform in
      T.initial_term_variables := Core.Std.String.Set.add !T.initial_term_variables $1;
      Term.DownVar $1
    }
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
      Util.switch_of_alist_exn $2 bool_constrs
    }
  | LSMILE separated_nonempty_list(COMMA, rec_entry) rec_list_tail_choice? RSMILE
    {
      Term.Choice (Util.map_of_alist_exn $2 bool_constrs, $3)
    }
  | LBRACE separated_nonempty_list(COMMA, rec_entry) rec_list_tail? RBRACE
    {
      Term.Record (Util.map_of_alist_exn $2 bool_constrs, $3)
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
  | ID { Core.Std.String.concat ["\""; $1; "\""] }
  | STRING { $1 }

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
  | ID
    {
      let module T = Transform in
      T.initial_bool_variables := Core.Std.String.Set.add !T.initial_bool_variables $1;
      Logic.Var $1
    }
  | LPAREN OR logical_term logical_term+ RPAREN
    {
      Core.Std.List.fold $4 ~init:$3 ~f:(fun a b -> Logic.(a + b))
    }
  | LPAREN AND logical_term logical_term+ RPAREN
    {
      Core.Std.List.fold $4 ~init:$3 ~f:(fun a b -> Logic.(a * b))
    }
  | LPAREN NOT logical_term RPAREN { Logic.(~-$3) }
  | UP_VAR
  | DOWN_VAR
    {
      Errors.parse_error "invalid Boolean expression" $startpos $endpos
    }

rec_list_tail_choice:
  | BAR UP_VAR
    {
      let module T = Transform in
      T.initial_term_variables := Core.Std.String.Set.add !T.initial_term_variables $2;
      $2
    }

rec_list_tail:
  | BAR DOWN_VAR
    {
      let module T = Transform in
      T.initial_term_variables := Core.Std.String.Set.add !T.initial_term_variables $2;
      $2
    }
