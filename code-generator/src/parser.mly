%{
  let term_hash = ref Core.Std.String.Map.empty
  let bool_hash = ref Core.Std.String.Map.empty
%}

%token <int> INT
%token <string> VAR ID STRING
%token NONE
%token NIL TRUE FALSE
%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET LSMILE RSMILE EQ
%token COLON COMMA NOMINAL EOF
(*%token SCOLON LEQ EQ *)

%start <Term.t Core.Std.String.Map.t * bool Core.Std.String.Map.t> values_parse
%start <(string list * string) Core.Std.String.Map.t> code_hash_parse
%%

code_hash_parse:
  | code_hash_entry* EOF
    {
      let module SM = Core.Std.String.Map in
      let module L = Core.Std.List in
      let l = L.dedup ~compare:(fun (k, _) (k', _) -> String.compare k k') $1 in
      SM.of_alist_exn l
    }

code_hash_entry:
  | STRING COLON LPAREN RPAREN STRING
    {
      let module S = Core.Std.String in
      $1, ([], $5)
    }
  | STRING COLON LPAREN separated_nonempty_list(COMMA, param) RPAREN STRING
    {
      let module S = Core.Std.String in
      $1, ($4, $6)
    }

param:
  | STRING { $1 }

values_parse:
  | assignment+ EOF
    {
      !term_hash, !bool_hash
    }

assignment:
  | bool_var_assignment { }
  | term_var_assignment { }

bool_var_assignment:
  | ID EQ TRUE
    {
      bool_hash := Core.Std.String.Map.add !bool_hash ~key:$1 ~data:true
    }
  | ID EQ FALSE
    {
      bool_hash := Core.Std.String.Map.add !bool_hash ~key:$1 ~data:false
    }

term_var_assignment:
  | VAR EQ term
    {
      term_hash := Core.Std.String.Map.add !term_hash ~key:$1 ~data:$3
    }

term:
  | NIL { Term.Nil }
  | NOMINAL INT { Term.NominalInt $2 }
  | INT { Term.OrdinalInt $1 }
  | STRING { Term.Symbol $1 }
  | ID { Term.Symbol $1 }
  | LPAREN term+ RPAREN
    {
      let open Core.Std in
      (* tuple of one element equals to the element itself *)
      if Poly.(List.length $2 = 1) then List.hd_exn $2 else Term.Tuple $2
    }
  | NONE
  | LSMILE RSMILE
    {
      Term.Choice (Core.Std.String.Map.empty, None)
    }
  | LBRACE RBRACE { Term.Nil }
  | LBRACKET RBRACKET { Term.Nil }
  | LSMILE separated_nonempty_list(COMMA, rec_entry) RSMILE
    {
      Term.Choice (Util.map_of_alist_exn $2, None)
    }
  | LBRACE separated_nonempty_list(COMMA, rec_entry) RBRACE
    {
      Term.Record (Util.map_of_alist_exn $2, None)
    }
  | LBRACKET separated_nonempty_list(COMMA, term) RBRACKET
    { Term.List ($2, None) }

rec_entry:
  | label COLON term
    {
      $1, (Logic.True, $3)
    }
  | error
    {
      Errors.parse_error "invalid record entry" $startpos $endpos
    }

label:
  | ID
    {
      $1
    }
  | STRING
    {
      if String.length $1 <= 2 then
        Errors.parse_error "Empty string as a label is not allowed" $startpos $endpos
      else
        $1
    }

