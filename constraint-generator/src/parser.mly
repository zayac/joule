%{
  let additional_constraints = ref []
  let class_hash = ref Core.Std.String.Map.empty
  let g = ref Generator.G.empty
  let in_term_map = ref Core.Std.Int.Map.empty
  let out_term_map = ref Core.Std.Int.Map.empty


  let index_class lt rt =
    let open Core.Std in
    let open Term in
    (* If left term is a variable, and the right one is a structure, we assume
       that the constraint is the definition of an interface object. *)
    match lt, rt with
    | DownVar v, Record (r, None) ->
      class_hash := String.Map.add !class_hash ~key:v ~data:r
    | _ -> ()
%}

%token <int> INT
%token <string> UP_VAR DOWN_VAR ID STRING
%token NONE
%token NIL TRUE FALSE NOT OR AND
%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET LANGULAR RANGULAR LSMILE RSMILE LAUX RAUX
%token COLON COMMA BAR NOMINAL AT EOF
%token SCOLON LEQ EQ

%start <Term.t Core.Std.Int.Map.t *
        Term.t Core.Std.Int.Map.t *
        (Term.t * Term.t) list *
        ((Cnf.t * Term.t) Core.Std.String.Map.t) Core.Std.String.Map.t> term_parse
%start <Generator.G.t> netlist_parse
%%

netlist_parse:
  | channel+ EOF { !g }

channel:
  | ID AT INT INT AT ID
    {
      g := Generator.G.add_edge_e !g (Generator.G.E.create $1 ($3, $4) $6)
      (*match (Generator.match_out_channel $1), (Generator.match_in_channel $2) with*)
      (*| (Some ch, comp), (Some ch', comp') ->*)
        (*g := Generator.G.add_edge_e !g (Generator.G.E.create comp (ch, ch') comp')*)
      (*| _ ->*)
        (*Errors.parse_error "wrong format of a channel" $startpos $endpos*)
    }
  | error
    { Errors.parse_error "invalid connection" $startpos $endpos }


term_parse:
  | constrs? in_interface out_interface EOF
    {
      let ret_class_hash = !class_hash in
      let _ = class_hash := Core.Std.String.Map.empty in
      !in_term_map, !out_term_map, !additional_constraints, ret_class_hash
    }

constrs:
  | LAUX constr* RAUX {}
  | error
    { Errors.parse_error "wrong format of auxiliary constraints" $startpos $endpos }

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

in_interface:
  | ID channel_map+
    {
      match $1 with
      | "IN" ->
        Core.Std.List.iter $2 ~f:(fun el ->
          let ch, t = el in
          in_term_map := Core.Std.Int.Map.add !in_term_map ~key:ch ~data:t
        )
      | _ -> Errors.parse_error "Expected 'IN' keyword" $startpos $endpos
    }

out_interface:
  | ID channel_map+
    {
      match $1 with
      | "OUT" ->
        Core.Std.List.iter $2 ~f:(fun el ->
          let ch, t = el in
          out_term_map := Core.Std.Int.Map.add !out_term_map ~key:ch ~data:t
        )
      | _ -> Errors.parse_error "Expected 'OUT' keyword" $startpos $endpos
    }

channel_map:
  | INT COLON term
    {
      $1, $3
    }

term:
  | NIL { Term.Nil }
  | NOMINAL INT { Term.NominalInt $2 }
  | INT { Term.OrdinalInt $1 }
  | STRING { Term.Symbol $1 }
  | UP_VAR { Term.UpVar $1 }
  | DOWN_VAR { Term.DownVar $1 }
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
  | LSMILE separated_nonempty_list(COMMA, rec_entry) rec_list_tail_choice? RSMILE
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
  | UP_VAR
  | DOWN_VAR
    {
      Errors.parse_error "invalid Boolean expression" $startpos $endpos
    }
  | error { Errors.parse_error "wrong format of a flag" $startpos $endpos }

rec_list_tail_choice:
| BAR UP_VAR { $2 }

rec_list_tail:
  | BAR DOWN_VAR { $2 }

