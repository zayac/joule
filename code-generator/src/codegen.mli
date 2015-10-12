
exception WrongFormat of string

val term_to_cpp_macro : bool -> Term.t -> string
val term_to_type : Term.t -> string

val open_code_hash_file : string -> unit

val read_json_file : string -> string -> unit

val methods_variables : Term.t Core.Std.String.Map.t ref

val print_salvo_routing: Core.Std.Out_channel.t -> string -> unit

(*val term_to_cpp_placeholders : string -> Core.Std.Out_channel.t -> unit*)

val create_class_decl : string -> (Cnf.t * Term.t) Core.Std.String.Map.t -> string

val generate_from_terms : Core.Std.Out_channel.t -> string -> string -> Term.t -> unit
val generate_from_bools : Core.Std.Out_channel.t -> string -> bool -> unit

