
exception WrongFormat of string

val term_to_cpp_macro : bool -> Term.t -> string
val term_to_type : Term.t -> string

val open_code_hash_file : string -> unit

val generate_from_terms : Core.Std.Out_channel.t -> string -> Term.t -> unit
val generate_from_bools : Core.Std.Out_channel.t -> string -> bool -> unit
