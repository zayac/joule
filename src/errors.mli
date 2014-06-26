open Core.Std

(** An exception that is thrown by a parser *)
exception Parsing_Error of string

(** Includes information about location in the code to the error message *)
val error : ?loc:Location.location option -> string -> string

(** A wrapper for [Parsing_Error] exception that constructs a string that
    includes information about error location in the code *)
val parse_error : string -> Lexing.position -> Lexing.position -> 'a
