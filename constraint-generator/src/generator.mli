open Core.Std

exception MessageFormat_Error of string

val gen_class_constraints :
     ((Cnf.t * Term.t) Core.Std.String.Map.t) Core.Std.String.Map.t
     -> Term.t
     -> Term.t
     -> (Term.t * Term.t) list
