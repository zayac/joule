type stats =
  { mutable name : string;
    mutable constraints : int;
    mutable aux_constraints : int;
    mutable term_variables : int;
    mutable flags : int;
    mutable iterations : int;
    mutable solutions : int;
    mutable approximations : int;
    mutable average_approximations : int;
    mutable execution_time : string;
  }

val t : stats

val str : unit -> string
val csv : unit -> string
