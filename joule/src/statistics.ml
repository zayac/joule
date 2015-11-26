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

let t =
  {
    name = "";
    constraints = 0;
    aux_constraints = 0;
    term_variables = 0;
    flags = 0;
    iterations = 0;
    solutions = 0;
    approximations = 0;
    average_approximations = 0;
    execution_time = ""
 }

 let str () =
   Printf.sprintf "-= Statistics -=
file name: %s
# of constraints: %d
# of auxiliary constraints: %d
# of term variables: %d
# of flags: %d
# of iterations: %d
# of solutions: %d
# of approximations: %d
# of approximations per variable: %d
execution time: %s"
  t.name
  t.constraints
  t.aux_constraints
  t.term_variables
  t.flags
  t.iterations
  t.solutions
  t.approximations
  t.average_approximations
  t.execution_time

let csv () =
  Printf.sprintf "%s,%d,%d,%d,%d,%d,%d,%d,%d,%s"
    t.name
    t.constraints
    t.aux_constraints
    t.term_variables
    t.flags
    t.iterations
    t.solutions
    t.approximations
    t.average_approximations
    t.execution_time
