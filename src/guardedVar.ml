open Core.Std

module T = struct
  type t =
    { var : string;
      logic : Cnf.t;
    }
  with sexp, compare
end
include T
include Comparable.Make(T)
