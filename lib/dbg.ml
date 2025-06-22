open Core

let tostr (sexp_of : 'a -> Sexp.t) (x : 'a) = x |> sexp_of |> Sexp.to_string_hum
let print (sexp_of : 'a -> Sexp.t) (x : 'a) = tostr sexp_of x |> print_endline

let debug (sexp_of : 'a -> Sexp.t) (x : 'a) =
  print sexp_of x;
  x
;;
