open Core

let debug sexp_of x =
  x |> sexp_of |> Sexp.to_string_hum |> print_endline;
  x

let print sexp_of x = x |> sexp_of |> Sexp.to_string_hum |> print_endline
