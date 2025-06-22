open Core
open Incremental
module Inc = Incremental.Make ()

type node = { id : int } [@@deriving sexp]
type edge = { left : node; right : node; distance : float } [@@deriving sexp]
type graph = { edges : edge list } [@@deriving sexp]

let hi =
  let myconst = Inc.const 1 in
  let myvar = Inc.Var.create 10 in
  let myvar_witness = Inc.Var.watch myvar in
  let myadd = Inc.map2 myconst myvar_witness ~f:(fun a b -> a + b) in
  let seen = observe myadd in
  Inc.stabilize ();
  Inc.Observer.value seen |> Or_error.ok_exn |> Int.to_string |> print_endline;
  Inc.Var.set myvar 20;
  Inc.stabilize ();
  Inc.Observer.value seen |> Or_error.ok_exn |> Int.to_string |> print_endline
