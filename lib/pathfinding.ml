open Core

module Instrument = struct
  module T = struct
    type t = Future of string | Spread of string * string
    [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

type quote = {
  ticker : string;
  instrument : Instrument.t;
  bid : float option;
  ask : float option;
}
[@@deriving sexp]

let parse_level (str : string) : float option =
  match String.strip str with
  | "None" -> None
  | other -> Float.of_string_opt other

let parse_instrument (ticker : string) : (Instrument.t, string) Result.t =
  let splitted_on_dash = String.split ~on:'-' ticker in
  match splitted_on_dash with
  | [] -> Error ("Impossible: " ^ ticker)
  | [ single ] -> Ok (Future single)
  | [ front; back ] -> Ok (Spread (front, back))
  | _ -> Error ("Unsupported: " ^ ticker)

let line_pattern = Re.Pcre.regexp {|^([A-Z0-9-]+)( [\d\w\.-]+)( [\d\w\.-]+)|}

let parse_line (line : string) : (quote, string) Result.t =
  let maybe_groups = Re.exec_opt line_pattern line in
  match maybe_groups with
  | Some groups ->
      let substrs = Re.Group.all groups in
      let maybe_ins = parse_instrument substrs.(1) in
      Result.map maybe_ins ~f:(fun ins ->
          {
            ticker = substrs.(1);
            instrument = ins;
            bid = parse_level substrs.(2);
            ask = parse_level substrs.(3);
          })
  | None -> Error ("can't parse line: " ^ line)

module InstrumentSet = Set.Make (Instrument)

type path = { legs : Instrument.t list } [@@deriving sexp]

let find_end_with (target : string) (from : InstrumentSet.t) : Instrument.t list
    =
  Set.filter from ~f:(function
    | Future fut -> String.equal target fut
    | Spread (_, back) -> String.equal target back)
  |> Set.to_list

let pathfind (target : Instrument.t) (set : InstrumentSet.t) : path list =
  let rec aux_fut (acc : path list) (wip : Instrument.t list)
      (curr : Instrument.t) =
    match curr with
    | Future _ -> { legs = wip } :: acc
    | Spread (front, _) ->
        let next_hops = find_end_with front set in
        List.fold ~init:acc next_hops ~f:(fun acc hop ->
            aux_fut acc (hop :: wip) hop)
  in
  match target with
  | Future future ->
      let starts = find_end_with future set in
      List.fold starts ~init:[] ~f:(fun acc start ->
          aux_fut acc [ start ] start)
  | Spread _ -> failwith "Unsupported"

let hi =
  let test_data = In_channel.read_lines "data/test" in
  print_endline "\n\n==Camino==";
  let quotes =
    test_data |> List.map ~f:parse_line |> List.filter_map ~f:Result.ok
  in
  let ins_set =
    InstrumentSet.of_list (List.map quotes ~f:(fun q -> q.instrument))
  in
  let res = pathfind (Future "CLZ5") ins_set in
  res |> Dbg.print [%sexp_of: path list]
