open Core
open Instrument

let parse_level (str : string) : float option =
  match String.strip str with
  | "None" -> None
  | other -> Float.of_string_opt other
;;

let parse_instrument (ticker : string) : (Instrument.t, string) Result.t =
  let splitted_on_dash = String.split ~on:'-' ticker in
  match splitted_on_dash with
  | [] -> Error ("Impossible: " ^ ticker)
  | [ single ] -> Ok (Future single)
  | [ front; back ] -> Ok (Spread (front, back))
  | _ -> Error ("Unsupported: " ^ ticker)
;;

let line_pattern = Re.Pcre.regexp {|^([A-Z0-9-]+)( [\d\w\.-]+)( [\d\w\.-]+)|}

let parse_line (line : string) : (Quote.t, string) Result.t =
  let maybe_groups = Re.exec_opt line_pattern line in
  match maybe_groups with
  | Some groups ->
    let substrs = Re.Group.all groups in
    let maybe_ins = parse_instrument substrs.(1) in
    Result.map maybe_ins ~f:(fun ins ->
      Quote.
        { ticker = substrs.(1)
        ; instrument = ins
        ; bid = parse_level substrs.(2)
        ; ask = parse_level substrs.(3)
        })
  | None -> Error ("can't parse line: " ^ line)
;;

module InstrumentMap = Map.Make (Instrument)
module InstrumentSet = Set.Make (Instrument)

let rec build_book (book : Quote.t InstrumentMap.t) (incoming : Quote.t list) =
  match incoming with
  | [] -> book
  | hd :: tl -> build_book (Map.update book hd.instrument ~f:(fun _ -> hd)) tl
;;
