open Core
open Instrument
open Market_data

let taker_price (quote : Quote.t) = function
  | Bid -> quote.ask
  | Ask -> quote.bid |> Option.map ~f:Float.neg
;;

let find_end_with (target : string) (set : InstrumentSet.t) : Instrument.t list =
  Set.filter set ~f:(function
    | Future fut -> String.equal target fut
    | Spread (_, back) -> String.equal target back)
  |> Set.to_list
;;

let pathfind (target : Instrument.t) (set : InstrumentSet.t) : path list =
  let rec aux_fut (acc : path list) (wip : Instrument.t list) (curr : Instrument.t) =
    match curr with
    | Future _ -> { legs = wip } :: acc
    | Spread (front, _) ->
      let next_hops = find_end_with front set in
      List.fold ~init:acc next_hops ~f:(fun acc hop -> aux_fut acc (hop :: wip) hop)
  in
  match target with
  | Future future ->
    let starts = find_end_with future set in
    List.fold starts ~init:[] ~f:(fun acc start -> aux_fut acc [ start ] start)
  | Spread _ -> failwith "Unsupported"
;;

let cost_of (path : path) (book : Quote.t InstrumentMap.t) : float Or_error.t =
  let side_for = function
    | Instrument.Future _ -> Bid
    | Instrument.Spread _ -> Ask
  in
  let maybe_quotes = List.map path.legs ~f:(fun leg -> Map.find_or_error book leg) in
  match Or_error.all maybe_quotes with
  | Ok quotes ->
    List.fold quotes ~init:(Ok 0.0) ~f:(fun acc quote ->
      Result.bind acc ~f:(fun inner ->
        let side = side_for quote.instrument in
        let missing_market = Error.of_string ("No market on " ^ quote.ticker) in
        let maybe_px = Result.of_option (taker_price quote side) ~error:missing_market in
        Result.map maybe_px ~f:(fun px -> px +. inner)))
  | Error err -> Error err
;;

let find_best_path_for (instrument : Instrument.t) (book : Quote.t InstrumentMap.t) =
  let ins_set = InstrumentSet.of_list (Map.keys book) in
  let paths = pathfind instrument ins_set in
  let evals = List.map paths ~f:(fun path -> path, cost_of path book) in
  let valid_paths =
    List.filter_map evals ~f:(fun (path, eval) ->
      match eval with
      | Ok cost -> Some (path, cost)
      | Error _ -> None)
  in
  List.min_elt valid_paths ~compare:(fun (_, lcost) (_, rcost) ->
    Float.compare lcost rcost)
;;

let hi =
  print_endline "\n==Camino==";
  let test_data = In_channel.read_lines "data/test" in
  let quotes =
    test_data |> List.filter_map ~f:(fun line -> line |> parse_line |> Result.ok)
  in
  let book = build_book InstrumentMap.empty quotes in
  let instrument = Instrument.Future "CLZ5" in
  match find_best_path_for instrument book with
  | Some (path, cost) ->
    print_endline
      ("Best path to get to " ^ (instrument |> Dbg.tostr [%sexp_of: Instrument.t]));
    path |> Dbg.print [%sexp_of: path];
    print_endline ("Where cost is: " ^ Float.to_string cost)
  | None -> failwith "no path"
;;
