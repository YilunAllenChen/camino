open Core

type ticker = string [@@deriving sexp, compare, hash]

module Instrument = struct
  module T = struct
    type t =
      | Future of ticker
      | Spread of ticker * ticker
    [@@deriving sexp, compare, hash]
  end

  include T
end

module Quote = struct
  module T = struct
    type t =
      { ticker : ticker
      ; instrument : Instrument.t
      ; bid : float option
      ; ask : float option
      }
    [@@deriving sexp, compare, hash]
  end

  include T
end

type path = { legs : Instrument.t list } [@@deriving sexp]

type side =
  | Bid
  | Ask
[@@deriving sexp]
