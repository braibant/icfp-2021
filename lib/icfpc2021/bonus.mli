open! Core

type t =
  { position : Point.t
  ; bonus : Bonus_kind.t
  ; problem : int
  }
[@@deriving sexp]
