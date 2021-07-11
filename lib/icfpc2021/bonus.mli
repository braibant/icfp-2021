open! Core

type t =
  { position : Point.t
  ; kind : Bonus_kind.t
  ; problem : int
  }
[@@deriving sexp]
