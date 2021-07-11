open! Core

type t =
  { kind : Bonus_kind.t
  ; problem : int
  }
[@@deriving sexp]
