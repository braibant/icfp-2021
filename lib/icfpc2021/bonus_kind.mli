open! Core

type t =
  | Globalist
  | Break_a_leg
  | Wallhack
[@@deriving compare, equal, sexp]

val of_string : string -> t
