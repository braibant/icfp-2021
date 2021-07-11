open! Core

type t =
  | Globalist
  | Break_a_leg
  | Wallhack
  | Superflex
[@@deriving compare, equal, sexp]

val to_string : t -> string
val of_string : string -> t
