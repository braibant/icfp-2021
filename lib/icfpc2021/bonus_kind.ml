open! Core

type t =
  | Globalist
  | Break_a_leg
  | Wallhack
[@@deriving compare, equal, sexp]

let of_string = function
  | "GLOBALIST" -> Globalist
  | "BREAK_A_LEG" -> Break_a_leg
  | "WALLHACK" -> Wallhack
  | str -> failwithf "Cannot parse Bonus_kind.t from %s" str ()
;;
