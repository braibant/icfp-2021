open! Core

type t =
  | Globalist
  | Break_a_leg
  | Wallhack
  | Superflex
[@@deriving compare, equal, sexp]

let to_string = function
  | Globalist -> "GLOBALIST"
  | Break_a_leg -> "BREAK_A_LEG"
  | Wallhack -> "WALLHACK"
  | Superflex -> "SUPERFLEX"
;;

let of_string = function
  | "GLOBALIST" -> Globalist
  | "BREAK_A_LEG" -> Break_a_leg
  | "WALLHACK" -> Wallhack
  | "SUPERFLEX" -> Superflex
  | str -> failwithf "Cannot parse Bonus_kind.t from %s" str ()
;;
