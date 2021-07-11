open! Core

type t =
  { hole : Point.t list
  ; figure_edges : Edge.t list
  ; figure_vertices : Point.t list
  ; epsilon : Bignum.t
  ; bonuses : Bonus.t list
  }
[@@deriving sexp]

val load_exn : filename:string -> t
val max_xy : t -> Bignum.t * Bignum.t
val to_string_hum : t -> string

(* computes 1000 * log2 component of the score equation *)
val score : t -> float
val neighbours : t -> int list Int.Map.t
