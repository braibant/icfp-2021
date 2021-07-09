open! Core

type t =
  { hole : Point.t list
  ; figure_edges : Point.t list
  ; figure_vertices : Point.t list
  ; epsilon : Bignum.t
  }
[@@deriving sexp]

val load_exn : filename:string -> t
val max_xy : t -> Bignum.t * Bignum.t
val to_string_hum : t -> string
