open! Core

type t =
  { hole : Point.t list
  ; figure_edges : Point.t list
  ; figure_vertices : Point.t list
  ; epsilon : Bignum.t
  }
[@@deriving sexp]

val load_exn : filename:string -> t
