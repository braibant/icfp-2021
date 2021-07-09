open! Core

type t =
  { x : Bignum.t
  ; y : Bignum.t
  }
[@@deriving sexp]
