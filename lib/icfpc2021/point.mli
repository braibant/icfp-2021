open! Core

type t =
  { x : Bignum.t
  ; y : Bignum.t
  }
[@@deriving fields, sexp]

val create : x:Bignum.t -> y:Bignum.t -> t
val distance : t -> t -> Bignum.t
