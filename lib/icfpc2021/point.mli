open! Core

type t =
  { x : Bignum.t
  ; y : Bignum.t
  }
[@@deriving fields, sexp]

include Comparable.S with type t := t

val create : x:Bignum.t -> y:Bignum.t -> t
val distance : t -> t -> Bignum.t
val equal : t -> t -> bool
val shift : t -> int * int -> t
