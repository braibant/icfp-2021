open! Core

type t [@@deriving sexp]

val empty : t
val create : Problem.t -> t
val find : t -> int -> int -> (Bignum.t * Bignum.t) list
