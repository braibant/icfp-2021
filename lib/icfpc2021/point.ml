open! Core

type t =
  { x : Bignum.t
  ; y : Bignum.t
  }
[@@deriving fields, sexp]

let create = Fields.create
let distance a b = Bignum.(((a.x - b.x) ** 2) + ((a.y - b.y) ** 2))
let equal a b = Bignum.equal a.x b.x && Bignum.equal a.y b.y
