open! Core

type t =
  { x : Bignum.t
  ; y : Bignum.t
  }
[@@deriving fields, sexp]

let create = Fields.create
