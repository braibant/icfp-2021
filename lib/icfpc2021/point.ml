open! Core

module T = struct
  type t =
    { x : Bignum.t
    ; y : Bignum.t
    }
  [@@deriving compare, fields, sexp]
end

include T
include Comparable.Make (T)

let create = Fields.create
let distance a b = Bignum.(((a.x - b.x) ** 2) + ((a.y - b.y) ** 2))
let equal a b = Bignum.equal a.x b.x && Bignum.equal a.y b.y
let shift t (dx, dy) = { x = Bignum.(t.x + of_int dx); y = Bignum.(t.y + of_int dy) }
