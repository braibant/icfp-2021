open! Core

module T = struct
  type t =
    { x : Bignum.t
    ; y : Bignum.t
    }
  [@@deriving compare, fields, hash, sexp]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let create = Fields.create
let zero = { x = Bignum.zero; y = Bignum.zero }
let sq_distance a b = Bignum.(((a.x - b.x) ** 2) + ((a.y - b.y) ** 2))
let sq_length a = sq_distance a zero
let equal a b = Bignum.equal a.x b.x && Bignum.equal a.y b.y
let shift t (dx, dy) = { x = Bignum.(t.x + of_int dx); y = Bignum.(t.y + of_int dy) }
let ( + ) a b = Bignum.{ x = a.x + b.x; y = a.y + b.y }
let ( - ) a b = Bignum.{ x = a.x - b.x; y = a.y - b.y }
let scale t k = Bignum.{ x = t.x * k; y = t.y * k }

let unit_length t =
  let length = Bignum.of_float_decimal (Float.sqrt (Bignum.to_float (sq_length t))) in
  if Bignum.(length = zero) then Printf.eprintf !"Unit length %{sexp: t}\n%!" t;
  let k = Bignum.(one / length) in
  scale t k
;;

(* Normalize the given vector to one of U, L, D, R. We could be more precise here, and consider 8 neighbours. *)
let normalize_dir (t : t) : t =
  let open Bignum in
  if t.y >= abs t.x (* U *)
  then { x = zero; y = one }
  else if t.y <= zero - abs t.x (* D *)
  then { x = zero; y = zero - one }
  else if zero <= t.x
  then { x = one; y = zero }
  else { x = zero - one; y = zero }
;;
