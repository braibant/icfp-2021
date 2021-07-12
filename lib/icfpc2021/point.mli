open! Core

type t =
  { x : Bignum.t
  ; y : Bignum.t
  }
[@@deriving compare, fields, hash, sexp]

include Comparable.S with type t := t
include Hashable.S with type t := t

val create : x:Bignum.t -> y:Bignum.t -> t

(** Computes the *square* of the distance between the two points. This is the
   distance that is used in the problem statement, but not what we usually mean
   by euclidian distance. *)
val sq_distance : t -> t -> Bignum.t

val equal : t -> t -> bool
val shift : t -> int * int -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val zero : t
val sq_length : t -> Bignum.t
val unit_length : t -> t
val scale : t -> Bignum.t -> t
val normalize_dir : t -> t
val dirs : t list
