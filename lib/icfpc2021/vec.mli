open! Core

type t =
  { x : Float.t
  ; y : Float.t
  }
[@@deriving compare, fields, sexp]

val ( = ) : t -> t -> bool
val create : x:Float.t -> y:Float.t -> t

(** Computes the *square* of the distance between the two points. This is the
   distance that is used in the problem statement, but not what we usually mean
   by euclidian distance. *)
val sq_distance : t -> t -> Float.t

val equal : t -> t -> bool
val shift : t -> int * int -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val zero : t
val sq_length : t -> Float.t
val unit_length : t -> t
val scale : t -> Float.t -> t
val normalize_dir : t -> t
