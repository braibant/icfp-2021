type t =
  { a : Point.t
  ; b : Point.t
  }
[@@deriving fields, sexp]

val create : Point.t -> Point.t -> t
val intersect : t -> t -> bool
val distance : t -> Point.t -> float
val contains : t -> Point.t -> bool
val contains_segment : t -> other:t -> bool
