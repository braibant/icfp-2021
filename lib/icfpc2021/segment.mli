type t =
  { a : Point.t
  ; b : Point.t
  }
[@@deriving fields, sexp]

val create : Point.t -> Point.t -> t
val intersect : t -> t -> bool
