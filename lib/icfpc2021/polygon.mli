type t = Point.t array

val of_vertices : Point.t list -> t
val contains : t -> Point.t -> bool
val intersect_segment : t -> Segment.t -> bool

(** Returns the minimum distance between the polygon edges and the given point.
   If the point lies inside the polygon, the distance returned is a positive
   number. If the point lies outside the polygon, the distance returned is
   negative. *)
val distance : t -> Point.t -> float
