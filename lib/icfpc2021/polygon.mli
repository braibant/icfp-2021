type t = Point.t array

val of_vertices : Point.t list -> t
val contains : t -> Point.t -> bool
val intersect_segment : t -> Segment.t -> bool
