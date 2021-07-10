

type t = Point.t array

val contains : t -> Point.t -> bool
val intersect_segment : t -> Segment.t -> bool
