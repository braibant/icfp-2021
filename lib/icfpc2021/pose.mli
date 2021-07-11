open! Core

type t

val create : Problem.t -> t
val problem : t -> Problem.t
val vertices : t -> Point.t Int.Map.t
val set_vertices : t -> Point.t list -> t
val set_vertices' : t -> Point.t Int.Map.t -> t

(* io *)

val load_exn : problem:Problem.t -> filename:string -> t * Int.Set.t
val save_exn : t -> frozen_vertices:Int.Set.t -> filename:string -> unit

(* Get segment from edge *)
val segment : t -> Edge.t -> Segment.t

(* get vertex by number *)
val vertex : t -> int -> Point.t

(* query *)
val edge_invalid : t -> Edge.t -> Bignum.t option
val invalid_edges : t -> (Edge.t * Bignum.t) list
val min_max_length_sq_for_edge : t -> Edge.t -> Bignum.t * Bignum.t
val edge_inside_hole : t -> Edge.t -> bool
val segment_inside_hole : t -> Segment.t -> bool
val dislikes : t -> int

val find_pose_edge_that_matches_hole_edge
  :  t
  -> frozen:Int.Set.t
  -> ((Point.t * Point.t) * (int * int)) list

val sort_by_min_distance_to_hole_vertices : t -> Point.t list -> Point.t list
val inside_hole : t -> bool

(* transform *)
val move : t -> int -> to_:Point.t -> t
val shift : t -> frozen:Int.Set.t -> Edge.t -> t
val reflect_vertical : t -> t
val transpose : t -> t

module Springs : sig
  val relax_one : t -> frozen:Int.Set.t -> Point.t Int.Map.t
end
