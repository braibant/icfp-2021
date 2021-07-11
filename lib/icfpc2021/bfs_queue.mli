open! Core

type t

val empty : t
val enqueue : t -> int -> t
val mem : t -> int -> bool

val dequeue_with_most_connections_to_frozen
  :  t
  -> vertices:Point.t Int.Map.t
  -> frozen_vertices:Int.Set.t
  -> vertex_edges:Edge.t list Int.Map.t
  -> (int * t) option
