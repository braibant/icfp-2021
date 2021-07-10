open! Core

type t

val create
  :  initial_pose:Pose.t
  -> manually_frozen_vertices:Int.Set.t
  -> alternative_offsets:Alternative_offsets.t
  -> t

val run : t -> [ `Done of t | `Failed ]
val pose : t -> Pose.t
