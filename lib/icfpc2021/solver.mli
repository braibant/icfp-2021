open! Core

type t

val create : initial_pose:Pose.t -> manually_frozen_vertices:Int.Set.t -> t
val run : t -> steps_to_do:int -> t
val pose : t -> Pose.t
