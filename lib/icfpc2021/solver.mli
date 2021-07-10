open! Core

type t

val create
  :  initial_pose:Pose.t
  -> manually_frozen_vertices:Int.Set.t
  -> alternative_offsets:Alternative_offsets.t
  -> t

val recursive_run : t -> [ `Done of t | `Failed ]
val pose : t -> Pose.t

module Stack_frame : sig
  type t
end

val incremental_run
  :  t
  -> work_to_do:int
  -> stack:Stack_frame.t list
  -> [ `Done of t | `Todo of t * Stack_frame.t list | `Failed of int ]

val create_initial_stack : t -> Stack_frame.t list
