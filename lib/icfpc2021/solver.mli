open! Core

module Kind : sig
  type t =
    | Dfs
    | Bfs
  [@@deriving sexp]
end

type t

val create
  :  Kind.t
  -> initial_pose:Pose.t
  -> manually_frozen_vertices:Int.Set.t
  -> alternative_offsets:Alternative_offsets.t
  -> t

val pose : t -> Pose.t
val run : t -> work_to_do:int -> [ `Done of t | `Todo of t | `Failed ]
