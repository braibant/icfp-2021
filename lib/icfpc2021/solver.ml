open! Core

type t =
  { pose : Pose.t
  ; manually_frozen_vertices : Int.Set.t
  ; edges_to_solve : Edge.Set.t
  }
[@@deriving fields]

let create ~initial_pose ~manually_frozen_vertices =
  let edges_to_solve =
    (Pose.problem initial_pose).figure_edges
    |> List.filter ~f:(fun (idx1, idx2) ->
           (not (Set.mem manually_frozen_vertices idx1))
           || not (Set.mem manually_frozen_vertices idx2))
    |> Edge.Set.of_list
  in
  { pose = initial_pose; manually_frozen_vertices; edges_to_solve }
;;

let run t ~steps_to_do:_ = t
