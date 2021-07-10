open! Core

type t =
  { pose : Pose.t
  ; manually_frozen_vertices : Int.Set.t
  ; frozen_vertices : Int.Set.t
  ; vertices_left : Int.Set.t
  ; vertex_edges : Edge.t list Int.Map.t
  }
[@@deriving fields]

let create ~initial_pose ~manually_frozen_vertices =
  let vertices_left =
    Set.diff
      (Map.keys (Pose.vertices initial_pose) |> Int.Set.of_list)
      manually_frozen_vertices
  in
  let vertex_edges =
    List.fold_left
      ~init:Int.Map.empty
      (Pose.problem initial_pose).figure_edges
      ~f:(fun vertex_edges ((a, b) as edge) ->
        let vertex_edges =
          Map.update vertex_edges a ~f:(fun vs ->
              let vs = Option.value vs ~default:[] in
              edge :: vs)
        in
        Map.update vertex_edges b ~f:(fun vs ->
            let vs = Option.value vs ~default:[] in
            (b, a) :: vs))
  in
  { pose = initial_pose
  ; manually_frozen_vertices
  ; frozen_vertices = manually_frozen_vertices
  ; vertices_left
  ; vertex_edges
  }
;;

let run t ~steps_to_do:_ =
  if Set.is_empty t.vertices_left
  then t
  else (
    let next_vertex, _conns, num_conns =
      List.map (Set.to_list t.vertices_left) ~f:(fun a ->
          let edges = Map.find t.vertex_edges a |> Option.value ~default:[] in
          let connected_vertices = List.map edges ~f:snd in
          let frozen_connections =
            List.filter connected_vertices ~f:(Set.mem t.frozen_vertices)
          in
          a, frozen_connections, List.length frozen_connections)
      |> List.sort ~compare:(fun (_, _, conns1) (_, _, conns2) ->
             Int.descending conns1 conns2)
      |> List.hd_exn
    in
    printf
      "Solver trying to fix vertex %d with %d frozen connections\n%!"
      next_vertex
      num_conns;
    (* CR scvalex: Compute the set of all possible indices for this
       vertex by intersecting the sets of alternative offsets for all
       the connected frozen vertices. *)
    t)
;;
