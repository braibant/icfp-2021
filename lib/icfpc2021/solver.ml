open! Core

type t =
  { pose : Pose.t
  ; manually_frozen_vertices : Int.Set.t
  ; frozen_vertices : Int.Set.t
  ; vertices_left : Int.Set.t
  ; vertex_edges : Edge.t list Int.Map.t
  ; alternative_offsets : Alternative_offsets.t
  }
[@@deriving fields]

let create ~initial_pose ~manually_frozen_vertices ~alternative_offsets =
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
  ; alternative_offsets
  }
;;

(* Pick the next vertex to work on: Looking at all the unfrozen
   vertices that are connected to [frozen_vertices], pick the one with
   the most connections to the [frozen_vertices]. *)
let pick_next_vertex ~vertices_left ~vertex_edges ~frozen_vertices =
  let next_vertex, conns, num_conns =
    List.map (Set.to_list vertices_left) ~f:(fun a ->
        let edges = Map.find vertex_edges a |> Option.value ~default:[] in
        let connected_vertices = List.map edges ~f:snd in
        let frozen_connections =
          List.filter connected_vertices ~f:(Set.mem frozen_vertices)
        in
        a, frozen_connections, List.length frozen_connections)
    |> List.sort ~compare:(fun (_, _, num_conns1) (_, _, num_conns2) ->
           Int.descending num_conns1 num_conns2)
    |> List.hd_exn
  in
  printf
    "Solver trying to fix vertex %d with %d frozen connections\n%!"
    next_vertex
    num_conns;
  next_vertex, conns, num_conns
;;

let find_alternative_positions_for_vertex
    vertex
    ~connections_to_frozen_vertices
    ~alternative_offsets
    ~pose
  =
  let alternative_positions_per_connected_node =
    List.map connections_to_frozen_vertices ~f:(fun connected_frozen_vertex ->
        let aos =
          Alternative_offsets.find alternative_offsets connected_frozen_vertex vertex
        in
        let connected_frozen_vertex =
          Map.find_exn (Pose.vertices pose) connected_frozen_vertex
        in
        List.map aos ~f:(fun (dx, dy) ->
            Point.create
              ~x:Bignum.(connected_frozen_vertex.x + dx)
              ~y:Bignum.(connected_frozen_vertex.y + dy))
        |> Point.Set.of_list)
  in
  match alternative_positions_per_connected_node with
  | [] -> Point.Set.empty
  | [ aps ] -> aps
  | aps :: apss -> List.fold_left apss ~init:aps ~f:(fun acc aps -> Set.inter acc aps)
;;

let rec run t =
  if Set.is_empty t.vertices_left
  then `Done t
  else (
    let next_vertex, conns, num_conns =
      pick_next_vertex
        ~vertices_left:t.vertices_left
        ~vertex_edges:t.vertex_edges
        ~frozen_vertices:t.frozen_vertices
    in
    printf
      "Solver trying to fix vertex %d with %d frozen connections\n%!"
      next_vertex
      num_conns;
    let alternative_positions =
      find_alternative_positions_for_vertex
        next_vertex
        ~connections_to_frozen_vertices:conns
        ~alternative_offsets:t.alternative_offsets
        ~pose:t.pose
    in
    printf "Solver found %d alternative positions\n%!" (Set.length alternative_positions);
    let rec alternative_position_loop = function
      | [] -> `Failed
      | pos :: rest_aps ->
        let pose = Pose.move t.pose next_vertex ~to_:pos in
        let frozen_vertices = Set.add t.frozen_vertices next_vertex in
        let vertices_left = Set.remove t.vertices_left next_vertex in
        let t = { t with pose; frozen_vertices; vertices_left } in
        (match run t with
        | `Done t -> `Done t
        | `Failed -> alternative_position_loop rest_aps)
    in
    alternative_position_loop (Set.to_list alternative_positions))
;;
