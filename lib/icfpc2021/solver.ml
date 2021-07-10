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

module Stack_frame = struct
  type nonrec t =
    { solver_t : t
    ; next_vertex : int
    ; alternative_positions : Point.t list
    }
end

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
  (* printf
   *   "Solver trying to fix vertex %d with %d frozen connections\n%!"
   *   next_vertex
   *   num_conns; *)
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

let rec recursive_run t =
  if Set.is_empty t.vertices_left
  then `Done t
  else (
    let next_vertex, conns, _num_conns =
      pick_next_vertex
        ~vertices_left:t.vertices_left
        ~vertex_edges:t.vertex_edges
        ~frozen_vertices:t.frozen_vertices
    in
    (* printf
     *   "Solver trying to fix vertex %d with %d frozen connections\n%!"
     *   next_vertex
     *   num_conns; *)
    let alternative_positions =
      find_alternative_positions_for_vertex
        next_vertex
        ~connections_to_frozen_vertices:conns
        ~alternative_offsets:t.alternative_offsets
        ~pose:t.pose
    in
    (* printf "Solver found %d alternative positions\n%!" (Set.length alternative_positions); *)
    let rec alternative_position_loop = function
      | [] -> `Failed
      | pos :: rest_aps ->
        let pose = Pose.move t.pose next_vertex ~to_:pos in
        (* CR scvalex: Check that all edges connecting next_vertex to
           the frozen_vertices in the new pose are inside the
           polygon. *)
        let frozen_vertices = Set.add t.frozen_vertices next_vertex in
        let vertices_left = Set.remove t.vertices_left next_vertex in
        let t = { t with pose; frozen_vertices; vertices_left } in
        (match recursive_run t with
        | `Done t -> `Done t
        | `Failed -> alternative_position_loop rest_aps)
    in
    alternative_position_loop (Set.to_list alternative_positions))
;;

let create_deeper_stack_frame t =
  let next_vertex, conns, _num_conns =
    pick_next_vertex
      ~vertices_left:t.vertices_left
      ~vertex_edges:t.vertex_edges
      ~frozen_vertices:t.frozen_vertices
  in
  (* printf
   *   "Solver trying to fix vertex %d with %d frozen connections\n%!"
   *   next_vertex
   *   num_conns; *)
  let alternative_positions =
    find_alternative_positions_for_vertex
      next_vertex
      ~connections_to_frozen_vertices:conns
      ~alternative_offsets:t.alternative_offsets
      ~pose:t.pose
    |> Set.to_list
  in
  (* printf "Solver found %d alternative positions\n%!" (List.length alternative_positions); *)
  Stack_frame.{ solver_t = t; next_vertex; alternative_positions }
;;

let create_initial_stack t = [ create_deeper_stack_frame t ]

let all_edges_to_frozen_inside_hole vertex ~frozen_vertices ~vertex_edges ~pose =
  let edges = Map.find vertex_edges vertex |> Option.value ~default:[] in
  let connected_vertices = List.map edges ~f:snd in
  let frozen_connections = List.filter connected_vertices ~f:(Set.mem frozen_vertices) in
  List.for_all frozen_connections ~f:(fun frozen_vertex ->
      Pose.edge_inside_hole pose (vertex, frozen_vertex))
;;

let incremental_run t0 ~work_to_do:work_to_do0 ~stack:stack0 =
  (* printf "\n\nSolver.incremental_run ~stack:%d\n%!" (List.length stack0); *)
  let rec incremental_loop t ~work_to_do ~stack =
    (* printf "incremental_loop ~stack:%d\n%!" (List.length stack); *)
    if Set.is_empty t.vertices_left
    then `Done t
    else if work_to_do <= 0
    then `Todo (t, stack)
    else (
      match (stack : Stack_frame.t list) with
      | [] -> failwith "Solver.incremental_run: empty stack"
      | { solver_t; next_vertex; alternative_positions } :: rest_stack ->
        (* solver_t is always the same as t, at this point *)
        (match
           alternative_position_loop
             solver_t
             alternative_positions
             ~work_to_do
             ~next_vertex
             ~stack_excluding_self:rest_stack
         with
        | `Done t -> `Done t
        | `Todo (t, stack) -> `Todo (t, stack)
        | `Failed work_to_do ->
          (* printf "incremental_loop failed at depth %d\n%!" (1 + List.length rest_stack); *)
          `Failed work_to_do))
  and alternative_position_loop
      (t : t)
      ~work_to_do
      ~next_vertex
      ~stack_excluding_self
      alternative_positions
    =
    (* let stack_depth = List.length stack_excluding_self + 1 in *)
    (* printf "alternative_position_loop\n%!";
     * List.iteri
     *   (List.rev stack_excluding_self)
     *   ~f:(fun i Stack_frame.{ alternative_positions; _ } ->
     *     printf
     *       "  - stack depth %d: alternative_positions=%d\n%!"
     *       (i + 1)
     *       (List.length alternative_positions));
     * printf
     *   "  - stack depth %d: alternative_positions=%d\n%!"
     *   stack_depth
     *   (List.length alternative_positions); *)
    if work_to_do <= 0
    then
      `Todo
        ( t
        , Stack_frame.{ solver_t = t; next_vertex; alternative_positions }
          :: stack_excluding_self )
    else (
      match alternative_positions with
      | [] -> `Failed work_to_do
      | pos :: rest_aps ->
        let pose = Pose.move t.pose next_vertex ~to_:pos in
        if all_edges_to_frozen_inside_hole
             next_vertex
             ~pose
             ~frozen_vertices:t.frozen_vertices
             ~vertex_edges:t.vertex_edges
        then (
          let frozen_vertices = Set.add t.frozen_vertices next_vertex in
          let vertices_left = Set.remove t.vertices_left next_vertex in
          let updated_t = { t with pose; frozen_vertices; vertices_left } in
          let cur_frame =
            (* CR scvalex: Should this solver_t be t or updated_t? *)
            Stack_frame.{ solver_t = t; next_vertex; alternative_positions = rest_aps }
          in
          if Set.is_empty vertices_left
          then `Done updated_t
          else (
            let updated_stack =
              create_deeper_stack_frame updated_t :: cur_frame :: stack_excluding_self
            in
            match
              incremental_loop updated_t ~work_to_do:(work_to_do - 1) ~stack:updated_stack
            with
            | `Done t -> `Done t
            | `Todo (t, stack) -> `Todo (t, stack)
            | `Failed work_to_do ->
              (* printf "trying alternatives after failure\n%!"; *)
              alternative_position_loop
                t
                rest_aps
                ~next_vertex
                ~work_to_do
                ~stack_excluding_self))
        else
          (* The new vertex has edges that go outside the hole. *)
          (* printf "trying alternatives edge overlap\n%!"; *)
          alternative_position_loop
            t
            rest_aps
            ~next_vertex
            ~work_to_do
            ~stack_excluding_self)
  in
  incremental_loop t0 ~work_to_do:work_to_do0 ~stack:stack0
;;
