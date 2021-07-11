open! Core

module Kind = struct
  type t =
    | Dfs
    | Bfs
    | Chf
  [@@deriving sexp]
end

module Incremental = struct
  type t =
    { kind : [ `Bfs | `Dfs ]
    ; pose : Pose.t
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
            (** In [solver_t] in this Stack_frame, the [vertex] is *not*
         already positioned and frozen. *)
      ; vertex : int
      ; alternative_positions : Point.t list
      ; queue : Bfs_queue.t
      }
  end

  let create kind ~initial_pose ~manually_frozen_vertices ~alternative_offsets =
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
    { kind
    ; pose = initial_pose
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
  let pick_next_vertex ~vertices ~vertices_left ~vertex_edges ~frozen_vertices =
    let sorted_vertices =
      List.map (Set.to_list vertices_left) ~f:(fun a ->
          let edges = Map.find vertex_edges a |> Option.value ~default:[] in
          let connected_vertices = List.map edges ~f:snd in
          let frozen_connections =
            List.filter connected_vertices ~f:(Set.mem frozen_vertices)
          in
          a, frozen_connections, List.length frozen_connections)
      |> List.sort ~compare:(fun (_, _, num_conns1) (_, _, num_conns2) ->
             Int.descending num_conns1 num_conns2)
    in
    let _, _, num_conns = sorted_vertices |> List.hd_exn in
    let vertices_with_max_conns =
      List.filter sorted_vertices ~f:(fun (_, _, n) -> n = num_conns)
    in
    (* Pick the furthest away vertex with maximum number of connections
     to frozen vertices.  *)
    let next_vertex, conns, _ =
      List.map vertices_with_max_conns ~f:(fun (vx, conns, _) ->
          let max_distance_to_frozen_nodes =
            let vx_pt = Map.find_exn vertices vx in
            List.map conns ~f:(fun vy ->
                Point.sq_distance vx_pt (Map.find_exn vertices vy))
            |> List.fold_left ~init:Bignum.zero ~f:Bignum.max
          in
          vx, conns, max_distance_to_frozen_nodes)
      |> List.sort ~compare:(fun (_, _, max_dist1) (_, _, max_dist2) ->
             Bignum.descending max_dist1 max_dist2)
      |> List.hd_exn
    in
    (* printf
     *   "Solver trying to fix vertex %d with %d frozen connections\n%!"
     *   next_vertex
     *   num_conns; *)
    next_vertex, conns, num_conns
  ;;

  module Vertex_and_vertex_and_position = struct
    module T = struct
      type t = int * int * Point.t [@@deriving compare, hash, sexp]
    end

    include T
    include Hashable.Make (T)
  end

  let alternative_positions_from_at_position_to =
    let cache = Vertex_and_vertex_and_position.Table.create () in
    let hits = ref 0 in
    let misses = ref 0 in
    fun ~alternative_offsets ~from_ ~to_ ~position ->
      let key = from_, to_, (position : Point.t) in
      match Hashtbl.find cache key with
      | Some res ->
        incr hits;
        (* printf "Cache hit! hit: %d; miss: %d\n%!" !hits !misses; *)
        res
      | None ->
        incr misses;
        let res =
          let aos = Alternative_offsets.find alternative_offsets from_ to_ in
          List.map aos ~f:(fun (dx, dy) ->
              Point.create ~x:Bignum.(position.x + dx) ~y:Bignum.(position.y + dy))
          |> Point.Set.of_list
        in
        Hashtbl.set cache ~key ~data:res;
        res
  ;;

  let find_alternative_positions_for_vertex
      vertex
      ~connections_to_frozen_vertices
      ~alternative_offsets
      ~pose
    =
    let alternative_positions_per_connected_node =
      List.map connections_to_frozen_vertices ~f:(fun connected_frozen_vertex ->
          alternative_positions_from_at_position_to
            ~alternative_offsets
            ~from_:connected_frozen_vertex
            ~position:(Map.find_exn (Pose.vertices pose) connected_frozen_vertex)
            ~to_:vertex)
    in
    let res =
      match alternative_positions_per_connected_node with
      | [] -> Point.Set.empty
      | [ aps ] -> aps
      | aps :: apss -> List.fold_left apss ~init:aps ~f:(fun acc aps -> Set.inter acc aps)
    in
    Set.to_list res |> Pose.sort_by_min_distance_to_hole_vertices pose
  ;;

  let create_dfs_stack_frame t =
    let vertex, conns, _num_conns =
      pick_next_vertex
        ~vertices:(Pose.vertices t.pose)
        ~vertices_left:t.vertices_left
        ~vertex_edges:t.vertex_edges
        ~frozen_vertices:t.frozen_vertices
    in
    (* printf
     *   "Solver trying to fix vertex %d with %d frozen connections\n%!"
     *   vertex
     *   num_conns; *)
    let alternative_positions =
      find_alternative_positions_for_vertex
        vertex
        ~connections_to_frozen_vertices:conns
        ~alternative_offsets:t.alternative_offsets
        ~pose:t.pose
    in
    (* printf "Solver found %d alternative positions\n%!" (List.length alternative_positions); *)
    Stack_frame.{ solver_t = t; vertex; alternative_positions; queue = Bfs_queue.empty }
  ;;

  let create_initial_dfs_stack t = [ create_dfs_stack_frame t ]

  let all_edges_to_frozen_inside_hole vertex ~frozen_vertices ~vertex_edges ~pose =
    let edges = Map.find vertex_edges vertex |> Option.value ~default:[] in
    let connected_vertices = List.map edges ~f:snd in
    let frozen_connections =
      List.filter connected_vertices ~f:(Set.mem frozen_vertices)
    in
    List.for_all frozen_connections ~f:(fun frozen_vertex ->
        Pose.edge_inside_hole pose (vertex, frozen_vertex))
  ;;

  let run_dfs ~work_to_do:work_to_do0 ~stack:stack0 =
    (* printf "\n\nSolver.incremental_run ~stack:%d\n%!" (List.length stack0); *)
    let rec incremental_loop ~work_to_do ~stack =
      (* printf "incremental_loop ~stack:%d\n%!" (List.length stack); *)
      if work_to_do <= 0
      then `Todo stack
      else (
        match (stack : Stack_frame.t list) with
        | [] -> failwith "Solver.incremental_run: empty stack"
        | { solver_t; vertex; alternative_positions; queue = _ } :: rest_stack ->
          if Set.is_empty solver_t.vertices_left
          then `Done stack
          else (
            match
              alternative_position_loop
                solver_t
                alternative_positions
                ~work_to_do
                ~vertex
                ~stack_excluding_self:rest_stack
            with
            | `Done stack -> `Done stack
            | `Todo stack -> `Todo stack
            | `Failed work_to_do ->
              (* printf "incremental_loop failed at depth %d\n%!" (1 + List.length rest_stack); *)
              `Failed work_to_do))
    and alternative_position_loop
        (t : t)
        ~work_to_do
        ~vertex
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
          (Stack_frame.
             { solver_t = t; vertex; alternative_positions; queue = Bfs_queue.empty }
           :: stack_excluding_self)
      else (
        match alternative_positions with
        | [] -> `Failed work_to_do
        | pos :: rest_aps ->
          let pose = Pose.move t.pose vertex ~to_:pos in
          if all_edges_to_frozen_inside_hole
               vertex
               ~pose
               ~frozen_vertices:t.frozen_vertices
               ~vertex_edges:t.vertex_edges
          then (
            let frozen_vertices = Set.add t.frozen_vertices vertex in
            let vertices_left = Set.remove t.vertices_left vertex in
            let updated_t = { t with pose; frozen_vertices; vertices_left } in
            let cur_frame =
              Stack_frame.
                { solver_t = t
                ; vertex
                ; alternative_positions = rest_aps
                ; queue = Bfs_queue.empty
                }
            in
            if Set.is_empty vertices_left
            then
              `Done
                (Stack_frame.
                   { solver_t = updated_t
                   ; vertex = ~-1
                   ; alternative_positions = []
                   ; queue = Bfs_queue.empty
                   }
                 :: cur_frame :: stack_excluding_self)
            else (
              let updated_stack =
                create_dfs_stack_frame updated_t :: cur_frame :: stack_excluding_self
              in
              match
                incremental_loop ~work_to_do:(work_to_do - 1) ~stack:updated_stack
              with
              | `Done stack -> `Done stack
              | `Todo stack -> `Todo stack
              | `Failed work_to_do ->
                (* printf "trying alternatives after failure\n%!"; *)
                alternative_position_loop
                  t
                  rest_aps
                  ~vertex
                  ~work_to_do
                  ~stack_excluding_self))
          else
            (* The new vertex has edges that go outside the hole. *)
            (* printf "trying alternatives edge overlap\n%!"; *)
            alternative_position_loop t rest_aps ~vertex ~work_to_do ~stack_excluding_self)
    in
    incremental_loop ~work_to_do:work_to_do0 ~stack:stack0
  ;;

  let create_bfs_stack_frame t vertex ~queue =
    let connections_to_frozen_vertices, other_conns =
      let edges = Map.find t.vertex_edges vertex |> Option.value ~default:[] in
      let connected_vertices = List.map edges ~f:snd in
      List.partition_tf connected_vertices ~f:(Set.mem t.frozen_vertices)
    in
    let alternative_positions =
      find_alternative_positions_for_vertex
        vertex
        ~connections_to_frozen_vertices
        ~alternative_offsets:t.alternative_offsets
        ~pose:t.pose
    in
    let queue =
      List.fold_left other_conns ~init:queue ~f:(fun queue other_vertex ->
          if Bfs_queue.mem queue other_vertex || Set.mem t.frozen_vertices other_vertex
          then queue
          else Bfs_queue.enqueue queue other_vertex)
    in
    Stack_frame.{ solver_t = t; vertex; alternative_positions; queue }
  ;;

  let create_initial_bfs_stack t =
    let vertices_adjacent_to_manually_frozen =
      Map.keys (Pose.vertices t.pose)
      |> List.filter ~f:(fun vertex ->
             (not (Set.mem t.manually_frozen_vertices vertex))
             && List.exists
                  (Map.find_exn t.vertex_edges vertex)
                  ~f:(fun (_, other_vertex) ->
                    Set.mem t.manually_frozen_vertices other_vertex))
    in
    let queue =
      List.fold_left
        vertices_adjacent_to_manually_frozen
        ~init:Bfs_queue.empty
        ~f:(fun queue vertex -> Bfs_queue.enqueue queue vertex)
    in
    let first_vertex, queue =
      Option.value_exn
        (Bfs_queue.dequeue_with_most_connections_to_frozen
           queue
           ~frozen_vertices:t.manually_frozen_vertices
           ~vertex_edges:t.vertex_edges)
    in
    (* printf
     *   !"Selected first vertex %d at %{sexp#hum: Point.t}\n%!"
     *   first_vertex
     *   (Map.find_exn (Pose.vertices t.pose) first_vertex); *)
    [ create_bfs_stack_frame t first_vertex ~queue ]
  ;;

  let run_bfs ~work_to_do:work_to_do0 ~stack:stack0 =
    (* printf "\n\nSolver.incremental_bfs_run ~stack:%d\n%!" (List.length stack0); *)
    let rec incremental_loop ~work_to_do ~stack =
      (* printf "incremental_loop ~stack:%d\n%!" (List.length stack); *)
      if work_to_do <= 0
      then `Todo stack
      else (
        match (stack : Stack_frame.t list) with
        | [] -> failwith "Solver.incremental_run: empty stack"
        | { solver_t; vertex; alternative_positions; queue } :: rest_stack ->
          if Set.is_empty solver_t.vertices_left
          then `Done stack
          else (
            match
              alternative_position_loop
                solver_t
                alternative_positions
                ~work_to_do
                ~vertex
                ~stack_excluding_self:rest_stack
                ~queue
            with
            | `Done stack -> `Done stack
            | `Todo stack -> `Todo stack
            | `Failed work_to_do ->
              (* printf "incremental_loop failed at depth %d\n%!" (1 + List.length rest_stack); *)
              `Failed work_to_do))
    and alternative_position_loop
        (t : t)
        alternative_positions
        ~work_to_do
        ~vertex
        ~stack_excluding_self
        ~queue
      =
      (* let stack_depth = List.length stack_excluding_self + 1 in
       * printf "alternative_position_loop\n%!";
       * List.iteri
       *   (List.rev stack_excluding_self)
       *   ~f:(fun i Stack_frame.{ alternative_positions; queue; _ } ->
       *     printf
       *       "  - stack depth %d: alternative_positions=%d; queue=%d\n%!"
       *       (i + 1)
       *       (List.length alternative_positions)
       *       (Fqueue.length queue));
       * printf
       *   "  - stack depth %d: alternative_positions=%d; queue=%d\n%!"
       *   stack_depth
       *   (List.length alternative_positions)
       *   (Fqueue.length queue); *)
      if work_to_do <= 0
      then
        `Todo
          (Stack_frame.{ solver_t = t; vertex; alternative_positions; queue }
           :: stack_excluding_self)
      else (
        match alternative_positions with
        | [] -> `Failed work_to_do
        | pos :: rest_aps ->
          let pose = Pose.move t.pose vertex ~to_:pos in
          if all_edges_to_frozen_inside_hole
               vertex
               ~pose
               ~frozen_vertices:t.frozen_vertices
               ~vertex_edges:t.vertex_edges
          then (
            let frozen_vertices = Set.add t.frozen_vertices vertex in
            let vertices_left = Set.remove t.vertices_left vertex in
            let updated_t = { t with pose; frozen_vertices; vertices_left } in
            let cur_frame =
              Stack_frame.
                { solver_t = t; vertex; alternative_positions = rest_aps; queue }
            in
            if Set.is_empty vertices_left
            then
              `Done
                (Stack_frame.
                   { solver_t = updated_t
                   ; vertex = ~-1
                   ; alternative_positions = []
                   ; queue = Bfs_queue.empty
                   }
                 :: cur_frame :: stack_excluding_self)
            else (
              match
                Bfs_queue.dequeue_with_most_connections_to_frozen
                  queue
                  ~frozen_vertices
                  ~vertex_edges:t.vertex_edges
              with
              | None -> failwith "Still had vertices to place but work queue was empty"
              | Some (queued_vertex, updated_queue) ->
                let updated_stack =
                  create_bfs_stack_frame updated_t queued_vertex ~queue:updated_queue
                  :: cur_frame :: stack_excluding_self
                in
                (match
                   incremental_loop ~work_to_do:(work_to_do - 1) ~stack:updated_stack
                 with
                | `Done stack -> `Done stack
                | `Todo stack -> `Todo stack
                | `Failed work_to_do ->
                  (* printf "trying alternatives after failure\n%!"; *)
                  alternative_position_loop
                    t
                    rest_aps
                    ~vertex
                    ~work_to_do
                    ~stack_excluding_self
                    ~queue)))
          else
            (* The new vertex has edges that go outside the hole, so try
             the next alternative position.. *)
            (* printf "trying alternatives edge overlap\n%!"; *)
            alternative_position_loop
              t
              rest_aps
              ~vertex
              ~work_to_do
              ~stack_excluding_self
              ~queue)
    in
    incremental_loop ~stack:stack0 ~work_to_do:work_to_do0
  ;;

  let top_create kind ~initial_pose ~manually_frozen_vertices ~alternative_offsets =
    let t = create kind ~initial_pose ~manually_frozen_vertices ~alternative_offsets in
    match kind with
    | `Bfs -> create_initial_bfs_stack t
    | `Dfs -> create_initial_dfs_stack t
  ;;

  let top_run (stack : Stack_frame.t list) ~work_to_do =
    match stack with
    | [] -> failwith "Incremental solver stack empty"
    | frame :: _ as stack ->
      let res =
        match frame.solver_t.kind with
        | `Dfs -> run_dfs ~work_to_do ~stack
        | `Bfs -> run_bfs ~work_to_do ~stack
      in
      (match res with
      | `Done stack -> `Done stack
      | `Todo stack -> `Todo stack
      | `Failed _ ->
        (match stack with
        | [] | [ _ ] -> `Failed
        | _ :: rest_stack -> `Todo rest_stack))
  ;;

  let pose (stack : Stack_frame.t list) = (List.hd_exn stack).solver_t.pose
end

module Complete_hole_fitter = struct
  type t =
    { pose : Pose.t
    ; alternative_offsets : Alternative_offsets.t
    ; hole_assignment : int array
          (** for each hole point, which vertex is assigned to it *)
    ; num_vertices : int
    ; inner : Incremental.Stack_frame.t list
    }

  let next_hole_assignment hole_assignment ~num_vertices =
    if num_vertices < Array.length hole_assignment
    then failwith "Too few figure vertices to assign to hole vertices";
    let hole_assignment = Array.copy hole_assignment in
    let find_unused_vertex ~after ~used =
      let rec unused_loop to_try =
        if to_try = after then failwith "Impossible given the assert above";
        if Set.mem used to_try
        then unused_loop ((1 + to_try) mod num_vertices)
        else to_try
      in
      unused_loop ((1 + after) mod num_vertices)
    in
    let rec loop idx ~used =
      if idx = Array.length hole_assignment - 1
      then (
        let x = find_unused_vertex ~after:hole_assignment.(idx) ~used in
        let wrapped_around = x < hole_assignment.(idx) in
        hole_assignment.(idx) <- x;
        wrapped_around)
      else if loop (idx + 1) ~used:(Set.add used hole_assignment.(idx))
      then (
        (* We wrapped around at the next step, so increment here
             now. *)
        let x = find_unused_vertex ~after:hole_assignment.(idx) ~used in
        let wrapped_around = x < hole_assignment.(idx) in
        hole_assignment.(idx) <- x;
        wrapped_around)
      else false
    in
    let (_ : bool) = loop 0 ~used:Int.Set.empty in
    hole_assignment
  ;;

  let create ~(initial_pose : Pose.t) ~alternative_offsets =
    let num_vertices = Map.length (Pose.vertices initial_pose) in
    let hole_assignment =
      let buf = Array.create 0 ~len:(List.length (Pose.problem initial_pose).hole) in
      for i = 0 to Array.length buf - 1 do
        buf.(i) <- i
      done;
      buf.(Array.length buf - 1) <- buf.(Array.length buf - 1) - 1;
      buf
    in
    (* printf !"Hole assignment: %{sexp#hum: int array}\n%!" hole_assignment; *)
    { pose = initial_pose
    ; alternative_offsets
    ; hole_assignment
    ; num_vertices
    ; inner = []
    }
  ;;

  let edges_between_hole_assigned_vertices_are_valid vertices ~pose =
    let vertices = Int.Set.of_list vertices in
    let edges_between_hole_assigned_vertices =
      List.filter (Pose.problem pose).figure_edges ~f:(fun (a, b) ->
          Set.mem vertices a && Set.mem vertices b)
    in
    List.for_all edges_between_hole_assigned_vertices ~f:(fun edge ->
        Option.is_none (Pose.edge_invalid pose edge) && Pose.edge_inside_hole pose edge)
  ;;

  let assign_holes t ~work_to_do =
    let rec find_valid_hole_assignment t ~work_to_do =
      if work_to_do <= 0
      then `Todo t
      else (
        let t =
          { t with
            hole_assignment =
              next_hole_assignment t.hole_assignment ~num_vertices:t.num_vertices
          }
        in
        printf !"Searching hole assignment: %{sexp#hum: int array}\n%!" t.hole_assignment;
        let hole_assigned_vertices =
          List.zip_exn (Array.to_list t.hole_assignment) (Pose.problem t.pose).hole
        in
        let t =
          List.fold_left hole_assigned_vertices ~init:t ~f:(fun t (vx, hole_position) ->
              { t with pose = Pose.move t.pose vx ~to_:hole_position })
        in
        if edges_between_hole_assigned_vertices_are_valid
             (List.map hole_assigned_vertices ~f:fst)
             ~pose:t.pose
        then (
          printf !"Found hole assignment: %{sexp#hum: int array}\n%!" t.hole_assignment;
          `Found t)
        else find_valid_hole_assignment t ~work_to_do:(work_to_do - 1))
    in
    match find_valid_hole_assignment t ~work_to_do with
    | `Failed -> `Failed
    | `Todo t -> `Todo t
    | `Found t ->
      let incremental =
        Incremental.top_create
          `Dfs
          ~initial_pose:t.pose
          ~manually_frozen_vertices:(Array.to_list t.hole_assignment |> Int.Set.of_list)
          ~alternative_offsets:t.alternative_offsets
      in
      `Todo { t with inner = incremental }
  ;;

  let run t ~work_to_do =
    match t.inner with
    | [] -> assign_holes t ~work_to_do
    | incremental ->
      (match Incremental.top_run incremental ~work_to_do with
      | `Done incremental ->
        `Done { t with inner = []; pose = Incremental.pose incremental }
      | `Todo incremental ->
        `Todo { t with inner = incremental; pose = Incremental.pose incremental }
      | `Failed -> assign_holes { t with inner = [] } ~work_to_do)
  ;;
end

type t =
  | Incremental of Incremental.Stack_frame.t list
  | Complete_hole_fitter of Complete_hole_fitter.t

let create (kind : Kind.t) ~initial_pose ~manually_frozen_vertices ~alternative_offsets =
  match kind with
  | Dfs ->
    Incremental
      (Incremental.top_create
         `Dfs
         ~initial_pose
         ~manually_frozen_vertices
         ~alternative_offsets)
  | Bfs ->
    Incremental
      (Incremental.top_create
         `Bfs
         ~initial_pose
         ~manually_frozen_vertices
         ~alternative_offsets)
  | Chf ->
    let solver = Complete_hole_fitter.create ~initial_pose ~alternative_offsets in
    Complete_hole_fitter solver
;;

let run t ~work_to_do =
  match t with
  | Incremental incremental ->
    (match Incremental.top_run incremental ~work_to_do with
    | `Done incremental -> `Done (Incremental incremental)
    | `Todo incremental -> `Todo (Incremental incremental)
    | `Failed -> `Failed)
  | Complete_hole_fitter chf ->
    (match Complete_hole_fitter.run chf ~work_to_do with
    | `Done chf -> `Done (Complete_hole_fitter chf)
    | `Todo chf -> `Todo (Complete_hole_fitter chf)
    | `Failed -> `Failed)
;;

let pose t =
  match t with
  | Incremental (frame :: _) -> frame.solver_t.pose
  | Incremental [] -> failwith "Incremental solver stack empty"
  | Complete_hole_fitter chf -> chf.pose
;;
