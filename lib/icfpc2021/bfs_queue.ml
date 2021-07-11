open! Core

type t =
  { in_queue : Int.Set.t
  ; queue : int list
  }

let empty = { in_queue = Int.Set.empty; queue = [] }
let enqueue t vx = { in_queue = Set.add t.in_queue vx; queue = vx :: t.queue }
let mem t vx = Set.mem t.in_queue vx

let dequeue_with_most_connections_to_frozen t ~vertices ~frozen_vertices ~vertex_edges =
  match t.queue with
  | [] -> None
  | queue_vertices ->
    let sorted_vertices =
      List.map queue_vertices ~f:(fun vx ->
          let connected_vertices = Map.find_exn vertex_edges vx |> List.map ~f:snd in
          let frozen_connections =
            List.filter connected_vertices ~f:(Set.mem frozen_vertices)
          in
          vx, frozen_connections, List.length frozen_connections)
      |> List.sort ~compare:(fun (_, _, num_frozen_conn1) (_, _, num_frozen_conn2) ->
             Int.descending num_frozen_conn1 num_frozen_conn2)
    in
    let _, _, num_conns = sorted_vertices |> List.hd_exn in
    let vertices_with_max_conns =
      List.filter sorted_vertices ~f:(fun (_, _, n) -> n = num_conns)
    in
    (* Pick the furthest away vertex with maximum number of connections
     to frozen vertices.  *)
    let selected, _, _ =
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
    let queue = List.filter t.queue ~f:(fun vx -> vx <> selected) in
    let t = { in_queue = Set.remove t.in_queue selected; queue } in
    Some (selected, t)
;;
