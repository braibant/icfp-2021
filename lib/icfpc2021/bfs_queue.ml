open! Core

type t =
  { in_queue : Int.Set.t
  ; queue : int list
  }

let empty = { in_queue = Int.Set.empty; queue = [] }
let enqueue t vx = { in_queue = Set.add t.in_queue vx; queue = vx :: t.queue }
let mem t vx = Set.mem t.in_queue vx

let dequeue_with_most_connections_to_frozen t ~frozen_vertices ~vertex_edges =
  match t.queue with
  | [] -> None
  | vertices ->
    let selected, _ =
      List.map vertices ~f:(fun vx ->
          let connected_vertices = Map.find_exn vertex_edges vx |> List.map ~f:snd in
          vx, List.count connected_vertices ~f:(Set.mem frozen_vertices))
      |> List.sort ~compare:(fun (_, num_frozen_conn1) (_, num_frozen_conn2) ->
             Int.descending num_frozen_conn1 num_frozen_conn2)
      |> List.hd_exn
    in
    let queue = List.filter t.queue ~f:(fun vx -> vx <> selected) in
    let t = { in_queue = Set.remove t.in_queue selected; queue } in
    Some (selected, t)
;;
