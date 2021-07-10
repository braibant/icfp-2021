open! Core

type t =
  { in_queue : Int.Set.t
  ; queue : int Fqueue.t
  }

let empty = { in_queue = Int.Set.empty; queue = Fqueue.empty }
let enqueue t vx = { in_queue = Set.add t.in_queue vx; queue = Fqueue.enqueue t.queue vx }
let mem t vx = Set.mem t.in_queue vx

(* CR scvalex: Actually implement the extra logic. *)
let dequeue_with_most_connections_to_frozen t ~frozen_vertices:_ ~vertex_edges:_ =
  match Fqueue.dequeue t.queue with
  | None -> None
  | Some (vx, queue) ->
    let t = { in_queue = Set.remove t.in_queue vx; queue } in
    Some (vx, t)
;;
