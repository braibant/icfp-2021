open! Core
module Int_int = Tuple.Comparable (Int) (Int)

type t =
  { problem : Problem.t
  ; vertices : Point.t list
  ; orig_lengths : Bignum.t Int_int.Map.t
  }
[@@deriving fields]

let current_point t vertex = List.nth_exn t.vertices vertex

let create problem =
  { problem
  ; vertices = problem.figure_vertices
  ; orig_lengths =
      List.map problem.figure_edges ~f:(fun (from_, to_) ->
          let from_p = List.nth_exn problem.figure_vertices from_ in
          let to_p = List.nth_exn problem.figure_vertices from_ in
          (from_, to_), Point.distance from_p to_p)
      |> Int_int.Map.of_alist_exn
  }
;;

let set_vertices t vertices = { t with vertices }

let load_exn ~problem ~filename =
  let module J = Tiny_json.Json in
  let json = J.parse_ch (In_channel.create filename) in
  let vertices =
    json
    |> J.getf "figure"
    |> J.getf "vertices"
    |> Common.json_as_point_list ~what:"pose vertices"
  in
  set_vertices (create problem) vertices
;;

let could_deform t (v1, v1_p) (v2, v2_p) =
  let orig_length = Map.find_exn t.orig_lengths (v1, v2) in
  let curr_length = Point.distance v1_p v2_p in
  Bignum.(abs ((curr_length / orig_length) - one) <= t.problem.epsilon / million)
;;

let move t vertex ~to_:point =
  let edges =
    (* all edges that start or end in point [vertex] *)
    List.filter t.problem.figure_edges ~f:(fun (v1, v2) -> v1 = vertex || v2 = vertex)
  in
  let cur_p i =
    (* current Point of vertex [i], taking into account requested move *)
    if i = vertex then point else current_point t vertex
  in
  let possible =
    List.for_all edges ~f:(fun (from_, to_) ->
        let from_p = cur_p from_ in
        let to_p = cur_p to_ in
        could_deform t (from_, from_p) (to_, to_p))
  in
  if possible
  then
    { t with
      vertices = List.mapi t.vertices ~f:(fun i p -> if i = vertex then point else p)
    }
  else t
;;
