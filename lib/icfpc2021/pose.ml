open! Core
module Int_int = Tuple.Comparable (Int) (Int)

type t =
  { problem : Problem.t
  ; vertices : Point.t list
  ; orig_lengths : Bignum.t Int_int.Map.t
  }
[@@deriving fields]

let create problem =
  { problem
  ; vertices = problem.figure_vertices
  ; orig_lengths =
      List.map problem.figure_edges ~f:(fun (from_, to_) ->
          let from_p = List.nth_exn problem.figure_vertices from_ in
          let to_p = List.nth_exn problem.figure_vertices to_ in
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

let could_deform t edge curr_length =
  let orig_length = Map.find_exn t.orig_lengths edge in
  let off_from_one = Bignum.(abs ((curr_length / orig_length) - one)) in
  let tolerance = Bignum.(t.problem.epsilon / million) in
  let res = Bignum.(off_from_one <= tolerance) in
  eprintf
    !"%{sexp:int*int}: %{Bignum#hum} -> %{Bignum#hum}: OFF BY %{Bignum#hum}, TOL \
      %{Bignum#hum} => %b\n\
      %!"
    edge
    orig_length
    curr_length
    off_from_one
    tolerance
    res;
  res
;;

let move t vertex ~to_:point =
  let edges =
    (* all edges that start or end in point [vertex] *)
    List.filter t.problem.figure_edges ~f:(fun (v1, v2) -> v1 = vertex || v2 = vertex)
  in
  let cur_p i =
    (* current Point of vertex [i], taking into account requested move *)
    if i = vertex then point else List.nth_exn t.vertices i
  in
  let possible =
    List.for_all edges ~f:(fun edge ->
        let from_, to_ = edge in
        let new_length = Point.distance (cur_p from_) (cur_p to_) in
        could_deform t edge new_length)
  in
  if possible
  then
    { t with
      vertices = List.mapi t.vertices ~f:(fun i p -> if i = vertex then point else p)
    }
  else t
;;
