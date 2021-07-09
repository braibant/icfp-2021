open! Core

type t =
  { hole : Point.t list
  ; figure_edges : (int * int) list
  ; figure_vertices : Point.t list
  ; epsilon : Bignum.t
  }
[@@deriving sexp]

let json_as_bigint x =
  match (x : Tiny_json.Json.t) with
  | String str | Number str -> Bignum.of_string str
  | Object _ | Array _ | Bool _ | Null ->
    failwith "json_as_bigint: Expected string or number"
;;

let json_as_point_list json ~what =
  let module J = Tiny_json.Json in
  json
  |> J.as_list
  |> List.map ~f:(fun j ->
         match J.as_list j with
         | [ x; y ] -> Point.create ~x:(json_as_bigint x) ~y:(json_as_bigint y)
         | _ -> failwithf "Parsing %s: expected list of pairs" what ())
;;

let json_as_int_pair_list json ~what =
  let module J = Tiny_json.Json in
  json
  |> J.as_list
  |> List.map ~f:(fun j ->
         match J.as_list j with
         | [ x; y ] -> J.as_int x, J.as_int y
         | _ -> failwithf "Parsing %s: expected list of pairs" what ())
;;

let load_exn ~filename =
  let module J = Tiny_json.Json in
  let json = J.parse_ch (In_channel.create filename) in
  let epsilon = json |> J.getf "epsilon" |> json_as_bigint in
  let hole = json |> J.getf "hole" |> json_as_point_list ~what:"hole" in
  let figure_edges =
    json
    |> J.getf "figure"
    |> J.getf "edges"
    |> json_as_int_pair_list ~what:"figure edges"
  in
  let figure_vertices =
    json
    |> J.getf "figure"
    |> J.getf "vertices"
    |> json_as_point_list ~what:"figure vertices"
  in
  { hole; figure_edges; figure_vertices; epsilon }
;;

let max_xy t =
  let fold_to_max points (max_x, max_y) =
    List.fold_left points ~init:(max_x, max_y) ~f:(fun (max_x, max_y) Point.{ x; y } ->
        Bignum.max max_x x, Bignum.max max_y y)
  in
  (Bignum.zero, Bignum.zero) |> fold_to_max t.hole |> fold_to_max t.figure_vertices
;;

let to_string_hum t =
  let max_x, max_y = max_xy t in
  sprintf
    !"Problem with:\n\
     \  - hole of %d points,\n\
     \  - figure of %d vertices and %d edges,\n\
     \  - epsilon: %{Bignum#hum},\n\
     \  - max x, y: %{Bignum#hum}x%{Bignum#hum}"
    (List.length t.hole)
    (List.length t.figure_vertices)
    (List.length t.figure_edges)
    t.epsilon
    max_x
    max_y
;;

let score t =
  let vertices = List.length t.figure_vertices in
  let edges = List.length t.figure_edges in
  let hole = List.length t.hole in
  Float.(1000.0 * log (of_int vertices * of_int edges * of_int hole))
;;
