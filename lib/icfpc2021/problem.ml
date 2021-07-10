open! Core

type t =
  { hole : Point.t list
  ; figure_edges : Edge.t list
  ; figure_vertices : Point.t list
  ; epsilon : Bignum.t
  }
[@@deriving sexp]

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
  let epsilon = json |> J.getf "epsilon" |> Common.json_as_bigint in
  let hole = json |> J.getf "hole" |> Common.json_as_point_list ~what:"hole" in
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
    |> Common.json_as_point_list ~what:"figure vertices"
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

let log2 x = log x /. log 2.

let score t =
  let vertices = List.length t.figure_vertices in
  let edges = List.length t.figure_edges in
  let hole = List.length t.hole in
  Float.(1000.0 * log2 (of_int vertices * of_int edges * of_int hole))
;;
