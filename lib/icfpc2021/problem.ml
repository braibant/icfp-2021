open! Core

type t =
  { hole : Point.t list
  ; figure_edges : Point.t list
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

let json_as_bignum_pair_list json ~what =
  let module J = Tiny_json.Json in
  json
  |> J.as_list
  |> List.map ~f:(fun j ->
         match J.as_list j with
         | [ x; y ] -> Point.create ~x:(json_as_bigint x) ~y:(json_as_bigint y)
         | _ -> failwithf "Parsing %s: expected list of pairs" what ())
;;

let load_exn ~filename =
  let module J = Tiny_json.Json in
  let json = J.parse_ch (In_channel.create filename) in
  let epsilon = json |> J.getf "epsilon" |> json_as_bigint in
  let hole = json |> J.getf "hole" |> json_as_bignum_pair_list ~what:"hole" in
  let figure_edges =
    json
    |> J.getf "figure"
    |> J.getf "edges"
    |> json_as_bignum_pair_list ~what:"figure edges"
  in
  let figure_vertices =
    json
    |> J.getf "figure"
    |> J.getf "vertices"
    |> json_as_bignum_pair_list ~what:"figure vertices"
  in
  { hole; figure_edges; figure_vertices; epsilon }
;;
