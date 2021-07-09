open! Core

type t =
  { problem : Problem.t
  ; vertices : Point.t list
  }
[@@deriving fields]

let create problem = { problem; vertices = problem.figure_vertices }
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

let move t idx ~to_ =
  let vertices = Array.of_list t.vertices in
  vertices.(idx) <- to_;
  { t with vertices = Array.to_list vertices }
;;
