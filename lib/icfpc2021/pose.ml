open! Core
module Int_int = Tuple.Comparable (Int) (Int)

type t =
  { problem : Problem.t
  ; vertices : Point.t Int.Map.t
  ; orig_lengths : Bignum.t Int_int.Map.t
  ; hole_polygon : Polygon.t
  }
[@@deriving fields]

let vertex t idx = Map.find_exn t.vertices idx

let create problem =
  { problem
  ; vertices =
      List.mapi problem.figure_vertices ~f:(fun i p -> i, p) |> Int.Map.of_alist_exn
  ; orig_lengths =
      List.map problem.figure_edges ~f:(fun (from_, to_) ->
          let from_p = List.nth_exn problem.figure_vertices from_ in
          let to_p = List.nth_exn problem.figure_vertices to_ in
          (from_, to_), Point.distance from_p to_p)
      |> Int_int.Map.of_alist_exn
  ; hole_polygon = Polygon.of_vertices problem.hole
  }
;;

let set_vertices t vertices =
  { t with vertices = List.mapi vertices ~f:(fun i p -> i, p) |> Int.Map.of_alist_exn }
;;

let load_exn ~problem ~filename =
  let module J = Tiny_json.Json in
  let json = J.parse_ch (In_channel.create filename) in
  let vertices =
    json |> J.getf "vertices" |> Common.json_as_point_list ~what:"pose vertices"
  in
  set_vertices (create problem) vertices
;;

let save_exn t ~filename =
  let module J = Tiny_json.Json in
  let json =
    J.Object
      [ ( "vertices"
        , J.Array
            (Int.Map.to_alist t.vertices
            |> List.map ~f:snd
            |> List.map ~f:(fun (point : Point.t) ->
                   J.Array
                     [ J.Number (Bignum.to_string_hum point.x)
                     ; J.Number (Bignum.to_string_hum point.y)
                     ])) )
      ]
  in
  let out = Out_channel.create filename in
  let formatter = Format.formatter_of_out_channel out in
  J.format formatter json;
  Format.pp_print_flush formatter ();
  Out_channel.close out
;;

let deformation_badness t edge curr_length =
  let orig_length = Map.find_exn t.orig_lengths edge in
  let off_from_one = Bignum.(abs ((curr_length / orig_length) - one)) in
  let tolerance = Bignum.(t.problem.epsilon / million) in
  let could_not = Bignum.(off_from_one > tolerance) in
  (* eprintf
   *   !"%{sexp:int*int}: %{Bignum#hum} -> %{Bignum#hum}: OFF BY %{Bignum#hum}, TOL \
   *     %{Bignum#hum} => %b\n\
   *     %!"
   *   edge
   *   orig_length
   *   curr_length
   *   off_from_one
   *   tolerance
   *   could_not; *)
  if could_not then Some off_from_one else None
;;

let min_max_length_sq_for_edge t edge =
  let orig_length = Map.find_exn t.orig_lengths edge in
  let tolerance = Bignum.(t.problem.epsilon / million) in
  Bignum.(orig_length * (one - tolerance), orig_length * (one + tolerance))
;;

let move t vertex ~to_:point =
  { t with vertices = Map.set t.vertices ~key:vertex ~data:point }
;;

let invalid_edges t =
  List.filter_map t.problem.figure_edges ~f:(fun edge ->
      let from_, to_ = edge in
      let new_length =
        Point.distance (Map.find_exn t.vertices from_) (Map.find_exn t.vertices to_)
      in
      Option.map (deformation_badness t edge new_length) ~f:(fun badness -> edge, badness))
;;

let shift t (dx, dy) =
  { t with vertices = Map.map t.vertices ~f:(fun p -> Point.shift p (dx, dy)) }
;;

let edge_inside_hole t (a, b) =
  let pa = Map.find_exn t.vertices a in
  let pb = Map.find_exn t.vertices b in
  let the_edge = Segment.create pa pb in
  Polygon.contains t.hole_polygon pa
  && Polygon.contains t.hole_polygon pb
  && not (Polygon.intersect_segment t.hole_polygon the_edge)
;;

let reflect_vertical t =
  let max_x, _max_y = Problem.max_xy t.problem in
  { t with
    vertices =
      Map.map t.vertices ~f:(fun p -> Point.create ~x:Bignum.(max_x - p.x) ~y:p.y)
  }
;;

let transpose t =
  { t with vertices = Map.map t.vertices ~f:(fun p -> Point.create ~x:p.y ~y:p.x) }
;;
