open! Core
module Int_int = Tuple.Comparable (Int) (Int)

type t =
  { problem : Problem.t
  ; vertices : Point.t Int.Map.t
  ; orig_lengths : Bignum.t Int_int.Map.t
  ; hole_polygon : Polygon.t
  ; bonuses : Pose_bonus.t list
  }
[@@deriving fields]

let vertex t idx = Map.find_exn t.vertices idx

let segment t (a, b) =
  let pa = vertex t a in
  let pb = vertex t b in
  Segment.create pa pb
;;

(* CR tbraibant: Maybe, actually normalize orig_lengths to have edges in normal form
   (with vertex 1 < vertex 2) *)
let original_length t (a, b) =
  if Map.mem t.orig_lengths (a, b)
  then Map.find_exn t.orig_lengths (a, b)
  else Map.find_exn t.orig_lengths (b, a)
;;

let create problem =
  { problem
  ; vertices =
      List.mapi problem.figure_vertices ~f:(fun i p -> i, p) |> Int.Map.of_alist_exn
  ; orig_lengths =
      List.map problem.figure_edges ~f:(fun (from_, to_) ->
          let from_p = List.nth_exn problem.figure_vertices from_ in
          let to_p = List.nth_exn problem.figure_vertices to_ in
          (from_, to_), Point.sq_distance from_p to_p)
      |> Int_int.Map.of_alist_exn
  ; hole_polygon = Polygon.of_vertices problem.hole
  ; bonuses = []
  }
;;

let set_vertices t vertices =
  { t with vertices = List.mapi vertices ~f:(fun i p -> i, p) |> Int.Map.of_alist_exn }
;;

let set_vertices' t vertices = { t with vertices }

let load_exn ~problem ~filename =
  let module J = Tiny_json.Json in
  let json = J.parse_ch (In_channel.create filename) in
  let vertices =
    json |> J.getf "vertices" |> Common.json_as_point_list ~what:"pose vertices"
  in
  let frozen_vertices =
    json
    |> J.getf_opt "frozen_vertices"
    |> Option.value_map ~default:Int.Set.empty ~f:(fun json ->
           json |> Common.json_as_int_list ~what:"frozen vertices" |> Int.Set.of_list)
  in
  let pose = set_vertices (create problem) vertices in
  pose, frozen_vertices
;;

let save_exn t ~frozen_vertices ~filename =
  let module J = Tiny_json.Json in
  let bonuses =
    if List.is_empty t.bonuses
    then []
    else
      [ ( "bonuses"
        , J.Array
            (List.map t.bonuses ~f:(fun bonus ->
                 J.Object
                   [ "bonus", J.String (Bonus_kind.to_string bonus.kind)
                   ; "problem", J.Number (Int.to_string bonus.problem)
                   ])) )
      ]
  in
  let vertices =
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
  let frozen_vertices =
    [ ( "frozen_vertices"
      , J.Array
          (Int.Set.to_list frozen_vertices
          |> List.map ~f:(fun i -> J.Number (Int.to_string i))) )
    ]
  in
  let json = J.Object (vertices @ bonuses @ frozen_vertices) in
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
  let orig_length = original_length t edge in
  let tolerance = Bignum.(t.problem.epsilon / million) in
  Bignum.(orig_length * (one - tolerance), orig_length * (one + tolerance))
;;

let move t vertex ~to_:point =
  { t with vertices = Map.set t.vertices ~key:vertex ~data:point }
;;

let edge_invalid t edge =
  let from_, to_ = edge in
  let new_length =
    Point.sq_distance (Map.find_exn t.vertices from_) (Map.find_exn t.vertices to_)
  in
  deformation_badness t edge new_length
;;

let invalid_edges t =
  List.filter_map t.problem.figure_edges ~f:(fun edge ->
      Option.map (edge_invalid t edge) ~f:(fun badness -> edge, badness))
;;

let shift t ~frozen (dx, dy) =
  { t with
    vertices =
      Map.mapi t.vertices ~f:(fun ~key:idx ~data:p ->
          if Set.mem frozen idx then p else Point.shift p (dx, dy))
  }
;;

let segment_inside_hole t (segment : Segment.t) =
  Polygon.contains t.hole_polygon segment.a
  && Polygon.contains t.hole_polygon segment.b
  && (not (Polygon.intersect_segment t.hole_polygon segment))
  (* This is a hack *)
  && Polygon.contains t.hole_polygon (Segment.middle segment)
;;

let edge_inside_hole t (a, b) =
  let pa = Map.find_exn t.vertices a in
  let pb = Map.find_exn t.vertices b in
  let the_edge = Segment.create pa pb in
  segment_inside_hole t the_edge
;;

let inside_hole t = List.for_all t.problem.figure_edges ~f:(edge_inside_hole t)

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

let dislikes_per_hole_vertex t =
  Array.map t.hole_polygon ~f:(fun hole_p ->
      Map.fold ~init:Int.max_value t.vertices ~f:(fun ~key:_ ~data:p closest_dist ->
          if closest_dist = 0
          then 0
          else (
            let dist = Point.sq_distance hole_p p |> Bignum.to_int_exn in
            Int.min dist closest_dist)))
;;

let dislikes t = dislikes_per_hole_vertex t |> Array.sum (module Int) ~f:Fn.id

let find_pose_edge_that_matches_hole_edge t ~frozen =
  (* go over all hole edges, looking for pose edges that will fit there.
     If only one such fit found, return it *)
  let frozen_coords = Point.Set.map frozen ~f:(fun idx -> Map.find_exn t.vertices idx) in
  let res = Queue.create () in
  for i = 0 to Array.length t.hole_polygon - 1 do
    let prev = if i = 0 then Array.length t.hole_polygon - 1 else i - 1 in
    let hole_from_p = t.hole_polygon.(prev) in
    let hole_to_p = t.hole_polygon.(i) in
    if Set.mem frozen_coords hole_from_p && Set.mem frozen_coords hole_to_p
    then ()
    else (
      let hole_edge_length = Point.sq_distance hole_from_p hole_to_p in
      let matching_edges =
        List.filter_map t.problem.figure_edges ~f:(fun edge ->
            let from_, to_ = edge in
            (* dont touch frozen edges *)
            if Set.mem frozen from_ || Set.mem frozen to_
            then None
            else (
              match deformation_badness t edge hole_edge_length with
              | None -> Some edge
              | Some _ -> None))
      in
      Queue.enqueue res ((hole_from_p, hole_to_p), matching_edges))
  done;
  let res = Queue.to_list res in
  (* sort edges with less ambiguous matches first *)
  let res =
    List.sort res ~compare:(fun (_, m1) (_, m2) ->
        Int.compare (List.length m1) (List.length m2))
  in
  List.concat_map res ~f:(fun ((hole_from_p, hole_to_p), matches) ->
      List.map matches ~f:(fun (from_, to_) ->
          let from_p = Map.find_exn t.vertices from_ in
          let to_p = Map.find_exn t.vertices to_ in
          printf
            !"HOLE EDGE %{sexp:Point.t} to %{sexp:Point.t} MATCHES %{sexp:Point.t} \
              %{sexp:Point.t}\n\
              %!"
            hole_from_p
            hole_to_p
            from_p
            to_p;
          (hole_from_p, hole_to_p), (from_, to_)))
;;

let sort_by_min_distance_to_hole_vertices t pts =
  let dislikes_per_hole = dislikes_per_hole_vertex t in
  let hole_vertices =
    Array.filteri t.hole_polygon ~f:(fun idx _ ->
        (* if hole vertex is already covered, ignore it *)
        dislikes_per_hole.(idx) > 0)
  in
  List.map pts ~f:(fun pt ->
      (* find min distance to some hole vertex *)
      let d =
        Array.fold ~init:Int.max_value hole_vertices ~f:(fun best hole_pt ->
            let distance = Point.sq_distance pt hole_pt |> Bignum.to_int_exn in
            Int.min distance best)
      in
      d, pt)
  |> List.sort ~compare:(fun (d1, _) (d2, _) -> Int.compare d1 d2)
  |> List.map ~f:snd
;;

module Springs = struct
  let edge_model pa pb lower_bound upper_bound : Point.t * Bignum.t =
    let sq_distance = Point.sq_distance pa pb in
    if Point.(pa = pb)
    then Point.zero, Bignum.zero
    else if Bignum.(sq_distance < lower_bound)
    then Point.(unit_length (pa - pb)), Bignum.(abs (sq_distance - lower_bound))
    else if Bignum.(upper_bound < sq_distance)
    then Point.(unit_length (pb - pa)), Bignum.(abs (sq_distance - upper_bound))
    else Point.zero, Bignum.zero
  ;;

  (* Compute the force exerced on vertex [a] by [b]. If the length of the edge
     is lower than our lower bound, [a] is "pushed away by b". If the length of
     the edge is longer than our upper bound, [a] is "attracted" toward other
     vertex. *)
  let force_one t a ~neighbour:b =
    let pa = vertex t a in
    let pb = vertex t b in
    let lower_bound, upper_bound = min_max_length_sq_for_edge t (a, b) in
    let v, k = edge_model pa pb lower_bound upper_bound in
    Point.scale v k
  ;;

  let force_neighbours (t : t) a neighbours =
    List.fold neighbours ~init:Point.zero ~f:(fun acc neighbour ->
        let f = force_one t a ~neighbour in
        Point.(acc + f))
  ;;

  (* We want to reduce dislike, by moving pose vertices closer to hole vertices,
     but doing this naively means that the figure could get clumped in a corner.
     Maybe, what we want to do is pick the closest hole vertex that we have not
     yet picked, and see if this improves things a bit. *)

  let forces t ~frozen : Point.t Int.Map.t =
    let neighbours = Problem.neighbours t.problem in
    Map.filter_mapi neighbours ~f:(fun ~key:a ~data:neighbours ->
        if Int.Set.mem frozen a then None else Some (force_neighbours t a neighbours))
  ;;

  let energy (forces : Point.t Int.Map.t) =
    Map.fold forces ~init:Bignum.zero ~f:(fun ~key:_ ~data acc ->
        Bignum.(acc + Point.sq_length data))
  ;;

  exception Found of int

  let pick_one (forces : Point.t Int.Map.t) energy : int option =
    let k = Bignum.(of_float_decimal (Random.float 1.0) * energy) in
    try
      let (_ : Bignum.t) =
        Int.Map.fold forces ~init:Bignum.zero ~f:(fun ~key ~data acc ->
            let l = Point.sq_length data in
            let acc = Bignum.(acc + l) in
            if Bignum.(k < acc)
            then
              (* Printf.eprintf "Vertex %i, force %f\n%!" key (Bignum.to_float l); *)
              raise (Found key)
            else acc)
      in
      None
    with
    | Found k -> Some k
  ;;

  (* Normalize the given vector to one of U, L, D, R. We could be more precise here, and consider 8 neighbours. *)
  let normalize_dir (t : Point.t) : Point.t =
    let open Bignum in
    if t.y >= abs t.x (* U *)
    then { x = zero; y = one }
    else if t.y <= zero - abs t.x (* D *)
    then { x = zero; y = zero - one }
    else if zero <= t.x
    then { x = one; y = zero }
    else { x = zero - one; y = zero }
  ;;

  let relax_one t ~frozen =
    let forces = forces t ~frozen in
    let energy = energy forces in
    match pick_one forces energy with
    | None ->
      (* The system is at rest *)
      (* Printf.eprintf "System at rest (energy %.2f)\n%!" (Bignum.to_float energy); *)
      t.vertices
    | Some vertex ->
      (* Printf.eprintf "Moving %i (energy %.2f)\n%!" vertex (Bignum.to_float energy); *)
      let dir = Map.find_exn forces vertex |> normalize_dir in
      let point = Point.(dir + Int.Map.find_exn t.vertices vertex) in
      let vertices = Int.Map.set t.vertices ~key:vertex ~data:point in
      vertices
  ;;

  module Testing = struct
    let point x y = Point.create ~x:(Bignum.of_int x) ~y:(Bignum.of_int y)
    let ( == ) a b = Bignum.(Point.sq_distance a b = zero)

    let%test _ =
      let p, _ = edge_model (point 0 0) (point 0 2) (Bignum.of_int 1) (Bignum.of_int 4) in
      (* Printf.printf !"%{sexp: Point.t}" p; *)
      p == Point.zero
    ;;

    let%test _ =
      (* Squared length is 4, edge is too long, 0 0 is nudged toward 0 2  *)
      let p, _ = edge_model (point 0 0) (point 0 2) (Bignum.of_int 1) (Bignum.of_int 3) in
      (* Printf.printf !"%{sexp: Point.t}" p; *)
      p == point 0 1
    ;;

    let%test _ =
      (* Squared length is 4, edge is short long, 0 0 is nudged away from 0 2  *)
      let p, _ = edge_model (point 0 0) (point 0 2) (Bignum.of_int 5) (Bignum.of_int 9) in
      (* Printf.printf !"%{sexp: Point.t}" p; *)
      p == point 0 (-1)
    ;;

    module At_rest = struct
      let problem =
        { Problem.hole = [ point 0 0; point 0 2; point 2 2; point 2 0 ]
        ; figure_edges = [ 0, 1; 1, 2; 2, 3; 3, 0 ]
        ; figure_vertices = [ point 0 0; point 0 2; point 2 2; point 2 0 ]
        ; epsilon = Bignum.of_int 1
        ; bonuses = []
        }
      ;;

      let pose = create problem

      let%test _ = force_one pose 0 ~neighbour:1 == Point.zero

      let%test _ =
        List.equal
          (fun (k1, d1) (k2, d2) -> k1 = k2 && d1 == d2)
          (forces pose ~frozen:Int.Set.empty |> Int.Map.to_alist)
          [ 0, Point.zero; 1, Point.zero; 2, Point.zero; 3, Point.zero ]
      ;;

      let%test _ =
        let forces = forces ~frozen:Int.Set.empty pose in
        Bignum.(energy forces = zero)
      ;;
    end
  end
end
