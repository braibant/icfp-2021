open Core

let vec_of_point (p : Point.t) =
  Vec.create ~x:(Bignum.to_float p.x) ~y:(Bignum.to_float p.y)
;;

let point_of_vec (p : Vec.t) =
  Point.create ~x:(Bignum.of_float_decimal p.x) ~y:(Bignum.of_float_decimal p.y)
;;

let vec t a = Pose.vertex t a |> vec_of_point

let edge_model pa pb lower_bound upper_bound : Vec.t * float =
  let sq_distance = Vec.sq_distance pa pb in
  if Vec.(pa = pb)
  then Vec.zero, 0.0
  else if Float.(sq_distance < lower_bound)
  then Vec.(unit_length (pa - pb)), Float.(abs (sq_distance - lower_bound))
  else if Float.(upper_bound < sq_distance)
  then Vec.(unit_length (pb - pa)), Float.(abs (sq_distance - upper_bound))
  else Vec.zero, 0.0
;;

let pose_vertex_to_hole_vertex_model (pv : Point.t) (hv : Point.t) (dislikes : Float.t)
    : Vec.t
  =
  if Point.(pv = hv)
  then Vec.zero
  else (
    let p = Point.(hv - pv) |> vec_of_point |> Vec.unit_length in
    let d = Point.sq_distance hv pv |> Bignum.to_float in
    Vec.scale p Float.(d / dislikes))
;;

(* Compute the force exerced on vertex [a] by [b]. If the length of the edge
     is lower than our lower bound, [a] is "pushed away by b". If the length of
     the edge is longer than our upper bound, [a] is "attracted" toward other
     vertex. *)
let force_one t a ~neighbour:b =
  let pa = vec t a in
  let pb = vec t b in
  let lower_bound, upper_bound = Pose.min_max_length_sq_for_edge t (a, b) in
  let lower_bound = Bignum.to_float lower_bound in
  let upper_bound = Bignum.to_float upper_bound in
  let v, k = edge_model pa pb lower_bound upper_bound in
  Vec.scale v k
;;

let force_neighbours (t : Pose.t) a neighbours =
  List.fold neighbours ~init:Vec.zero ~f:(fun acc neighbour ->
      let f = force_one t a ~neighbour in
      Vec.(acc + f))
;;

let uncovered_hole_vertices (t : Pose.t) : Point.t list =
  let hole = Pose.hole_polygon t |> Point.Set.of_array in
  let vertices = Map.data (Pose.vertices t) |> Point.Set.of_list in
  Point.Set.diff hole vertices |> Point.Set.to_list
;;

let force_to_nearest_hole_vertex pt uhvs dislikes =
  List.map uhvs ~f:(fun uhv -> Point.sq_distance pt uhv |> Bignum.to_float, uhv)
  |> List.min_elt ~compare:(fun a b -> Float.compare (fst a) (fst b))
  |> Option.value_map ~default:Vec.zero ~f:(fun (_, uhv) ->
         pose_vertex_to_hole_vertex_model pt uhv dislikes)
;;

let edges t ~frozen : Forces.t =
  let vertices =
    Map.filter_keys (Pose.vertices t) ~f:(fun a -> not (Int.Set.mem frozen a))
  in
  Map.mapi vertices ~f:(fun ~key:vertex ~data:_ ->
      let neighbours = Map.find_exn (Pose.neighbours t) vertex in
      force_neighbours t vertex neighbours)
;;

(* Compute the set of neighbours up to distance [d] *)
let relatives t ~vertex ~distance =
  assert (0 <= distance);
  let neighbours = Pose.neighbours t in
  let v = Array.create ~len:(distance + 1) Int.Set.empty in
  let acc = ref (Int.Set.singleton vertex) in
  for i = 0 to distance do
    v.(i) <- !acc;
    let next =
      Set.fold !acc ~init:Int.Set.empty ~f:(fun acc n ->
          Int.Set.union (Int.Set.of_list (Map.find_exn neighbours n)) acc)
    in
    acc := next
  done;
  v
;;

let drag t ~frozen ~vertex ~distance : Forces.t =
  let relatives = relatives t ~vertex ~distance in
  let forces = ref (edges t ~frozen) in
  let visited = ref Int.Set.empty in
  let scale = ref 1.0 in
  for i = 1 to distance do
    let s = relatives.(i) in
    let s = Int.Set.diff s !visited in
    Set.iter s ~f:(fun v ->
        match Map.find !forces v with
        | None -> ()
        | Some f -> forces := Map.set !forces ~key:v ~data:(Vec.scale f !scale));
    visited := Set.union !visited s;
    scale := 0.8 *. !scale
  done;
  !forces
;;

(* We want to reduce dislike, by moving pose vertices closer to hole vertices,
     but doing this naively means that the figure could get clumped in a corner.
     Maybe, what we want to do is pick the closest hole vertex that we have not
     yet picked, and see if this improves things a bit. *)
let holes t ~frozen : Forces.t =
  let vertices =
    Map.filter_keys (Pose.vertices t) ~f:(fun a -> not (Int.Set.mem frozen a))
  in
  let uncovered_hole_vertices = uncovered_hole_vertices t in
  let dislikes = Pose.dislikes t |> Float.of_int in
  Map.mapi vertices ~f:(fun ~key:_vertex ~data:pt ->
      force_to_nearest_hole_vertex pt uncovered_hole_vertices dislikes)
;;

exception Found of int

let pick_one (forces : Vec.t Int.Map.t) energy : int option =
  let k = Random.float 1.0 *. energy in
  try
    let (_ : Float.t) =
      Int.Map.fold forces ~init:Float.zero ~f:(fun ~key ~data acc ->
          let l = Vec.sq_length data in
          let acc = Float.(acc + l) in
          if Float.(k < acc)
          then
            (* Printf.eprintf "Vertex %i, force %f\n%!" key (Bignum.to_float l); *)
            raise (Found key)
          else acc)
    in
    None
  with
  | Found k -> Some k
;;

let relax_one t forces =
  let energy = Forces.energy forces in
  match pick_one forces energy with
  | None ->
    (* The system is at rest *)
    (* Printf.eprintf "System at rest (energy %.2f)\n%!" (Bignum.to_float energy); *)
    Pose.vertices t
  | Some v ->
    (* Printf.eprintf "Moving %i (energy %.2f)\n%!" vertex (Bignum.to_float energy); *)
    let dir = Map.find_exn forces v |> point_of_vec |> Point.normalize_dir in
    let v' = Point.(dir + Pose.vertex t v) in
    Int.Map.set (Pose.vertices t) ~key:v ~data:v'
;;

module Testing = struct
  let vec x y = Vec.create ~x:(Float.of_int x) ~y:(Float.of_int y)
  let point x y = Point.create ~x:(Bignum.of_int x) ~y:(Bignum.of_int y)
  let ( == ) a b = Float.(Vec.sq_distance a b = zero)

  let%test _ =
    let p, _ = edge_model (vec 0 0) (vec 0 2) (Float.of_int 1) (Float.of_int 4) in
    (* Printf.printf !"%{sexp: Point.t}" p; *)
    p == Vec.zero
  ;;

  let%test _ =
    (* Squared length is 4, edge is too long, 0 0 is nudged toward 0 2  *)
    let p, _ = edge_model (vec 0 0) (vec 0 2) (Float.of_int 1) (Float.of_int 3) in
    (* Printf.printf !"%{sexp: Point.t}" p; *)
    p == vec 0 1
  ;;

  let%test _ =
    (* Squared length is 4, edge is short long, 0 0 is nudged away from 0 2  *)
    let p, _ = edge_model (vec 0 0) (vec 0 2) (Float.of_int 5) (Float.of_int 9) in
    (* Printf.printf !"%{sexp: Point.t}" p; *)
    p == vec 0 (-1)
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

    let pose = Pose.create problem

    let%test _ = force_one pose 0 ~neighbour:1 == Vec.zero

    let%test _ =
      let forces = edges pose ~frozen:Int.Set.empty |> Int.Map.to_alist in
      List.is_empty forces
    ;;

    let%test _ =
      let forces = edges ~frozen:Int.Set.empty pose in
      let energy = Forces.energy forces in
      Float.(energy = zero)
    ;;
  end
end
