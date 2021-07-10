open! Core

type t = (int * int) list Int.Map.t Int.Map.t

let compute_offsets _prob ~a:_ ~b:_ =
  let offsets = ref [] in
  (* let a = List.nth_exn prob.figure_vertices a in
   * let b = List.nth_exn problem.figure_vertices b in
   * let tolerance = Bignum.(prob.epsilon / million) in
   * let len_min = Point.distance a b in *)
  !offsets
;;

let create (prob : Problem.t) =
  List.fold_left prob.figure_edges ~init:Int.Map.empty ~f:(fun t (a, b) ->
      let offsets = compute_offsets prob ~a ~b in
      let t =
        Int.Map.change t a ~f:(fun xs ->
            let xs = Option.value xs ~default:Int.Map.empty in
            Some Int.Map.(set xs ~key:b ~data:offsets))
      in
      Int.Map.change t b ~f:(fun xs ->
          let xs = Option.value xs ~default:Int.Map.empty in
          Some Int.Map.(set xs ~key:a ~data:offsets)))
;;

let find t a b =
  match Map.find t a with
  | None -> []
  | Some xs ->
    (match Map.find xs b with
    | None -> []
    | Some ys -> ys)
;;
