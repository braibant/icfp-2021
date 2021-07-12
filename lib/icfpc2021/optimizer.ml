open! Core

(* Large negative score if we have points outside the hull, or if the pose is not
   completely inside the hole.  *)
let score (t : Pose.t) =
  let dislikes = Pose.dislikes t in
  let vertices = Pose.vertices t |> Int.Map.to_alist in
  let polygon = Pose.hole_polygon t in
  let score =
    List.filter_map vertices ~f:(fun (_v, p) ->
        if Polygon.contains polygon p then None else Some (Polygon.distance polygon p))
    |> List.sum (module Float) ~f:Fn.id
  in
  if Float.(score < 0.0)
  then -3000. -. (1000.0 *. Float.log (1. -. score))
  else if not (Pose.inside_hole t)
  then -2000.
  else (
    let bad_edges = Pose.invalid_edges t in
    let badness = List.sum (module Bignum) bad_edges ~f:snd |> Bignum.to_float in
    if Float.(badness > 0.0)
    then Float.(-1000.0 * log (1. +. badness))
    else Float.(100_000_000.0 - of_int dislikes))
;;

let optimize1 t =
  let vertices = Pose.vertices t |> Int.Map.to_alist in
  let initial_score = score t in
  let best_score = ref initial_score in
  let best = ref None in
  List.iter vertices ~f:(fun (v, p) ->
      List.iter Point.dirs ~f:(fun d ->
          let t' = Pose.update_vertex t v Point.(p + d) in
          let s = score t' in
          if Float.(s > !best_score)
          then (
            best := Some t';
            best_score := s)
          else ()));
  (match !best with
  | None -> Printf.eprintf "No improvement found\n%!"
  | Some _ ->
    Printf.eprintf "Improved score (new %f, was %f)\n%!" !best_score initial_score);
  !best
;;
