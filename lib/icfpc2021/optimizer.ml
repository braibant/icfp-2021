open! Core

let score (t : Pose.t) dislikes =
  (* Negative score if we have points outside the hull *)
  let vertices = Pose.vertices t |> Int.Map.to_alist in
  let polygon = Pose.hole_polygon t in
  let score =
    List.filter_map vertices ~f:(fun (_v, p) ->
        if Polygon.contains polygon p then None else Some (Polygon.distance polygon p))
    |> List.sum (module Float) ~f:Fn.id
  in
  let score = score +. if Pose.inside_hole t then 0. else -1000. in
  if Float.(score < 0.0) then score else Float.(100_000_000.0 - of_int dislikes)
;;
