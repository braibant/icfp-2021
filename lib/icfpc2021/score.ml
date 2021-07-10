open! Core

(* Line segment sthat connect in the original figure connect in the assumed
   pose. *)
(* Edges cannot be compressed or stretched arbitrrarily. *)
(* Every point contained on any line segment of the figure in the pose must lay
   inside the hole or its boundary. *)

let evaluate (pose : Pose.t) : int option =
  if Pose.inside_hole pose && List.is_empty (Pose.invalid_edges pose)
  then Some (Pose.dislikes pose)
  else None
;;
