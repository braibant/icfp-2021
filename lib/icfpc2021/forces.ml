open! Core

type t = Vec.t Int.Map.t

let energy (forces : t) =
  Map.fold forces ~init:Float.zero ~f:(fun ~key:_ ~data acc ->
      Float.(acc + Vec.sq_length data))
;;

(* We first normalize the energetic contribution of each force field, then apply the coefficients k1 / k2 to make one dominate the other *)
let mix k1 f1 k2 f2 =
  let w1 = energy f1 in
  let w2 = energy f2 in
  let w = Float.(w1 + w2) in
  let k = k1 +. k2 in
  let k1 = Float.(k1 * w1 / (k * w)) in
  let k2 = Float.(k2 * w2 / (k * w)) in
  Map.merge f1 f2 ~f:(fun ~key:_ data ->
      match data with
      | `Left x -> Some (Vec.scale x k1)
      | `Right x -> Some (Vec.scale x k2)
      | `Both (l, r) ->
        let v = Vec.(scale l k1 + scale r k2) in
        if Vec.(v = zero) then None else Some v)
;;

let empty = Int.Map.empty
