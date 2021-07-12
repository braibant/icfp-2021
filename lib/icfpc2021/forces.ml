open! Core

type t = Vec.t Int.Map.t

let energy (forces : t) =
  Map.fold forces ~init:Float.zero ~f:(fun ~key:_ ~data acc ->
      Float.(acc + Vec.sq_length data))
;;

let mix f1 f2 =
  let w1 = energy f1 in
  let w2 = energy f2 in
  let w = Float.(w1 + w2) in
  let k1 = Float.(w1 / w) in
  let k2 = Float.(w2 / w) in
  Map.merge f1 f2 ~f:(fun ~key:_ data ->
      match data with
      | `Left x -> Some (Vec.scale x k1)
      | `Right x -> Some (Vec.scale x k2)
      | `Both (l, r) ->
        let v = Vec.(scale l k1 + scale r k2) in
        if Vec.(v = zero) then None else Some v)
;;

let empty = Int.Map.empty
