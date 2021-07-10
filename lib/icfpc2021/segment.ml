open! Core

type t =
  { a : Point.t
  ; b : Point.t
  }
[@@deriving fields, sexp]

let create a b = Fields.create ~a ~b

(* Checkt that [point] belong to the segment, PROVIDED that we have checked that
   the three points are aligned. Scale is a hack to avoid having to deal with
   the fact that the points of interest have rational coordinates. *)
let contains { a; b } scale (point : Point.t) =
  let open Bignum in
  let minx = min a.x b.x in
  let maxx = max a.x b.x in
  let miny = min a.y b.y in
  let maxy = max a.y b.y in
  scale * minx <= point.x
  && point.x <= scale * maxx
  && scale * miny <= point.y
  && point.y <= scale * maxy
;;

(* https://bryceboe.com/2006/10/23/line-segment-intersection-algorithm/ *)
let ccw (a : Point.t) (b : Point.t) (c : Point.t) =
  let open Bignum in
  (c.y - a.y) * (b.x - a.x) > (b.y - a.y) * (c.x - a.x)
;;

let intersect s1 s2 =
  let a = s1.a in
  let b = s1.b in
  let c = s2.a in
  let d = s2.b in
  Bool.(ccw a c d <> ccw b c d) && Bool.(ccw a b c <> ccw a b d)
;;

module Testing = struct
  let point x y = Point.create ~x:(Bignum.of_int x) ~y:(Bignum.of_int y)
  let segment = create

  let%test _ =
    let a = segment (point 0 0) (point 0 2) in
    contains a Bignum.one (point 0 1)
  ;;

  let%test _ =
    let a = segment (point 0 0) (point 2 2) in
    contains a Bignum.one (point 1 1)
  ;;

  let%test _ =
    let a = segment (point 0 0) (point 1 1) in
    contains a (Bignum.of_int 2) (point 1 1)
  ;;

  let%test _ =
    let a = segment (point 1 0) (point 1 2) in
    let b = segment (point 0 1) (point 2 1) in
    intersect a b
  ;;
end
