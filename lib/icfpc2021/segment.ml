open! Core

type t =
  { a : Point.t
  ; b : Point.t
  }
[@@deriving fields, sexp]

let create a b = Fields.create ~a ~b

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

(* https://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment *)
let distance =
  let open Bignum in
  let sqr x = x * x in
  let dist2 (v : Point.t) (w : Point.t) = sqr (v.x - w.x) + sqr (v.y - w.y) in
  let distance_to_segment_squared segment point =
    let l2 = dist2 segment.a segment.b in
    if l2 = zero
    then dist2 point segment.a
    else (
      let t =
        (((point.x - segment.a.x) * (segment.b.x - segment.a.x))
        + ((point.y - segment.a.y) * (segment.b.y - segment.a.y)))
        / l2
      in
      let t = max zero (min one t) in
      let projection =
        let x = segment.a.x + (t * (segment.b.x - segment.a.x)) in
        let y = segment.a.y + (t * (segment.b.y - segment.a.y)) in
        Point.create ~x ~y
      in
      dist2 point projection)
  in
  fun segment point ->
    Float.sqrt (Bignum.to_float (distance_to_segment_squared segment point))
;;

let contains segment point = Float.(abs (distance segment point) <= 0.00001)
let contains_segment segment ~other = contains segment other.a && contains segment other.b

module Testing = struct
  let point x y = Point.create ~x:(Bignum.of_int x) ~y:(Bignum.of_int y)
  let segment = create
  let ( == ) a b = Float.(abs (a - b) <= 0.00001)

  let%test _ =
    let a = segment (point 0 0) (point 0 2) in
    contains a (point 0 1)
  ;;

  let%test _ =
    let a = segment (point 0 0) (point 2 2) in
    contains a (point 1 1)
  ;;

  let%test _ =
    let a = segment (point 0 0) (point 1 1) in
    contains a (point 1 1)
  ;;

  let%test _ =
    let a = segment (point 1 0) (point 1 2) in
    let b = segment (point 0 1) (point 2 1) in
    intersect a b
  ;;

  let%test _ =
    let s = segment (point 0 0) (point 0 2) in
    distance s (point 1 1) == 1.0
  ;;

  let%test _ =
    let s = segment (point 0 10) (point 10 10) in
    distance s (point 0 10) == 0.0
  ;;

  let%test _ =
    let s = segment (point 0 10) (point 10 10) in
    distance s (point 10 10) == 0.0
  ;;

  let%test _ =
    let s = segment (point 0 10) (point 10 10) in
    contains_segment s ~other:s
  ;;

  let%test _ =
    let s = segment (point 0 10) (point 10 10) in
    let h = segment (point 5 10) (point 10 10) in
    contains_segment s ~other:h
  ;;

  let%test _ =
    let s = segment (point 0 0) (point 0 2) in
    distance s (point 2 1) == 2.0
  ;;
end
