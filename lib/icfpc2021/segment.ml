type t = {
  a : Point.t ;
  b : Point.t 
}
[@@deriving fields, sexp]


let create a b = Fields.create ~a ~b 
(* Find a,b,c such that ax + by = c *)
let coefs {a;b}  =
let open Bignum in
  let x1 = a.x in let y1 = a.y in let x2 = b.x in let y2 = b.y in
  let a = y2 - y1 in let b = x1 - x2 in let c = a * x1 + b * y1 in
  (a,b,c)


(* Checkt that [point] belong to the segment, PROVIDED that we have checked that
   the three points are aligned. Scale is a hack to avoid having to deal with
   the fact that the points of interest have rational coordinates. *)
let contains {a;b} scale (point : Point.t) =
  let open Bignum in
  let minx = min a.x b.x in
  let maxx = max a.x b.x in

  let miny = min a.y b.y in
  let maxy = max a.y b.y in

  scale * minx <= point.x  && point.x <= scale * maxx
  && scale * miny <= point.y && point.y <= scale * maxy


let intersect s1 s2 =
  let open Bignum in
  let (a1, b1, c1) = coefs s1 in
  let (a2, b2, c2 ) = coefs s2 in
  let det = a1 * b2 - a2 * b1 in
  if Bignum.equal det zero
  then false   (* Colinear *)
  else
    (* x / det , y / det are the coordinates of the line intersection  *)
    let x = b2 * c1 - b1 * c2 in
    let y = a1 * c2 - a2 * c1 in
    let p = Point.create ~x ~y in
    (* Let's check that the intersection is on the first segment  *)
        contains s1 det p && contains s2 det p
;;

