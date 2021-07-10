open! Core

type t = Point.t array

let of_vertices l = Array.of_list l
(*
https://wrf.ecse.rpi.edu/Research/Short_Notes/pnpoly.html
```
   int pnpoly(int nvert, float *vertx, float *verty, float testx, float testy)
    {
      int i, j, c = 0;
      for (i = 0, j = nvert-1; i < nvert; j = i++) {
          if ( ((verty[i]>testy) != (verty[j]>testy)) &&
	 (testx < (vertx[j]-vertx[i]) * (testy-verty[i]) / (verty[j]-verty[i]) + vertx[i]) )
              c = !c;
        }
          return c;
    }
```
*)

(*
Or, https://github.com/substack/point-in-polygon, which derives from the same source

  var x = point[0], y = point[1];

    var inside = false;
    for (var i = 0, j = vs.length - 1; i < vs.length; j = i++) {
        var xi = vs[i][0], yi = vs[i][1];
        var xj = vs[j][0], yj = vs[j][1];

        var intersect = ((yi > y) != (yj > y))
            && (x < (xj - xi) * (y - yi) / (yj - yi) + xi);
        if (intersect) inside = !inside;
    }

    return inside;
 *)

let fold_edges (type a) t ~(f : a -> Segment.t -> a) ~(init : a) : a =
  let i = ref 0 in
  let j = ref (Array.length t - 1) in
  let acc = ref init in
  while !i < Array.length t do
    let s = Segment.create t.(!j) t.(!i) in
    acc := f !acc s;
    j := !i;
    i := Int.(!i + 1)
  done;
  !acc
;;

let contains_excluding_perimeter (t : t) (pt : Point.t) =
  let x = pt.x in
  let y = pt.y in
  let c = ref false in
  let i = ref 0 in
  let j = ref (Array.length t - 1) in
  while !i < Array.length t do
    let open Bignum in
    let xi = t.(!i).x in
    let yi = t.(!i).y in
    let xj = t.(!j).x in
    let yj = t.(!j).y in
    let intersect =
      Bool.( <> ) (yi > y) (yj > y) && x < ((xj - xi) * (y - yi) / (yj - yi)) + xi
    in
    if intersect then c := not !c;
    j := !i;
    i := Int.(!i + 1)
  done;
  !c
;;

let contains t pt =
  let on_perimeter =
    fold_edges t ~f:(fun acc segment -> acc || Segment.contains segment pt) ~init:false
  in
  on_perimeter || contains_excluding_perimeter t pt
;;

let intersect_segment (t : t) s1 =
  let i = ref 0 in
  let intersect = ref false in
  while (not !intersect) && !i < Array.length t do
    let pa = t.(!i mod Array.length t) in
    let pb = t.((!i + 1) mod Array.length t) in
    let s2 = Segment.create pa pb in
    intersect := Segment.intersect s1 s2;
    i := !i + 1
  done;
  !intersect
;;

let on_perimeter (t : t) s1 =
  fold_edges t ~f:(fun acc other -> acc || Segment.contains_segment s1 ~other) ~init:false
;;

let intersect_segment t s1 = if on_perimeter t s1 then false else intersect_segment t s1

let distance t point =
  let distance =
    fold_edges
      t
      ~f:(fun acc s -> Float.min (Segment.distance s point) acc)
      ~init:Float.infinity
  in
  if contains t point then distance else -.distance
;;

module Testing = struct
  let point x y = Point.create ~x:(Bignum.of_int x) ~y:(Bignum.of_int y)

  let polygon l =
    List.map l ~f:(fun (x, y) -> Point.create ~x:(Bignum.of_int x) ~y:(Bignum.of_int y))
    |> Array.of_list
  ;;

  let%test _ =
    let polygon = polygon [ 0, 0; 0, 2; 2, 2; 2, 0; 0, 0 ] in
    contains polygon (point 0 1)
  ;;

  let%test _ =
    let polygon = polygon [ 0, 0; 0, 2; 2, 2; 2, 0; 0, 0 ] in
    contains polygon (point 1 0)
  ;;

  let%test _ =
    let polygon = polygon [ 0, 0; 0, 2; 2, 2; 2, 0; 0, 0 ] in
    not (contains polygon (point 1 100))
  ;;

  let%test _ =
    let polygon = polygon [ 0, 0; 2, 0; 4, 2; 2, 2; 0, 0 ] in
    contains polygon (point 2 1)
  ;;

  let%test _ =
    let polygon = polygon [ 0, 0; 0, 2; 2, 2; 2, 0; 0, 0 ] in
    let segment = Segment.create (point 1 1) (point 1 3) in
    intersect_segment polygon segment
  ;;

  let%test _ =
    let polygon = polygon [ 0, 0; 0, 4; 4, 4; 4, 0; 0, 0 ] in
    let segment = Segment.create (point 1 1) (point 1 3) in
    not (intersect_segment polygon segment)
  ;;

  let%test _ =
    let polygon = polygon [ 0, 0; 0, 4; 4, 4; 4, 0; 0, 0 ] in
    contains polygon (point 0 4)
  ;;

  let%test _ =
    let polygon = polygon [ 0, 0; 0, 4; 4, 4; 4, 0; 0, 0 ] in
    contains polygon (point 0 2)
  ;;

  let%test _ =
    let polygon = polygon [ 10, 0; 10, 10; 0, 10; 10, 0 ] in
    let segment = Segment.create (point 10 0) (point 10 10) in
    not (intersect_segment polygon segment)
  ;;

  let ( == ) a b = Float.(abs (a - b) <= 0.00001)

  let%test _ =
    let polygon = polygon [ 0, 0; 0, 2; 2, 2; 2, 0 ] in
    let point = point 1 1 in
    distance polygon point == 1.0
  ;;
end
