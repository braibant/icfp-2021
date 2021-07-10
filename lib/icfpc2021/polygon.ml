open! Core

type t = Point.t array

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

let contains (t : t) (pt : Point.t) =
  let i = ref 0 in
  let j = ref (Array.length t - 1) in
  let c = ref false in
  while !i < Array.length t do
    let open Bignum in
    if ((t.(!i).y <= pt.y && pt.y < t.(!j).y) || (t.(!j).y <= pt.y && pt.y < t.(!i).y))
       &&
       let m = t.(!j).y - t.(!i).y in
       m * (pt.x - t.(!i).x) < (t.(!j).x - t.(!i).x) * (pt.y - t.(!i).y)
    then c := not !c;
    j := !i;
    i := Int.(!i + 1)
  done;
  !c
;;

let intersect_segment (t : t) s1 =
  let i = ref 0 in
  let intersect = ref false in
  while (not !intersect) && !i < Array.length t do
    let pa = t.(!i mod Array.length t) in
    let pb = t.((!i + 1) mod Array.length t) in
    let s2 = Segment.create pa pb in
    intersect := Segment.intersect s1 s2
  done;
  !intersect
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
end
