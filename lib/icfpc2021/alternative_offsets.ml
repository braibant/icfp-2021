open! Core

type t = (Bignum.t * Bignum.t) list Int.Map.t Int.Map.t [@@deriving sexp]

let empty = Int.Map.empty

let compute_offsets epsilon ~a ~b =
  let offsets = ref [] in
  let tolerance = Bignum.(epsilon / million) in
  let orig_len = Point.distance a b in
  let min_len = Bignum.(orig_len * (one - tolerance)) in
  let max_len = Bignum.(orig_len * (one + tolerance)) in
  let _int_len = Bignum.round max_len ~dir:`Up |> Bignum.to_int_exn in
  let origin = Point.create ~x:Bignum.zero ~y:Bignum.zero in
  let max_x =
    (* at most sqrt(max_len) *)
    Float.(sqrt (Bignum.to_float max_len) |> round_up |> to_int)
  in
  for x = 0 to max_x do
    let y_from_len len =
      (*  y = sqrt (len - x^2) *)
      let y2 = Bignum.(len - (of_int x * of_int x)) in
      if Bignum.(y2 <= zero)
      then 0
      else Bignum.to_float y2 |> Float.sqrt |> Float.round_up |> Float.to_int
    in
    (* Smallest y should be the one that lets us reach min_len, largest y should 
       be the one where we reach max_len *)
    for y = y_from_len min_len to y_from_len max_len do
      let p = Point.create ~x:(Bignum.of_int x) ~y:(Bignum.of_int y) in
      let dist = Point.distance origin p in
      if Bignum.( <= ) min_len dist && Bignum.( <= ) dist max_len
      then
        let open Bignum in
        let x = of_int x in
        let y = of_int y in
        offsets := (x, y) :: (neg x, y) :: (x, neg y) :: (neg x, neg y) :: !offsets
    done
  done;
  !offsets
;;

let%expect_test "offsets" =
  let offsets =
    Bignum.(
      compute_offsets
        (of_int 100_000)
        ~a:(Point.create ~x:one ~y:one)
        ~b:(Point.create ~x:ten ~y:ten))
  in
  printf !"%{sexp: (Bignum.t * Bignum.t) list}\n" offsets;
  [%expect
    {|
    ((13 3) (-13 3) (13 -3) (-13 -3) (13 2) (-13 2) (13 -2) (-13 -2) (13 1)
     (-13 1) (13 -1) (-13 -1) (13 0) (-13 0) (13 0) (-13 0) (12 5) (-12 5)
     (12 -5) (-12 -5) (12 4) (-12 4) (12 -4) (-12 -4) (12 3) (-12 3) (12 -3)
     (-12 -3) (12 2) (-12 2) (12 -2) (-12 -2) (11 7) (-11 7) (11 -7) (-11 -7)
     (11 6) (-11 6) (11 -6) (-11 -6) (11 5) (-11 5) (11 -5) (-11 -5) (10 8)
     (-10 8) (10 -8) (-10 -8) (10 7) (-10 7) (10 -7) (-10 -7) (9 9) (-9 9)
     (9 -9) (-9 -9) (8 10) (-8 10) (8 -10) (-8 -10) (7 11) (-7 11) (7 -11)
     (-7 -11) (7 10) (-7 10) (7 -10) (-7 -10) (6 11) (-6 11) (6 -11) (-6 -11)
     (5 12) (-5 12) (5 -12) (-5 -12) (5 11) (-5 11) (5 -11) (-5 -11) (4 12)
     (-4 12) (4 -12) (-4 -12) (3 13) (-3 13) (3 -13) (-3 -13) (3 12) (-3 12)
     (3 -12) (-3 -12) (2 13) (-2 13) (2 -13) (-2 -13) (2 12) (-2 12) (2 -12)
     (-2 -12) (1 13) (-1 13) (1 -13) (-1 -13) (0 13) (0 13) (0 -13) (0 -13)) |}]
;;

let create (prob : Problem.t) =
  printf "Creating alternative offsets...\n%!";
  List.fold_left prob.figure_edges ~init:Int.Map.empty ~f:(fun t (a, b) ->
      let pa = List.nth_exn prob.figure_vertices a in
      let pb = List.nth_exn prob.figure_vertices b in
      let offsets = compute_offsets prob.epsilon ~a:pa ~b:pb in
      let t =
        Map.update t a ~f:(fun xs ->
            let xs = Option.value xs ~default:Int.Map.empty in
            Int.Map.(set xs ~key:b ~data:offsets))
      in
      Int.Map.update t b ~f:(fun xs ->
          let xs = Option.value xs ~default:Int.Map.empty in
          Int.Map.(set xs ~key:a ~data:offsets)))
;;

let find t a b =
  match Map.find t a with
  | None -> []
  | Some xs ->
    (match Map.find xs b with
    | None -> []
    | Some ys -> ys)
;;
