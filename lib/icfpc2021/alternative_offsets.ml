open! Core

type t = (Bignum.t * Bignum.t) list Int.Map.t Int.Map.t [@@deriving sexp]

let empty = Int.Map.empty

let compute_offsets (prob : Problem.t) ~a ~b =
  let offsets = ref [] in
  let a = List.nth_exn prob.figure_vertices a in
  let b = List.nth_exn prob.figure_vertices b in
  let tolerance = Bignum.(prob.epsilon / million) in
  let orig_len = Point.distance a b in
  let min_len = Bignum.(orig_len * (one - tolerance)) in
  let max_len = Bignum.(orig_len * (one + tolerance)) in
  let int_len = Bignum.round max_len ~dir:`Up |> Bignum.to_int_exn in
  let origin = Point.create ~x:Bignum.zero ~y:Bignum.zero in
  for x = 0 to int_len do
    for y = 0 to int_len do
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

let create (prob : Problem.t) =
  printf "Creating alternative offsets...\n%!";
  List.fold_left prob.figure_edges ~init:Int.Map.empty ~f:(fun t (a, b) ->
      let offsets = compute_offsets prob ~a ~b in
      let t =
        Int.Map.change t a ~f:(fun xs ->
            let xs = Option.value xs ~default:Int.Map.empty in
            Some Int.Map.(set xs ~key:b ~data:offsets))
      in
      Int.Map.change t b ~f:(fun xs ->
          let xs = Option.value xs ~default:Int.Map.empty in
          Some Int.Map.(set xs ~key:a ~data:offsets)))
;;

let find t a b =
  match Map.find t a with
  | None -> []
  | Some xs ->
    (match Map.find xs b with
    | None -> []
    | Some ys -> ys)
;;
