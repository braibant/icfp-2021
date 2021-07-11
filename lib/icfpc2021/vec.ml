open! Core

type t =
  { x : float
  ; y : float
  }
[@@deriving fields, hash, sexp]

let compare a b =
  let c = Float.robustly_compare a.x b.x in
  if c = 0 then Float.robustly_compare a.y b.y else c
;;

let ( = ) a b = compare a b = 0
let create = Fields.create
let zero = { x = Float.zero; y = Float.zero }
let sq_distance a b = Float.(((a.x - b.x) ** 2.0) + ((a.y - b.y) ** 2.0))
let sq_length a = sq_distance a zero
let equal a b = Float.equal a.x b.x && Float.equal a.y b.y
let shift t (dx, dy) = { x = Float.(t.x + of_int dx); y = Float.(t.y + of_int dy) }
let ( + ) a b = Float.{ x = a.x + b.x; y = a.y + b.y }
let ( - ) a b = Float.{ x = a.x - b.x; y = a.y - b.y }
let scale t k = Float.{ x = t.x * k; y = t.y * k }

let unit_length t =
  let length = Float.sqrt (sq_length t) in
  if Float.(length = zero) then Printf.eprintf !"Unit length %{sexp: t}\n%!" t;
  let k = Float.(one / length) in
  scale t k
;;

(* Normalize the given vector to one of U, L, D, R. We could be more precise here, and consider 8 neighbours. *)
let normalize_dir (t : t) : t =
  let open Float in
  if t.y >= abs t.x (* U *)
  then { x = zero; y = one }
  else if t.y <= zero - abs t.x (* D *)
  then { x = zero; y = zero - one }
  else if zero <= t.x
  then { x = one; y = zero }
  else { x = zero - one; y = zero }
;;
