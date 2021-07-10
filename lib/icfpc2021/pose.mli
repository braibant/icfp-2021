open! Core

type t

val create : Problem.t -> t
val problem : t -> Problem.t
val vertices : t -> Point.t Int.Map.t
val set_vertices : t -> Point.t list -> t

(* io *)
val load_exn : problem:Problem.t -> filename:string -> t
val save_exn : t -> filename:string -> unit

(* get vertex by number *)
val vertex : t -> int -> Point.t

(* query *)
val invalid_edges : t -> (Edge.t * Bignum.t) list
val min_max_length_sq_for_edge : t -> Edge.t -> Bignum.t * Bignum.t
val edge_inside_hole : t -> Edge.t -> bool

(* transform *)
val move : t -> int -> to_:Point.t -> t
val shift : t -> Edge.t -> t
val reflect_vertical : t -> t
val transpose : t -> t
