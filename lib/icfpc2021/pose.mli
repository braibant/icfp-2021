open! Core

type t

val create : Problem.t -> t
val load_exn : problem:Problem.t -> filename:string -> t
val vertices : t -> Point.t list
val problem : t -> Problem.t
val set_vertices : t -> Point.t list -> t
