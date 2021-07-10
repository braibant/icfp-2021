open! Core

type t

val create : Problem.t -> t
val find : t -> int -> int -> (int * int) list
