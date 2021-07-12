open! Core

type t = Vec.t Int.Map.t

val energy : t -> float
val mix : t -> t -> t
val empty : t
