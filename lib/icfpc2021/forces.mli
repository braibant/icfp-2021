open! Core

type t = Vec.t Int.Map.t

val energy : t -> float
val mix : float -> t -> float -> t -> t
val empty : t
