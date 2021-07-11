open! Core

val edges : Pose.t -> frozen:Int.Set.t -> Forces.t
val holes : Pose.t -> frozen:Int.Set.t -> Forces.t
val relax_one : Pose.t -> Forces.t -> Point.t Int.Map.t
