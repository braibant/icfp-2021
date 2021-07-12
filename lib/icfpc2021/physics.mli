open! Core

val edges : Pose.t -> frozen:Int.Set.t -> Forces.t
val holes : Pose.t -> frozen:Int.Set.t -> Forces.t

val drag
  :  Pose.t
  -> frozen:Int.Set.t
  -> vertex:int
  -> distance:int
  -> dampening_factor:float
  -> Forces.t

val electrify : Pose.t -> frozen:Int.Set.t -> vertex:int -> Forces.t
val relax_one : Pose.t -> Forces.t -> Point.t Int.Map.t
