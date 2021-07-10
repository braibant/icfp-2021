open! Core

type t = int String.Map.t

val create : (string * int) list -> t
val load_exn : string -> t
val save_exn : t -> string -> unit

(** [improved a b] returns the set of elements where [score a < score b]  *)
val improved : t -> t -> t

val print : t -> unit
