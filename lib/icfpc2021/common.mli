open! Core

val json_as_bigint : Tiny_json.Json.t -> Bignum.t
val json_as_point : Tiny_json.Json.t -> what:string -> Point.t
val json_as_point_list : Tiny_json.Json.t -> what:string -> Point.t list
