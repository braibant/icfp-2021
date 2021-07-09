open! Core

let json_as_bigint x =
  match (x : Tiny_json.Json.t) with
  | String str | Number str -> Bignum.of_string str
  | Object _ | Array _ | Bool _ | Null ->
    failwith "json_as_bigint: Expected string or number"
;;

let json_as_point_list json ~what =
  let module J = Tiny_json.Json in
  json
  |> J.as_list
  |> List.map ~f:(fun j ->
         match J.as_list j with
         | [ x; y ] -> Point.create ~x:(json_as_bigint x) ~y:(json_as_bigint y)
         | _ -> failwithf "Parsing %s: expected list of pairs" what ())
;;
