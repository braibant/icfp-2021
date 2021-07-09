open! Core

type t =
  { hole : Point.t list
  ; figure_edges : Point.t list
  ; figure_vertices : Point.t list
  ; epsilon : Bignum.t
  }
[@@deriving sexp]

let load_exn ~filename =
  let module J = Ezjsonm in
  let x = J.from_channel (In_channel.create filename) in
  (* print_endline (Sexp.to_string_hum (J.sexp_of_t x)); *)
  let epsilon = J.find x [ "epsilon" ] |> J.value_to_string in
  print_endline epsilon;
  { hole = []; figure_edges = []; figure_vertices = []; epsilon = Bignum.zero }
;;
