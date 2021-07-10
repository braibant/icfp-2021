open Core

type t = int String.Map.t [@@deriving sexp]

let create l = String.Map.of_alist_exn l

let load_exn filename : t =
  Stdio.In_channel.with_file filename ~f:(fun channel ->
      let content = Stdio.In_channel.input_all channel in
      Sexp.of_string_conv_exn content t_of_sexp)
;;

let save_exn t filename =
  Stdio.Out_channel.with_file filename ~f:(fun out ->
      let s = Sexp.to_string_hum (sexp_of_t t) in
      Stdio.Out_channel.output_string out s)
;;

let improved a b =
  Map.merge a b ~f:(fun ~key:_ data ->
      match data with
      | `Left _ -> None
      | `Right score -> Some score
      | `Both (left, right) -> if right < left then Some right else None)
;;

let print t =
  Map.iteri t ~f:(fun ~key:problem_id ~data:score ->
      Printf.printf "%s,\t%i\n" problem_id score)
;;
