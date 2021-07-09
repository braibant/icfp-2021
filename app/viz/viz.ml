open! Core
module G = Graphics
open Icfpc2021

let display ~filename =
  let prob = Problem.load_exn ~filename in
  G.open_graph " 1000x1000";
  G.set_window_title "ICFPC 2021";
  printf !"%{sexp:Problem.t}" prob
;;

let commands =
  let open Command.Let_syntax in
  Command.group
    ~summary:"Vizualizer for ICFPC 2021"
    [ ( "display"
      , Command.basic
          ~summary:"Display a problem"
          (let%map_open filename =
             anon ("FILE" %: Filename.arg_type)
             (* and api_key =
              *   flag
              *     "-api-key"
              *     (required string)
              *     ~doc:"API-KEY API-KEY for identifying with the server" *)
           in
           fun () -> display ~filename) )
    ]
;;

let () = Command.run commands
