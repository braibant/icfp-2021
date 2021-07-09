open! Core
module G = Graphics
open Icfpc2021

let draw_bg () =
  G.clear_graph ();
  G.set_color G.black;
  let width = G.size_x () in
  let height = G.size_y () in
  G.fill_rect 0 0 width height;
  G.moveto ((width / 2) - 50) (height - 20);
  G.set_color G.white;
  G.draw_string "ICFP 2021"
;;

let rec interact ~prob =
  draw_bg ();
  let status = G.wait_next_event [ G.Key_pressed ] in
  match (status : G.status) with
  | { keypressed = true; key = 'q' | '\027'; _ } -> print_endline "Quitting..."
  | _ ->
    printf
      !"Ignoring input: %{sexp#mach: Sexp.t}\n%!"
      [%sexp
        { mouse_x : int = status.mouse_x
        ; mouse_y : int = status.mouse_y
        ; button : bool = status.button
        ; keypressed : bool = status.keypressed
        ; key : char = status.key
        }];
    let (_ : float) = Unix.nanosleep 0.033 in
    interact ~prob
;;

let display ~filename =
  let prob = Problem.load_exn ~filename in
  G.open_graph " 1000x800";
  G.set_window_title "ICFPC 2021";
  printf !"%{Problem#hum}\n%!" prob;
  let () = interact ~prob in
  G.close_graph ()
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
