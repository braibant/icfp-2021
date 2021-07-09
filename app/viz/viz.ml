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

(* Draw the hole, figure, and various info.  Note that the coordinate
   system is (0, 0) in the bottom left corner and growing rightwards
   and upwards. *)
let draw_problem ~prob ~wall_x ~wall_y ~wall_width ~wall_height =
  (* Draw a blue border around the wall. *)
  G.set_color (G.rgb 100 100 150);
  G.fill_rect wall_x wall_y wall_width wall_height;
  (* Make the wall smaller by the size of the border. *)
  let wall_y = wall_y + 4 in
  let wall_x = wall_x + 4 in
  let wall_height = wall_height - 8 in
  let wall_width = wall_width - 8 in
  (* Draw the wall grey background. *)
  G.set_color (G.rgb 200 200 200);
  G.fill_rect wall_x wall_y wall_width wall_height;
  (* Figure out the scaling for the problem points. *)
  let max_x, max_y = Problem.max_xy prob in
  let scale_x =
    let open Bignum in
    let wall_width = of_int wall_width in
    max_x / wall_width
  in
  let scale_y =
    let open Bignum in
    let wall_height = of_int wall_height in
    max_y / wall_height
  in
  let scale = Bignum.max scale_x scale_y in
  let scale_point Point.{ x; y } =
    let x = wall_x + (Bignum.(x / scale) |> Bignum.round |> Bignum.to_int_exn) in
    let y =
      wall_y + wall_height - (Bignum.(y / scale) |> Bignum.round |> Bignum.to_int_exn)
    in
    x, y
  in
  (* Draw some info text. *)
  G.moveto (wall_x + wall_width + 10) (wall_y + wall_height - 10);
  G.set_color G.white;
  G.draw_string (sprintf !"Scale: %{Bignum#hum}" scale);
  (* Draw the actual hole (scaled to the size of the wall). *)
  G.set_color G.black;
  G.fill_poly (List.map prob.hole ~f:scale_point |> Array.of_list);
  (* Draw the red stick figure. *)
  let scaled_vertices = List.map prob.figure_vertices ~f:scale_point |> Array.of_list in
  G.set_color G.red;
  G.draw_segments
    (List.map prob.figure_edges ~f:(fun (idx1, idx2) ->
         try
           let x1, y1 = scaled_vertices.(idx1) in
           let x2, y2 = scaled_vertices.(idx2) in
           x1, y1, x2, y2
         with
         | _ -> failwithf "Failed to draw edge %d->%d" idx1 idx2 ())
    |> Array.of_list)
;;

let rec interact ~prob =
  draw_bg ();
  draw_problem
    ~prob
    ~wall_x:10
    ~wall_y:(G.size_y () - 30 - 700)
    ~wall_width:800
    ~wall_height:700;
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
