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
   and upwards.

   There are two coordinate systems here:
   - figure space where coordinates are in [Bignum.t]s, and
   - screen/wall space where coordinates are in [ints]
*)
let draw_problem ~prob ~wall_x ~wall_y ~wall_width ~wall_height ~mouse =
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
    (max_x + Bignum.one) / wall_width
  in
  let scale_y =
    let open Bignum in
    let wall_height = of_int wall_height in
    (max_y + Bignum.one) / wall_height
  in
  let scale = Bignum.max scale_x scale_y in
  let figure_to_wall_space Point.{ x; y } =
    let x = wall_x + (Bignum.(x / scale) |> Bignum.round |> Bignum.to_int_exn) in
    let y = wall_y + (Bignum.(y / scale) |> Bignum.round |> Bignum.to_int_exn) in
    x, y
  in
  (* One [px] is the size of one pixel in figure space converted to
     wall space. This matters if the figure is being scaled up. *)
  let px =
    let open Bignum in
    round (one / scale) ~dir:`Nearest |> to_int_exn
  in
  (* Draw some info text. *)
  G.moveto (wall_x + wall_width + 10) (wall_y + wall_height - 10);
  G.set_color G.white;
  G.draw_string (sprintf !"Scale: %{Bignum#hum}" scale);
  (* Draw the actual hole (scaled to the size of the wall). *)
  G.set_color G.black;
  G.fill_poly
    (prob.hole
    |> List.map ~f:figure_to_wall_space
    |> List.map ~f:(fun (x, y) ->
           (* Center the vertex into the "pixel". *)
           x + (px / 2), y + (px / 2))
    |> Array.of_list);
  (* Draw the red stick figure. *)
  let scaled_vertices =
    prob.figure_vertices
    |> List.map ~f:figure_to_wall_space
    |> List.map ~f:(fun (x, y) ->
           (* Center the vertex into the "pixel". *)
           x + (px / 2), y + (px / 2))
    |> Array.of_list
  in
  G.set_color G.red;
  G.draw_segments
    (List.map prob.figure_edges ~f:(fun (idx1, idx2) ->
         try
           let x1, y1 = scaled_vertices.(idx1) in
           let x2, y2 = scaled_vertices.(idx2) in
           x1, y1, x2, y2
         with
         | _ -> failwithf "Failed to draw edge %d->%d" idx1 idx2 ())
    |> Array.of_list);
  (* Figure out where the mouse is in wall space and draw some info
     text. *)
  let mouse_x, mouse_y =
    match mouse with
    | None -> None, None
    | Some (mouse_x, mouse_y) ->
      if wall_x <= mouse_x
         && mouse_x < wall_x + wall_width
         && wall_y <= mouse_y
         && mouse_y < wall_y + wall_height
      then (
        let mouse_x =
          Bignum.( * ) (Bignum.of_int (mouse_x - wall_x)) scale |> Bignum.round ~dir:`Down
        in
        let mouse_y =
          Bignum.( * ) (Bignum.of_int (mouse_y - wall_y)) scale |> Bignum.round ~dir:`Down
        in
        Some mouse_x, Some mouse_y)
      else None, None
  in
  G.set_color G.white;
  G.moveto (wall_x + wall_width + 10) (wall_y + wall_height - 25);
  G.draw_string
    (sprintf !"Mouse X: %{Bignum#hum}" (Option.value mouse_x ~default:Bignum.zero));
  G.moveto (wall_x + wall_width + 10) (wall_y + wall_height - 40);
  G.draw_string
    (sprintf !"Mouse Y: %{Bignum#hum}" (Option.value mouse_y ~default:Bignum.zero));
  (* Draw a yellow rectangle around the mouse's current "pixel" in
     figure space. *)
  G.set_color G.yellow;
  let () =
    match mouse_x, mouse_y with
    | Some mouse_x, Some mouse_y ->
      let mouse_x, mouse_y = figure_to_wall_space (Point.create ~x:mouse_x ~y:mouse_y) in
      G.draw_rect mouse_x mouse_y px px
    | _ -> ()
  in
  ()
;;

(* Returns the mouse position, or [None] if it's outside the
   screen. *)
let get_mouse_pos () =
  let mouse_x, mouse_y = G.mouse_pos () in
  if mouse_x < 0 || mouse_x >= G.size_x () || mouse_y < 0 || mouse_y >= G.size_y ()
  then None
  else Some (mouse_x, mouse_y)
;;

let rec interact ~prob =
  draw_bg ();
  draw_problem
    ~prob
    ~wall_x:10
    ~wall_y:(G.size_y () - 30 - 700)
    ~wall_width:800
    ~wall_height:700
    ~mouse:(get_mouse_pos ());
  G.synchronize ();
  (* let status = G.wait_next_event [ G.Key_pressed; G.Mouse_motion ] in
   * printf
   *   !"Input: %{sexp#mach: Sexp.t}\n%!"
   *   [%sexp
   *     { mouse_x : int = status.mouse_x
   *     ; mouse_y : int = status.mouse_y
   *     ; button : bool = status.button
   *     ; keypressed : bool = status.keypressed
   *     ; key : char = status.key
   *     }]; *)
  let shutting_down = ref false in
  let () =
    while G.key_pressed () do
      match G.read_key () with
      | 'q' | '\027' -> shutting_down := true
      | ch -> printf "Ignoring pressed key: '%c'\n%!" ch
    done
  in
  if not !shutting_down
  then (
    let (_ : float) = Unix.nanosleep 0.033 in
    interact ~prob)
;;

let display ~filename =
  let prob = Problem.load_exn ~filename in
  G.open_graph " 1000x800";
  G.set_window_title "ICFPC 2021";
  G.auto_synchronize false;
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
