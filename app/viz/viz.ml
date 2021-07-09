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

module State = struct
  type t =
    { selected_vertex : int option
    ; pose : Pose.t
    }

  let create ~pose = { selected_vertex = None; pose }
end

open State

(* Draw the hole, figure, and various info.  Note that the coordinate
   system is (0, 0) in the bottom left corner and growing rightwards
   and upwards.

   There are two coordinate systems here:
   - figure space where coordinates are in [Bignum.t]s, and
   - screen/wall space where coordinates are in [ints]

   Graphics API: https://ocaml.github.io/graphics/graphics/Graphics/index.html
*)
let draw_problem
    ~wall_x
    ~wall_y
    ~wall_width
    ~wall_height
    ~mouse
    ~mouse_clicked
    ~space_pressed
    ~state
  =
  let prob = Pose.problem state.pose in
  (* Draw a blue border around the wall. *)
  G.set_color (G.rgb 100 100 150);
  G.fill_rect wall_x wall_y wall_width wall_height;
  (* Make the wall smaller by the size of the border. *)
  let wall_y = wall_y + 4 in
  let wall_x = wall_x + 4 in
  let wall_height = wall_height - 8 in
  let wall_width = wall_width - 8 in
  (* Draw the wall grey background. *)
  G.set_color (G.rgb 100 100 100);
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
    let y =
      wall_y
      + wall_height
      - (Bignum.((y + one) / scale) |> Bignum.round |> Bignum.to_int_exn)
    in
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
          Bignum.( - )
            (Bignum.( * ) (Bignum.of_int (wall_y + wall_height - mouse_y)) scale)
            Bignum.one
          |> Bignum.round ~dir:`Up
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
  (* See if we've selected a vertex. *)
  let state =
    match mouse_clicked with
    | false ->
      let state =
        match state.selected_vertex with
        | None -> state
        | Some selected_vertex ->
          (match mouse_x, mouse_y with
          | Some mouse_x, Some mouse_y ->
            let mouse_point = Point.create ~x:mouse_x ~y:mouse_y in
            if Point.equal
                 mouse_point
                 (List.nth_exn (Pose.vertices state.pose) selected_vertex)
            then state
            else (
              let pose = Pose.move state.pose selected_vertex ~to_:mouse_point in
              { state with pose })
          | _, _ -> state)
      in
      state
    | true ->
      (match state.selected_vertex with
      | Some _ ->
        (* { state with selected_vertex = None } *)
        (* We only unselect when space is pressed.  Without this, the
            selection state flickers and it's annoying to fix in a
            better way. *)
        state
      | None ->
        let res =
          match mouse_x, mouse_y with
          | Some mouse_x, Some mouse_y ->
            let mouse_point = Point.create ~x:mouse_x ~y:mouse_y in
            let selected_idx, _distance =
              List.foldi
                ~init:(-1, Bignum.million)
                (Pose.vertices state.pose)
                ~f:(fun idx (best, best_distance) v ->
                  let d = Point.distance v mouse_point in
                  if Bignum.(d < best_distance) then idx, d else best, best_distance)
            in
            Some selected_idx
          | _, _ -> None
        in
        (match res with
        | None -> { state with selected_vertex = None }
        | Some idx -> { state with selected_vertex = Some idx }))
  in
  let state = if space_pressed then { state with selected_vertex = None } else state in
  G.set_color G.white;
  G.moveto (wall_x + wall_width + 10) (wall_y + wall_height - 55);
  G.draw_string
    (sprintf !"Selected vrtx: %d" (Option.value state.selected_vertex ~default:~-1));
  (* Draw the actual hole (scaled to the size of the wall). *)
  G.set_line_width (Int.max 1 (px / 2));
  G.set_color (G.rgb 200 200 200);
  let hole_vertices =
    prob.hole
    |> List.map ~f:figure_to_wall_space
    |> List.map ~f:(fun (x, y) ->
           (* Center the vertex into the "pixel". *)
           x + (px / 2), y + (px / 2))
  in
  G.fill_poly (Array.of_list hole_vertices);
  (* Draw the countour of the hole to account for the "line thickness"
     that is ignored by [fill_poly].  Without this, if the figure is
     scaled up, it looks like it doesn't fit in the hole. *)
  G.draw_segments
    (List.zip_exn hole_vertices (List.tl_exn hole_vertices @ [ List.hd_exn hole_vertices ])
    |> List.map ~f:(fun ((x1, y1), (x2, y2)) -> x1, y1, x2, y2)
    |> Array.of_list);
  (* Draw the red stick figure. *)
  let scaled_pose_vertices =
    Pose.vertices state.pose
    |> List.map ~f:figure_to_wall_space
    |> List.map ~f:(fun (x, y) ->
           (* Center the vertex into the "pixel". *)
           x + (px / 2), y + (px / 2))
    |> Array.of_list
  in
  G.set_color (G.rgb 100 175 100);
  G.draw_segments
    (List.map prob.figure_edges ~f:(fun (idx1, idx2) ->
         try
           let x1, y1 = scaled_pose_vertices.(idx1) in
           let x2, y2 = scaled_pose_vertices.(idx2) in
           x1, y1, x2, y2
         with
         | _ -> failwithf "Failed to draw edge %d->%d" idx1 idx2 ())
    |> Array.of_list);
  List.iter (Pose.invalid_edges state.pose) ~f:(fun ((idx1, idx2), off) ->
      let wrongness =
        let open Bignum in
        to_float (off - (prob.epsilon / of_int 1000000))
      in
      let wrongness = Float.max 0.0 wrongness |> Float.min 1.0 in
      let redness = 100 + Float.to_int (155. *. wrongness) in
      G.set_color (G.rgb redness 0 0);
      try
        let x1, y1 = scaled_pose_vertices.(idx1) in
        let x2, y2 = scaled_pose_vertices.(idx2) in
        G.draw_segments [| x1, y1, x2, y2 |]
      with
      | _ -> failwithf "Failed to draw edge %d->%d" idx1 idx2 ());
  G.set_line_width 1;
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
  (* Draw a green rectangle around the selected vertex. *)
  G.set_color G.green;
  let () =
    match state.selected_vertex with
    | Some selected_vertex ->
      let point_x, point_y =
        figure_to_wall_space (List.nth_exn (Pose.vertices state.pose) selected_vertex)
      in
      G.draw_rect point_x point_y px px
    | None -> ()
  in
  (* Help text *)
  G.set_color G.white;
  G.moveto 10 (wall_y - 20);
  G.draw_string (sprintf !"Click to select vertex");
  G.moveto 10 (wall_y - 35);
  G.draw_string (sprintf !"Press SPACE to deselect vertex");
  state
;;

(* Returns the mouse position, or [None] if it's outside the
   screen. *)
let get_mouse_pos () =
  let mouse_x, mouse_y = G.mouse_pos () in
  if mouse_x < 0 || mouse_x >= G.size_x () || mouse_y < 0 || mouse_y >= G.size_y ()
  then None
  else Some (mouse_x, mouse_y)
;;

let rec interact ~state ~answer_filename =
  let shutting_down = ref false in
  let space_pressed = ref false in
  let s_pressed = ref false in
  let () =
    while G.key_pressed () do
      match G.read_key () with
      | 'q' | '\027' -> shutting_down := true
      | ' ' -> space_pressed := true
      | 's' -> s_pressed := true
      | ch -> printf "Ignoring pressed key: '%c'\n%!" ch
    done
  in
  if !s_pressed
  then (
    Pose.save_exn state.pose ~filename:answer_filename;
    printf "Saved answer to %s!\n%!" answer_filename);
  draw_bg ();
  let state =
    draw_problem
      ~wall_x:10
      ~wall_y:(G.size_y () - 30 - 700)
      ~wall_width:800
      ~wall_height:700
      ~mouse:(get_mouse_pos ())
      ~mouse_clicked:(G.button_down ())
      ~space_pressed:!space_pressed
      ~state
  in
  G.synchronize ();
  if not !shutting_down
  then (
    let (_ : float) = Unix.nanosleep 0.033 in
    interact ~state ~answer_filename)
;;

let display ~filename ~answer_filename =
  let prob = Problem.load_exn ~filename in
  let pose =
    match answer_filename with
    | None -> Pose.create prob
    | Some answer_filename -> Pose.load_exn ~problem:prob ~filename:answer_filename
  in
  let answer_filename =
    match answer_filename with
    | Some answer_filename -> answer_filename
    | None -> Filename.chop_extension filename ^ ".answer.json"
  in
  G.open_graph " 1000x800";
  G.set_window_title "ICFPC 2021";
  G.auto_synchronize false;
  printf !"%{Problem#hum}\n%!" prob;
  let () = interact ~state:(State.create ~pose) ~answer_filename in
  G.close_graph ()
;;

let commands =
  let open Command.Let_syntax in
  Command.group
    ~summary:"Vizualizer for ICFPC 2021"
    [ ( "display"
      , Command.basic
          ~summary:"Display a problem"
          (let%map_open filename = anon ("FILE" %: Filename.arg_type)
           and answer_filename =
             flag "-answer" (optional string) ~doc:"FILE File containing an answer"
           in
           fun () -> display ~filename ~answer_filename) )
    ]
;;

let () = Command.run commands
