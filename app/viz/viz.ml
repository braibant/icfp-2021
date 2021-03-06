open! Core
module G = Graphics
open Icfpc2021

module Stats = struct
  let (period_length : Time_ns.Span.t) = Time_ns.Span.of_sec 1.0

  type t =
    { fps : float
    ; fps_since_last_period : int
    ; last_period_time : Time_ns.t
    ; solver_work : int
    ; solver_time : Time_ns.Span.t
    }

  let create () =
    { fps = 0.0
    ; fps_since_last_period = 0
    ; last_period_time = Time_ns.now ()
    ; solver_work = 0
    ; solver_time = Time_ns.Span.zero
    }
  ;;

  let frame_start t =
    let now = Time_ns.now () in
    let d_period = Time_ns.diff now t.last_period_time in
    let fps_since_last_period, fps, last_period_time =
      if Time_ns.Span.( > ) d_period period_length
      then
        ( 0
        , Int.to_float (t.fps_since_last_period + 1)
          /. (Time_ns.Span.to_sec d_period /. Time_ns.Span.to_sec period_length)
        , now )
      else t.fps_since_last_period + 1, t.fps, t.last_period_time
    in
    { t with fps; fps_since_last_period; last_period_time }
  ;;

  let reset_solver t = { t with solver_work = 0; solver_time = Time_ns.Span.zero }

  let time_solver t ~work ~f =
    let start = Time_ns.now () in
    let res = f () in
    let t =
      { t with
        solver_work = t.solver_work + work
      ; solver_time =
          Time_ns.Span.( + ) t.solver_time (Time_ns.diff (Time_ns.now ()) start)
      }
    in
    res, t
  ;;

  let solver_speed t = Float.of_int t.solver_work /. Time_ns.Span.to_sec t.solver_time
end

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

module Operation = struct
  type t =
    | Move_points of Pose.t (* we moved some point (manually or via solve) in this pose *)
    | Change_frozen of Int.Set.t
  (* we changed this set of frozen points *)
end

module State = struct
  type t =
    { selected_vertex : int option
    ; drag_physics : bool
    ; repulsion_physics : bool
    ; manually_frozen_vertices : Int.Set.t
    ; pose : Pose.t
    ; history : Operation.t list
    ; scale : Bignum.t
    ; view_offset : int * int
    }

  let create ~pose ~manually_frozen_vertices =
    { selected_vertex = None
    ; drag_physics = false
    ; repulsion_physics = false
    ; pose
    ; manually_frozen_vertices
    ; history = []
    ; scale = Bignum.one
    ; view_offset = 0, 0
    }
  ;;

  let reset t =
    { t with
      history =
        Change_frozen t.manually_frozen_vertices :: Move_points t.pose :: t.history
    ; manually_frozen_vertices = Int.Set.empty
    ; selected_vertex = None
    ; pose = Pose.set_vertices t.pose (Pose.problem t.pose).Problem.figure_vertices
    }
  ;;

  let shift t (dx, dy) =
    if dx = 0 && dy = 0
    then t
    else
      { t with
        pose = Pose.shift t.pose ~frozen:t.manually_frozen_vertices (dx, dy)
      ; history = Move_points t.pose :: t.history
      }
  ;;

  let undo t =
    match t.history with
    | [] -> t
    | Move_points pose :: rest -> { t with history = rest; pose }
    | Change_frozen manually_frozen_vertices :: rest ->
      { t with history = rest; manually_frozen_vertices }
  ;;

  let find_hole_vertex_near t ~x ~y =
    let mouse_point = Point.create ~x ~y in
    let start = Point.create ~x:Bignum.tenth ~y:Bignum.tenth in
    let v, _ =
      List.fold
        ~init:(Point.create ~x:Bignum.zero ~y:Bignum.zero, Bignum.million)
        (Pose.problem t.pose).Problem.hole
        ~f:(fun (best, best_distance) v ->
          let d = Point.sq_distance v mouse_point in
          if Bignum.(d < best_distance) then v, d else best, best_distance)
    in
    if Point.equal v start then None else Some v
  ;;

  let snap_to_closest t =
    match t.selected_vertex with
    | None -> t
    | Some idx ->
      let curr = Pose.vertex t.pose idx in
      (match find_hole_vertex_near t ~x:curr.x ~y:curr.y with
      | None -> t
      | Some p ->
        { t with
          selected_vertex = None
        ; pose = Pose.move t.pose idx ~to_:p
        ; history = Move_points t.pose :: t.history
        })
  ;;

  let reflect_vertical t =
    { t with
      pose = Pose.reflect_vertical t.pose
    ; history = Move_points t.pose :: t.history
    }
  ;;

  let rotate t =
    { t with
      pose = Pose.reflect_vertical (Pose.transpose t.pose)
    ; history = Move_points t.pose :: t.history
    }
  ;;

  let find_vertex_near_mouse state ~mouse_x ~mouse_y =
    let mouse_point = Point.create ~x:mouse_x ~y:mouse_y in
    let idx, _ =
      Map.fold
        ~init:(-1, Bignum.million)
        (Pose.vertices state.pose)
        ~f:(fun ~key:idx ~data:v (best, best_distance) ->
          let d = Point.sq_distance v mouse_point in
          if Bignum.(d < best_distance) then idx, d else best, best_distance)
    in
    if idx = -1 then None else Some idx
  ;;

  let update_manually_frozen_vertices state ~mouse_x ~mouse_y =
    match state.selected_vertex with
    | Some _ -> state
    | None ->
      (match mouse_x, mouse_y with
      | None, _ | _, None -> state
      | Some mouse_x, Some mouse_y ->
        (match find_vertex_near_mouse state ~mouse_x ~mouse_y with
        | None -> state
        | Some idx ->
          let old_frozen = state.manually_frozen_vertices in
          let manually_frozen_vertices =
            if Set.mem state.manually_frozen_vertices idx
            then Set.remove state.manually_frozen_vertices idx
            else Set.add state.manually_frozen_vertices idx
          in
          { state with
            manually_frozen_vertices
          ; history = Operation.Change_frozen old_frozen :: state.history
          }))
  ;;

  let fit_idx = ref 0
  let reset_fit () = fit_idx := 0

  let fit_unique_edge t =
    match
      Pose.find_pose_edge_that_matches_hole_edge t.pose ~frozen:t.manually_frozen_vertices
    with
    | [] -> t
    | hole_edge_matches ->
      (* This is a very ad-hoc way make fit - undo - fit cycle though
          options *)
      let num_matches = List.length hole_edge_matches in
      if !fit_idx >= num_matches then fit_idx := 0;
      let (hole_from, hole_to), (from_p, to_p) =
        List.nth_exn hole_edge_matches !fit_idx
      in
      printf "'=': match %d out of %d\n%!" !fit_idx num_matches;
      incr fit_idx;
      { t with
        history =
          Change_frozen t.manually_frozen_vertices :: Move_points t.pose :: t.history
      ; pose = Pose.move (Pose.move t.pose from_p ~to_:hole_from) to_p ~to_:hole_to
      ; manually_frozen_vertices =
          Set.add (Set.add t.manually_frozen_vertices from_p) to_p
      }
  ;;

  let randomize t =
    let vertices = Pose.vertices t.pose in
    let max_x, max_y = Problem.max_xy (Pose.problem t.pose) in
    let max_x = Bignum.to_int_exn max_x in
    let max_y = Bignum.to_int_exn max_y in
    Random.self_init ();
    let vertices =
      Map.mapi vertices ~f:(fun ~key:idx ~data:p ->
          if Set.mem t.manually_frozen_vertices idx
          then p
          else
            Point.create
              ~x:(Bignum.of_int (Random.int max_x))
              ~y:(Bignum.of_int (Random.int max_y)))
    in
    { t with
      pose = Pose.set_vertices t.pose (Map.data vertices)
    ; history = Move_points t.pose :: t.history
    }
  ;;

  let freeze_all t =
    { t with
      manually_frozen_vertices = Pose.vertices t.pose |> Map.keys |> Int.Set.of_list
    ; history = Change_frozen t.manually_frozen_vertices :: t.history
    }
  ;;

  let unfreeze_all t =
    { t with
      manually_frozen_vertices = Int.Set.empty
    ; history = Change_frozen t.manually_frozen_vertices :: t.history
    }
  ;;

  let pan t (dx, dy) =
    let x, y = t.view_offset in
    { t with view_offset = x - dx, y - dy }
  ;;

  let spring_physics t =
    let forces = Physics.edges t.pose ~frozen:t.manually_frozen_vertices in
    let vertices = Physics.relax_one t.pose forces in
    { t with
      pose = Pose.set_vertices' t.pose vertices
    ; history = Move_points t.pose :: t.history
    }
  ;;

  let drag_physics t ~vertex ~distance ~dampening_factor =
    let pose = ref t.pose in
    for _i = 0 to 19 do
      let forces =
        Physics.drag
          !pose
          ~frozen:t.manually_frozen_vertices
          ~vertex
          ~distance
          ~dampening_factor
      in
      let vertices = Physics.relax_one !pose forces in
      pose := Pose.set_vertices' t.pose vertices
    done;
    let vertices = Pose.vertices !pose in
    { t with
      pose = Pose.set_vertices' t.pose vertices
    ; history = Move_points t.pose :: t.history
    }
  ;;

  let toggle_drag_physics t = { t with drag_physics = not t.drag_physics }

  let repulsion_physics t ~vertex =
    let pose = ref t.pose in
    for _i = 0 to 19 do
      let forces = Physics.electrify !pose ~vertex ~frozen:t.manually_frozen_vertices in
      let vertices = Physics.relax_one !pose forces in
      pose := Pose.set_vertices' t.pose vertices
    done;
    let vertices = Pose.vertices !pose in
    { t with
      pose = Pose.set_vertices' t.pose vertices
    ; history = Move_points t.pose :: t.history
    }
  ;;

  let toggle_repulsion_physics t = { t with repulsion_physics = not t.repulsion_physics }

  let optimize t =
    Printf.eprintf "Calling the optimizer\n%!";
    match Optimizer.optimize1 t.pose with
    | Some pose -> { t with pose; history = Move_points t.pose :: t.history }
    | None -> t
  ;;
end

open State

let draw_wall ~wall_x ~wall_y ~wall_width ~wall_height =
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
  wall_x, wall_y, wall_height, wall_width
;;

let compute_scale ~prob ~wall_width ~wall_height =
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
  Bignum.max scale_x scale_y
;;

let draw_right_text ~wall_x ~wall_y ~wall_width ~wall_height ~right_text_count str =
  G.moveto
    (wall_x + wall_width + 10)
    (wall_y + wall_height - 10 - (15 * !right_text_count));
  incr right_text_count;
  G.set_color G.white;
  G.draw_string str
;;

let draw_bottom_text ~x ~wall_y ~bottom_text_count str =
  G.set_color G.white;
  G.moveto x (wall_y - 20 - (15 * (!bottom_text_count mod 7)));
  incr bottom_text_count;
  G.draw_string str
;;

let update_select_and_move_vertex state ~mouse_clicked ~mouse_x ~mouse_y ~space_pressed =
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
            if Point.equal mouse_point (Pose.vertex state.pose selected_vertex)
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
          | Some mouse_x, Some mouse_y -> find_vertex_near_mouse state ~mouse_x ~mouse_y
          | _, _ -> None
        in
        (match res with
        | None -> { state with selected_vertex = None }
        | Some idx ->
          { state with
            selected_vertex = Some idx
          ; history = Operation.Move_points state.pose :: state.history
          }))
  in
  if space_pressed then { state with selected_vertex = None } else state
;;

let mouse_to_figure_space
    ~mouse
    ~scale
    ~wall_x
    ~wall_y
    ~wall_width
    ~wall_height
    ~view_offset_x
    ~view_offset_y
  =
  match mouse with
  | None -> None, None
  | Some (mouse_x, mouse_y) ->
    if wall_x <= mouse_x
       && mouse_x < wall_x + wall_width
       && wall_y <= mouse_y
       && mouse_y < wall_y + wall_height
    then (
      let mouse_x =
        Bignum.( * ) (Bignum.of_int (mouse_x - wall_x - view_offset_x)) scale
        |> Bignum.round ~dir:`Down
      in
      let mouse_y =
        Bignum.( - )
          (Bignum.( * )
             (Bignum.of_int (wall_y + wall_height + view_offset_y - mouse_y))
             scale)
          Bignum.one
        |> Bignum.round ~dir:`Up
      in
      Some mouse_x, Some mouse_y)
    else None, None
;;

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
    ~f_pressed
    ~alternative_offsets
    ~show_alternative_offsets
    ~state
    ~(stats : Stats.t)
    ~solver_kind_idx
  =
  let prob = Pose.problem state.pose in
  let wall_x, wall_y, wall_height, wall_width =
    draw_wall ~wall_x ~wall_y ~wall_height ~wall_width
  in
  let scale = Bignum.( * ) state.scale (compute_scale ~prob ~wall_width ~wall_height) in
  let figure_to_wall_space Point.{ x; y } =
    let x =
      fst state.view_offset
      + wall_x
      + (Bignum.(x / scale) |> Bignum.round |> Bignum.to_int_exn)
    in
    let y =
      snd state.view_offset
      + wall_y
      + wall_height
      - (Bignum.((y + one) / scale) |> Bignum.round |> Bignum.to_int_exn)
    in
    x, y
  in
  (* One [px] is the size of one pixel in figure space converted to
     wall space. This matters if the figure is being scaled up. *)
  let px =
    let open Bignum in
    round (one + one + (one / scale)) ~dir:`Nearest |> to_int_exn
  in
  let draw_right_text =
    let right_text_count = ref 0 in
    fun str ->
      draw_right_text ~wall_x ~wall_y ~wall_height ~wall_width ~right_text_count str
  in
  draw_right_text (sprintf !"FPS: %.0f" stats.fps);
  draw_right_text (sprintf !"Solver: %{Solver.Kind}" Solver.Kind.all.(solver_kind_idx));
  draw_right_text (sprintf !"Solver speed: %.0f" (Stats.solver_speed stats));
  draw_right_text (sprintf !"Scale: %{Bignum#hum}" scale);
  let mouse_x, mouse_y =
    mouse_to_figure_space
      ~mouse
      ~scale
      ~wall_x
      ~wall_y
      ~wall_width
      ~wall_height
      ~view_offset_x:(fst state.view_offset)
      ~view_offset_y:(snd state.view_offset)
  in
  draw_right_text
    (sprintf !"Mouse X: %{Bignum#hum}" (Option.value mouse_x ~default:Bignum.zero));
  draw_right_text
    (sprintf !"Mouse Y: %{Bignum#hum}" (Option.value mouse_y ~default:Bignum.zero));
  let state =
    update_select_and_move_vertex state ~mouse_clicked ~mouse_x ~mouse_y ~space_pressed
  in
  let state =
    match f_pressed with
    | false -> state
    | true -> update_manually_frozen_vertices state ~mouse_x ~mouse_y
  in
  draw_right_text
    (sprintf !"Selected vrtx: %d" (Option.value state.selected_vertex ~default:~-1));
  (* Show the number of neighbours for a given vertex *)
  draw_right_text
    (sprintf
       !"Neighbours: %d"
       (Option.value_map state.selected_vertex ~default:~-1 ~f:(fun x ->
            Map.find_exn (Pose.neighbours state.pose) x |> List.length)));
  draw_right_text (sprintf !"Dislikes: %d" (Pose.dislikes state.pose));
  draw_right_text (sprintf !"Drag_physics: %b" state.drag_physics);
  draw_right_text (sprintf !"Repulsion_physics: %b" state.repulsion_physics);
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
  (* Draw a white halo around hole points because Dmitry asked for it. *)
  G.set_color G.white;
  G.set_line_width (Int.max 1 (px / 4));
  List.iter hole_vertices ~f:(fun (x, y) -> G.draw_circle x y 6);
  (* Draw the bonuses *)
  List.iter prob.bonuses ~f:(fun bonus ->
      let x, y = figure_to_wall_space bonus.position in
      G.set_color (G.rgb 255 200 100);
      G.fill_circle (x + (px / 2)) (y + (px / 2)) 15;
      G.set_color G.white;
      G.fill_rect x y px px;
      G.set_color G.black;
      G.moveto (x + px + 2) (y + (px / 2) - 5);
      G.draw_string (sprintf "%s %d" (Bonus_kind.to_string bonus.kind) bonus.problem));
  (* Draw the red stick figure. *)
  G.set_line_width (Int.max 1 (px / 2));
  let scaled_pose_vertices =
    Pose.vertices state.pose
    |> Map.data
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
        to_float (off - (prob.epsilon / million))
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
  G.set_line_width (Int.max 1 (px / 8));
  List.iter prob.figure_edges ~f:(fun ((idx1, idx2) as edge) ->
      if not (Pose.edge_inside_hole state.pose edge)
      then (
        G.set_color (G.rgb 100 50 100);
        try
          let x1, y1 = scaled_pose_vertices.(idx1) in
          let x2, y2 = scaled_pose_vertices.(idx2) in
          G.draw_segments [| x1, y1, x2, y2 |]
        with
        | _ -> failwithf "Failed to draw edge %d->%d" idx1 idx2 ()));
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
        figure_to_wall_space (Pose.vertex state.pose selected_vertex)
      in
      G.draw_rect point_x point_y px px
    | None -> ()
  in
  (* Draw circles around the vertices connected to the selected vertex
     for how long their edges can be. *)
  let length_sq_to_wall_space len =
    let len = Float.sqrt (Bignum.to_float len) |> Bignum.of_float_dyadic in
    let open Bignum in
    len / scale |> Bignum.round |> Bignum.to_int_exn
  in
  let () =
    match state.selected_vertex with
    | Some selected_vertex ->
      let connected_points =
        List.filter_map prob.figure_edges ~f:(fun ((idx1, idx2) as edge) ->
            if idx1 = selected_vertex
            then Some (idx2, idx1, Pose.min_max_length_sq_for_edge state.pose edge)
            else if idx2 = selected_vertex
            then Some (idx1, idx2, Pose.min_max_length_sq_for_edge state.pose edge)
            else None)
      in
      List.iter
        connected_points
        ~f:(fun (point_idx, other_idx, (min_edge_sq, max_edge_sq)) ->
          let point = Pose.vertex state.pose point_idx in
          let point_x, point_y = figure_to_wall_space point in
          G.set_color (G.rgb 150 150 0);
          G.draw_circle point_x point_y (length_sq_to_wall_space min_edge_sq);
          G.set_color (G.rgb 255 255 0);
          G.draw_circle point_x point_y (length_sq_to_wall_space max_edge_sq);
          if show_alternative_offsets
          then (
            let r = 55 + (Int.hash point_idx mod 200) in
            let g = 55 + (Int.hash other_idx mod 200) in
            let b = 100 + (75 / (1 + point_idx)) in
            G.set_color (G.rgb r g b);
            let alternative_offsets =
              Alternative_offsets.find alternative_offsets point_idx other_idx
            in
            List.iter alternative_offsets ~f:(fun (dx, dy) ->
                let point_x, point_y =
                  figure_to_wall_space
                    (Point.create ~x:Bignum.(point.x + dx) ~y:Bignum.(point.y + dy))
                in
                G.fill_rect (point_x + (px / 2) - 1) (point_y + (px / 2) - 1) 3 3)))
    | None -> ()
  in
  (* Draw black squares around manually frozen vertices. *)
  G.set_color G.black;
  let () =
    Int.Set.iter state.manually_frozen_vertices ~f:(fun idx ->
        let point_x, point_y = figure_to_wall_space (Pose.vertex state.pose idx) in
        G.draw_rect (point_x + (px / 2) - 5) (point_y + (px / 2) - 5) 11 11)
  in
  (* Help text *)
  let draw_bottom_text =
    let bottom_text_count = ref 0 in
    fun str ->
      draw_bottom_text
        ~x:(10 + (290 * (!bottom_text_count / 7)))
        ~wall_y
        ~bottom_text_count
        str
  in
  draw_bottom_text (sprintf !"Click to select vertex");
  draw_bottom_text (sprintf !"Press SPACE to deselect vertex");
  draw_bottom_text (sprintf !"Press s to save");
  draw_bottom_text (sprintf !"Press f to freeze highlighted vertex");
  draw_bottom_text (sprintf !"Press F to freeze ALL vertices");
  draw_bottom_text (sprintf !"Press U to unfreeze ALL vertices");
  draw_bottom_text (sprintf !"Press AWSD to shift");
  draw_bottom_text (sprintf !"Press o to show alternative offsets");
  draw_bottom_text (sprintf !"Press O to hide alternative offsets");
  draw_bottom_text (sprintf !"Press z to undo");
  draw_bottom_text (sprintf !"Press v to snap to closest hole vertex");
  draw_bottom_text (sprintf !"Press | to reflect vertically");
  draw_bottom_text (sprintf !"Press > to rotate clockwise");
  draw_bottom_text (sprintf !"Press = to print edge that matches hole edge");
  draw_bottom_text (sprintf !"Press R to reset the problem");
  draw_bottom_text (sprintf !"Press * to randomize unfrozen vertices");
  draw_bottom_text (sprintf !"Press +-0,JKLI to zoom/pan");
  draw_bottom_text (sprintf !"Press p/P to enable/disable spring physics");
  draw_bottom_text (sprintf !"Press d to toggle spring physics dragging");
  draw_bottom_text (sprintf !"Press e to toggle repulsion physics");
  draw_bottom_text (sprintf !"Press i to micro-optimize one point");
  draw_bottom_text (sprintf !"Press TAB to cycle solvers");
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

let rec interact
    ~state
    ~stats
    ~answer_filename
    ~alternative_offsets
    ~show_alternative_offsets
    ~solver
    ~work_per_frame
    ~solver_kind_idx
  =
  let stats = Stats.frame_start stats in
  let shutting_down = ref false in
  let space_pressed = ref false in
  let s_pressed = ref false in
  let f_pressed = ref false in
  let z_pressed = ref false in
  let v_pressed = ref false in
  let vbar_pressed = ref false in
  let rotate_pressed = ref false in
  let fit_pressed = ref false in
  let reset_pressed = ref false in
  let random_pressed = ref false in
  let freeze_all_pressed = ref false in
  let unfreeze_all_pressed = ref false in
  let scale_up_pressed = ref false in
  let scale_down_pressed = ref false in
  let normal_scale_pressed = ref false in
  let pan_left = ref false in
  let pan_right = ref false in
  let pan_down = ref false in
  let pan_up = ref false in
  let start_solver = ref false in
  let stop_solver = ref false in
  let spring_physics = ref 0 in
  let toggle_drag_physics = ref false in
  let toggle_repulsion_physics = ref false in
  let shift = ref (0, 0) in
  let tab_pressed = ref false in
  let optimize = ref false in
  let () =
    while G.key_pressed () do
      match G.read_key () with
      | 'q' | '\027' -> shutting_down := true
      | ' ' -> space_pressed := true
      | '\t' -> tab_pressed := true
      | 's' -> s_pressed := true
      | 'f' -> f_pressed := true
      | 'F' -> freeze_all_pressed := true
      | 'U' -> unfreeze_all_pressed := true
      | 'A' -> shift := -10, 0
      | 'S' -> shift := 0, 10
      | 'D' -> shift := 10, 0
      | 'W' -> shift := 0, -10
      | 'o' -> show_alternative_offsets := true
      | 'O' -> show_alternative_offsets := false
      | 'z' -> z_pressed := true
      | 'v' -> v_pressed := true
      | '|' -> vbar_pressed := true
      | '>' -> rotate_pressed := true
      | '=' -> fit_pressed := true
      | '/' -> State.reset_fit ()
      | 'R' -> reset_pressed := true
      | '*' -> random_pressed := true
      | '+' -> scale_up_pressed := true
      | '-' -> scale_down_pressed := true
      | '0' -> normal_scale_pressed := true
      | 'J' -> pan_left := true
      | 'L' -> pan_right := true
      | 'K' -> pan_down := true
      | 'I' -> pan_up := true
      | 'g' -> start_solver := true
      | 'G' -> stop_solver := true
      | 'p' -> spring_physics := 1
      | 'P' -> spring_physics := 100
      | 'd' -> toggle_drag_physics := true
      | 'e' -> toggle_repulsion_physics := true
      | 'i' -> optimize := true
      | ch -> printf "Ignoring pressed key: '%c'\n%!" ch
    done
  in
  if !s_pressed
  then (
    Pose.save_exn
      state.pose
      ~frozen_vertices:state.manually_frozen_vertices
      ~filename:answer_filename;
    printf "Saved answer to %s!\n%!" answer_filename;
    let invalid_edges = Pose.invalid_edges state.pose in
    if List.length invalid_edges > 0
    then printf "WARNING: Invalid edges: %d\n%!" (List.length invalid_edges));
  draw_bg ();
  let state = State.shift state !shift in
  let state = if !z_pressed then State.undo state else state in
  let state = if !v_pressed then State.snap_to_closest state else state in
  let state = if !vbar_pressed then State.reflect_vertical state else state in
  let state = if !rotate_pressed then State.rotate state else state in
  let state = if !reset_pressed then State.reset state else state in
  let state = if !fit_pressed then State.fit_unique_edge state else state in
  let state = if !random_pressed then State.randomize state else state in
  let state = if !freeze_all_pressed then State.freeze_all state else state in
  let state = if !unfreeze_all_pressed then State.unfreeze_all state else state in
  let state = if !toggle_drag_physics then State.toggle_drag_physics state else state in
  let state = if !optimize then State.optimize state else state in
  let state =
    if !toggle_repulsion_physics then State.toggle_repulsion_physics state else state
  in
  let state =
    let state = ref state in
    while !spring_physics > 0 do
      state := State.spring_physics !state;
      spring_physics := !spring_physics - 1
    done;
    !state
  in
  let state =
    if state.drag_physics && Option.is_some state.selected_vertex
    then (
      let vertex = Option.value_exn state.selected_vertex in
      State.drag_physics state ~vertex ~distance:5 ~dampening_factor:0.95)
    else state
  in
  let state =
    if state.repulsion_physics && Option.is_some state.selected_vertex
    then (
      let vertex = Option.value_exn state.selected_vertex in
      State.repulsion_physics state ~vertex)
    else state
  in
  let state =
    if !scale_up_pressed
    then { state with scale = Bignum.(state.scale - tenth) }
    else state
  in
  let state =
    if !scale_down_pressed
    then { state with scale = Bignum.(state.scale + tenth) }
    else state
  in
  let state =
    if !normal_scale_pressed
    then { state with scale = Bignum.one; view_offset = 0, 0 }
    else state
  in
  let state = if !pan_left then State.pan state (-100, 0) else state in
  let state = if !pan_right then State.pan state (100, 0) else state in
  let state = if !pan_down then State.pan state (0, -100) else state in
  let state = if !pan_up then State.pan state (0, 100) else state in
  let state =
    draw_problem
      ~wall_x:10
      ~wall_y:(G.size_y () - 30 - 700)
      ~wall_width:800
      ~wall_height:700
      ~mouse:(get_mouse_pos ())
      ~mouse_clicked:(G.button_down ())
      ~space_pressed:!space_pressed
      ~f_pressed:!f_pressed
      ~alternative_offsets
      ~show_alternative_offsets:!show_alternative_offsets
      ~state
      ~stats
      ~solver_kind_idx
  in
  let solver = if !stop_solver then None else solver in
  let solver, solver_kind_idx =
    if !tab_pressed
    then None, (solver_kind_idx + 1) mod Array.length Solver.Kind.all
    else solver, solver_kind_idx
  in
  let solver, state, stats =
    let frozen_vertices_count = Set.length state.manually_frozen_vertices in
    if !start_solver
       && Option.is_none solver
       && frozen_vertices_count < Map.length (Pose.vertices state.pose)
       && 0 < frozen_vertices_count
    then (
      let solver =
        Solver.create
          Solver.Kind.all.(solver_kind_idx)
          ~initial_pose:state.pose
          ~manually_frozen_vertices:state.manually_frozen_vertices
          ~alternative_offsets
      in
      printf "Invoking solver...\n%!";
      ( Some solver
      , { state with history = Move_points state.pose :: state.history }
      , Stats.reset_solver stats ))
    else solver, state, stats
  in
  G.synchronize ();
  let state, solver, stats =
    match solver with
    | None -> state, solver, stats
    | Some solver ->
      let solver_res, stats =
        Stats.time_solver stats ~work:work_per_frame ~f:(fun () ->
            Solver.run solver ~work_to_do:work_per_frame)
      in
      (match solver_res with
      | `Done solver ->
        printf "Solving done\n%!";
        { state with pose = Solver.pose solver }, None, stats
      | `Todo solver -> { state with pose = Solver.pose solver }, Some solver, stats
      | `Failed ->
        printf "ERROR: Solving failed\n%!";
        state, None, stats)
  in
  if not !shutting_down
  then (
    let (_ : float) = Unix.nanosleep 0.033 in
    interact
      ~state
      ~stats
      ~answer_filename
      ~alternative_offsets
      ~show_alternative_offsets
      ~solver
      ~solver_kind_idx
      ~work_per_frame)
;;

let display ~filename ~answer_filename ~no_alternative_offsets ~work_per_frame =
  let prob = Problem.load_exn ~filename in
  let alternative_offsets =
    if no_alternative_offsets
    then Alternative_offsets.empty
    else Alternative_offsets.create prob
  in
  let pose, manually_frozen_vertices =
    match answer_filename with
    | None -> Pose.create prob, Int.Set.empty
    | Some answer_filename -> Pose.load_exn ~problem:prob ~filename:answer_filename
  in
  let answer_filename =
    match answer_filename with
    | Some answer_filename -> answer_filename
    | None -> Filename.chop_extension filename ^ ".answer.json"
  in
  G.open_graph " 1000x850";
  G.set_window_title "ICFPC 2021";
  G.auto_synchronize false;
  printf !"%{Problem#hum}\n%!" prob;
  let stats = Stats.create () in
  let () =
    interact
      ~state:(State.create ~pose ~manually_frozen_vertices)
      ~stats
      ~answer_filename
      ~alternative_offsets
      ~show_alternative_offsets:(ref false)
      ~solver:None
      ~solver_kind_idx:0
      ~work_per_frame
  in
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
           and no_alternative_offsets =
             flag
               "-no-alternative-offsets"
               no_arg
               ~doc:" Don't compute alternative offsets"
           and work_per_frame =
             flag
               "-work-to-do"
               (optional_with_default 10 int)
               ~doc:"INT The amount of work the solver should do per animation frame"
           in
           fun () ->
             display ~filename ~answer_filename ~no_alternative_offsets ~work_per_frame) )
    ]
;;

let () = Command.run commands
