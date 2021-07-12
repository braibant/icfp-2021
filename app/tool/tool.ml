open! Core
open Icfpc2021

let score_estimate filename dislikes =
  let problem = Problem.load_exn ~filename in
  let score = Problem.score problem in
  let score our_dislikes =
    Float.(round_up (score * sqrt ((dislikes + 1.) / (our_dislikes + 1.))))
  in
  let best = score dislikes in
  printf "%.0f\n" best
;;

let score ~dir problem_id =
  let problem_filename = Printf.sprintf "%s/prob%s.json" dir problem_id in
  let pose_filename = Printf.sprintf "%s/prob%s.answer.json" dir problem_id in
  let problem = Problem.load_exn ~filename:problem_filename in
  let pose, _ = Pose.load_exn ~problem ~filename:pose_filename in
  problem_id, Score.evaluate pose |> Option.value ~default:Int.max_value
;;

let score_all dir =
  let files = Sys.ls_dir dir in
  let problem_ids =
    List.filter_map files ~f:(fun filename ->
        if Filename.check_suffix filename ".answer.json"
        then (
          let filename = Filename.chop_suffix filename ".answer.json" in
          String.chop_prefix filename ~prefix:"prob")
        else None)
  in
  List.map problem_ids ~f:(fun problem_id -> score ~dir problem_id)
;;

exception Done

let commands =
  let open Command.Let_syntax in
  Command.group
    ~summary:"Vizualizer for ICFPC 2021"
    [ ( "score-estimate"
      , Command.basic
          ~summary:"estimate score for a problem"
          (let%map_open filename = anon ("FILE" %: Filename.arg_type)
           and dislikes = anon ("DISLIKES" %: float) in
           fun () -> score_estimate filename dislikes) )
    ; ( "dislikes"
      , Command.basic
          ~summary:"compute dislikes"
          (let%map_open problem = anon ("PROBLEM" %: Filename.arg_type)
           and answer = anon ("ANSWER" %: Filename.arg_type) in
           fun () ->
             let problem = Problem.load_exn ~filename:problem in
             let pose, _ = Pose.load_exn ~problem ~filename:answer in
             match Pose.invalid_edges pose with
             | [] -> printf "%d\n" (Pose.dislikes pose)
             | _ -> printf "999999999\n") )
    ; ( "all"
      , Command.basic
          ~summary:"compute all our scores"
          (let%map_open dir = anon ("DIR" %: Filename.arg_type)
           and state =
             flag "--state" (optional Filename.arg_type) ~doc:"STATE score state file"
           in
           fun () ->
             let scores = score_all dir |> Scores.create in
             match state with
             | None -> Scores.print scores
             | Some state -> Scores.save_exn scores state) )
    ; ( "improved"
      , Command.basic
          ~summary:"scores that have improved"
          (let%map_open dir = anon ("DIR" %: Filename.arg_type)
           and state =
             flag "--state" (required Filename.arg_type) ~doc:"STATE score state file"
           in
           fun () ->
             let scores = score_all dir |> Scores.create in
             let state = Scores.load_exn state in
             let improved = Scores.improved state scores in
             Scores.print improved) )
    ; ( "optimize"
      , Command.basic
          ~summary:"apply the optimizer"
          (let%map_open filename = anon ("POSE" %: Filename.arg_type) in
           fun () ->
             let stem = Filename.chop_suffix filename ".answer.json" in
             let pose_filename = stem ^ ".answer.json" in
             let problem_filename = stem ^ ".json" in
             let problem = Problem.load_exn ~filename:problem_filename in
             let pose, frozen = Pose.load_exn ~problem ~filename:pose_filename in
             let initial_dislike = Pose.dislikes pose in
             Printf.eprintf "Initial dislikes %i\n%!" initial_dislike;
             let pose = ref pose in
             if initial_dislike = 0
             then ()
             else (
               try
                 Sys.catch_break true;
                 while true do
                   match Optimizer.optimize1 !pose with
                   | Some p' ->
                     Printf.eprintf "Improved dislikes to %i\n%!" (Pose.dislikes p');
                     pose := p'
                   | None -> raise Done
                 done;
                 assert false
               with
               | Done | Sys.Break ->
                 let new_dislikes = Pose.dislikes !pose in
                 if new_dislikes < initial_dislike
                 then (
                   Printf.eprintf "Improved by %i\n%!" (initial_dislike - new_dislikes);
                   Pose.save_exn !pose ~frozen_vertices:frozen ~filename:pose_filename)
                 else Printf.eprintf "No improvement\n%!")) )
    ]
;;

let () = Command.run commands
