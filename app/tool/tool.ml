open! Core
open Icfpc2021

let score_estimate filename dislikes =
  let problem = Problem.load_exn ~filename in
  let score = Problem.score problem in
  let score our_dislikes =
    Float.(round_up (score * sqrt ((dislikes + 1.) / (our_dislikes + 1.))))
  in
  let best = score dislikes in
  let realistic = score Float.(2.0 * dislikes) in
  printf
    "Best score for this problem is %.0f points. Assuming 2x dislikes, we could score \
     %.0f points\n"
    best
    realistic
;;

let score ~dir problem_id =
  let problem_filename = Printf.sprintf "%s/prob%s.json" dir problem_id in
  let pose_filename = Printf.sprintf "%s/prob%s.answer.json" dir problem_id in
  let problem = Problem.load_exn ~filename:problem_filename in
  let pose = Pose.load_exn ~problem ~filename:pose_filename in
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
             let pose = Pose.load_exn ~problem ~filename:answer in
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
    ]
;;

let () = Command.run commands
