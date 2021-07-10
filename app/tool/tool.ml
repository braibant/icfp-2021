open! Core
open Icfpc2021

let score filename dislikes =
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

let commands =
  let open Command.Let_syntax in
  Command.group
    ~summary:"Vizualizer for ICFPC 2021"
    [ ( "score-estimate"
      , Command.basic
          ~summary:"estimate score for a problem"
          (let%map_open filename = anon ("FILE" %: Filename.arg_type)
           and dislikes = anon ("DISLIKES" %: float) in
           fun () -> score filename dislikes) )
    ; ( "dislikes"
      , Command.basic
          ~summary:"compute dislikes"
          (let%map_open problem = anon ("PROBLEM" %: Filename.arg_type)
           and answer = anon ("ANSWER" %: Filename.arg_type) in
           fun () ->
             let problem = Problem.load_exn ~filename:problem in
             let pose = Pose.load_exn ~problem ~filename:answer in
             printf "%d\n" (Pose.dislikes pose)) )
    ]
;;

let () = Command.run commands
