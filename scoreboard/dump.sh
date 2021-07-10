#!/bin/bash
while true ; do
  date > scoreboard.txt
  links -dump https://poses.live/teams >> scoreboard.txt
  git commit -m "scoreboard update" scoreboard.txt
  sleep $(( 30 * 60 ))
done
