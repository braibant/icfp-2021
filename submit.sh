#!/bin/bash

set -e -o pipefail


function improved () {
    dune exec app/tool/tool.exe -- improved problems --state score
}

function update () {
    dune exec app/tool/tool.exe -- all problems --state score
}

function submit(){
    while IFS=, read -r p new
    do
        echo "Submitting problem ${p} (score ${new})"
        echo ./api.sh send "$p" "problems/$p.answer.json"
    done 
}

case "$1" in
    improved)
        improved ;;
    run)
        improved | submit;;
    update)
        update;;
    *)
        echo "usage: $0 (improved|run|update)"
esac

   
