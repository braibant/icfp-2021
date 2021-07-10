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
        ./api.sh send "$p" "problems/prob$p.answer.json"
    done 
}

case "$1" in
    improved)
        improved ;;
    submit)
        improved | submit;;
    save)
        update;;
    *)
        echo "usage: $0 (improved|submit|save)"
esac

   
