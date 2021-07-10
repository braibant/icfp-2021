#!/bin/bash

set -e -o pipefail

SERVER="https://poses.live"
API_KEY="6a1b4728-a989-4a82-85af-3b344635c125"

function usage {
    echo "USAGE:"
    echo "  $0 test|get|send [PROBLEM_ID] [FILENAME]"
    exit 1
}

if [[ $# -lt 1 ]]; then
    usage
fi

case "$1" in
    test)
         curl -X GET "${SERVER}/api/hello" -H "Authorization: Bearer ${API_KEY}"
         ;;

    get)
        if [[ $# -lt 2 ]]; then
            usage
        fi
        curl -X GET "${SERVER}/api/problems/$2" -H "Authorization: Bearer ${API_KEY}"
        ;;
    send)
        if [[ $# -lt 3 ]]; then
            usage
        fi
        curl -X POST "${SERVER}/api/problems/$2/solutions" -H "Authorization: Bearer ${API_KEY}" -H "Content-Type: application/json" --data @"${3}"
        ;;
esac
