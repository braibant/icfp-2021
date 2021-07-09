#!/bin/bash
from_=${1:-1}
to_=${2:-59}

for i in `seq ${from_} ${to_}` ; do
    echo -n "Getting $i ..."
    ../scripts/get_problem $i > $i.json
    echo "done"
done
