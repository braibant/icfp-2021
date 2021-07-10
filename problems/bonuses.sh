#!/bin/bash
echo "digraph bonuses {" > bonuses.dot
(for i in prob*.json ; do
    [ "${i/answer//}" != "${i}" ] && continue
    i=${i##prob}
    i=${i%%.json}
    jq --arg p $i '.bonuses[] | "p\($p)->p\(.problem)[label=\"\(.bonus)\"]"' prob${i}.json -r
 done) >> bonuses.dot
echo "}" >> bonuses.dot

dot -Tpng -obonuses.png bonuses.dot
