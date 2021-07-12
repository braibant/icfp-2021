#!/bin/bash
p="$1"

problem="prob${p}.json"
[ -f "prob${p}.json" ] || { echo "USAGE: $0 PROBLEM_ID"; exit 1 ; }

answer="prob${p}.answer.json"
if [ -f "${answer}" ] ; then
    extra_arg="-answer ${answer}"
    current_dislikes=$(../_build/default/app/tool/tool.exe dislikes $problem $answer)
else
    current_dislikes=99999999
    extra_arg=""
fi

dune exec ../app/tool/tool.exe optimize $answer

if [ $? -eq 0 ] ; then
    if [ -f "${answer}" ] ; then
        new_dislikes=$(../_build/default/app/tool/tool.exe dislikes $problem $answer)
    fi
    echo "======================================="
    if [ "$new_dislikes" -lt "$current_dislikes" ] ; then
        echo "Looks like you improved the solution:"
        echo "Was: $current_dislikes"
        echo "Now: $new_dislikes"
        echo "Impovement: $(( $current_dislikes - $new_dislikes ))"
        echo -n "Do you want to commit and submit? [Yn] "
        read foo
        if [ "$ans" = "n" -o "$ans" = "N" ] ; then
            echo "NOT SUBMITTING"
        else
            echo "Committing..."
            git add $answer
            git commit -m "Impovement for $p: $(( $current_dislikes - $new_dislikes ))" $answer
            echo "Submitting..."
            ../api.sh send $p $answer
            echo "DONE"
        fi
    else
        echo "Looks like solution is no better than what we have"
        echo "Was: $current_dislikes"
        echo "Now: $new_dislikes"
        echo "Impovement: $(( $current_dislikes - $new_dislikes ))"
        echo "I am going to revert any changes to the answer just in case"
        #git checkout $answer
    fi
fi
