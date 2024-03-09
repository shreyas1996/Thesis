#!/bin/bash

(sml ${1}.sml & echo $! > pid) | tail -n +11 | while read line
do
    if [ "$line" == "-" ]; then
        kill -n 15 $(<pid)
        break
    fi
    if [[ $line == [* ]]; then
        echo $line
    fi
done
rm pid

