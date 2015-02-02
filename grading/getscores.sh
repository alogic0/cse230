#!/bin/sh

ASSN=$1

for dir in `find "studentfiles/" -iname "$ASSN-*-graded"`; do
    studentdir=`dirname $dir`
    student=`basename $studentdir`
    score=`tail -1 $dir/gradereport | cut -d " " -f 11`
    if ! expr $score + 0 &> /dev/null; then
        score="0"
    fi
    echo "$student, $score"
done
