#!/bin/sh

ASSN=$1

for dir in `find "studentfiles/" -iname "$ASSN-*-graded"`; do
    studentdir=`dirname $dir`
    student=`basename $studentdir`
    score=`tail -1 $dir/gradereport | head -1 | cut -d " " -f 11`
    if test -z "$score"; then
      score="0"
    fi
    echo "$student, $score"
done
