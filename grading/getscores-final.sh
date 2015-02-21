#!/bin/sh

for dir in `find "../studentfiles/" -name "Final-*-graded"`; do
    studentdir=`dirname $dir`
    student=`basename $studentdir`
    score=`tail -1 $dir/gradereport | head -1 | cut -d " " -f 11`
    echo "$student, $score"
done
