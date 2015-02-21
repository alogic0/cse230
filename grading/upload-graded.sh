#!/bin/sh

ASSN=$1

for dir in `find "studentfiles/" -iname "$ASSN-*-graded"`; do
     echo "$dir"
     rm -f $dir/grade$ASSN
     rm -f $dir/*.o
     rm -f $dir/*.hi
     rm -f $dir/*~
    
     graded=`basename $dir`
     studentdir=`dirname $dir`
     student=`basename $studentdir`

     scp -r studentfiles/$student/$graded/ cse230-goto:~/current/$student/
done
