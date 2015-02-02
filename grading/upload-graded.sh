#!/bin/sh

HW=$1

for dir in `find "studentfiles/" -name "HW$HW-*-graded"`; do
     echo "$dir"
     rm -f $dir/gradehw$HW
     rm -f $dir/*.o
     rm -f $dir/*.hi
     rm -f $dir/*~
    
     assn=`basename $dir`
     studentdir=`dirname $dir`
     student=`basename $studentdir`

     scp -r studentfiles/$student/$assn/ cse230-goto:~/current/$student/
done
