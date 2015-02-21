#!/bin/sh

CSE230=/home/niki/cse230-sshfs

for dir in `find "../studentfiles/" -name "Final-*-graded"`; do
     echo "$dir"
     rm $dir/gradefinal
     rm $dir/*.o
     rm $dir/*.hi
     rm $dir/*~
    
     assn=`basename $dir`
     studentdir=`dirname $dir`
     student=`basename $studentdir`
    
     echo "to copy" 
     echo "../strudentfiles/$student/$assn/"
     echo "$student/$assn/"

     cp -r ../studentfiles/$student/$assn/ $CSE230/$student/
done
