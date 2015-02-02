#!/bin/bash

GRADING=$PWD
STUDENTS="$GRADING/../studentfiles"
echo $STUDENTS
for STUDENT in $1
do

STUDENTDIR=$STUDENTS/$STUDENT
HW=4

rm -rf $STUDENTDIR/*-graded || exit 1

HOMEWORKDIR=`ls -d1 $STUDENTDIR/HW$HOMEWORK* | tail -1`
GRADEDIR="$HOMEWORKDIR-graded"

mkdir -p $GRADEDIR
cp $HOMEWORKDIR/* $GRADEDIR

GRADEFILE="$GRADEDIR/gradereport"

ghc -odir $GRADEDIR -o $GRADEDIR/gradehw$HW --make hw$HW/gradehw$HW.hs grade.hs $HOMEWORKDIR/hw$HW.lhs hw$HW/solution.lhs

GRADEFILE="$GRADEDIR/gradereport"
echo "Grading homework $HOMEWORK for student $STUDENT" > $GRADEFILE
$GRADEDIR/gradehw$HW >> $GRADEFILE
echo "GRATEREPORT WRITTEN"

#cat $GRADEFILE
done
