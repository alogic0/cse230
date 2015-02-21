#!/bin/bash

GRADING=$PWD
STUDENTS="$GRADING/../studentfiles"
echo $STUDENTS
for STUDENT in $1
do

STUDENTDIR=$STUDENTS/$STUDENT

rm -rf $STUDENTDIR/*-graded || exit 1

HOMEWORKDIR=`ls -d1 $STUDENTDIR/Final* | tail -1`
GRADEDIR="$HOMEWORKDIR-graded"

mkdir -p $GRADEDIR
cp $HOMEWORKDIR/* $GRADEDIR

GRADEFILE="$GRADEDIR/gradereport"

ghc -odir $GRADEDIR -o $GRADEDIR/gradefinal --make final/gradefinal.hs grade.hs $HOMEWORKDIR/final.lhs final/solution.lhs

GRADEFILE="$GRADEDIR/gradereport"
echo "Grading Final for student $STUDENT" > $GRADEFILE
$GRADEDIR/gradefinal >> $GRADEFILE
echo "GRATEREPORT WRITTEN"

#cat $GRADEFILE
done
