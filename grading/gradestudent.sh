#!/bin/bash

STUDENT=$1
HW=$2

STUDENTDIR=studentfiles/$STUDENT

rm -rf $STUDENTDIR/*-graded || exit 1

HOMEWORKDIR=`ls -d1 $STUDENTDIR/HW$HW* | tail -1`
GRADEDIR="$HOMEWORKDIR-graded"

mkdir -p $GRADEDIR

cp $HOMEWORKDIR/* $GRADEDIR

# copy suplementary files...
#
# cp hw3/twoPowerEight.imp $GRADEDIR
# cp hw3/gcd.imp $GRADEDIR
#

GRADEFILE="$GRADEDIR/gradereport"

HWFILE=$HOMEWORKDIR/hw$HW.lhs
if [ ! -e $HWFILE ]; then
   HWFILE=$HOMEWORKDIR/hw$HW.hs
fi

ghc -iSOE/src:hw$HW -odir $GRADEDIR -o $GRADEDIR/gradehw$HW --make hw$HW/gradehw$HW.hs grade.hs $HWFILE hw$HW/Solution.hs

GRADEFILE="$GRADEDIR/gradereport"
echo "Grading homework $HW for student $STUDENT" | tee $GRADEFILE
$GRADEDIR/gradehw$HW | tee -a $GRADEFILE
echo "GRADE REPORT WRITTEN"
