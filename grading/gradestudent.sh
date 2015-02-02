#!/bin/bash

STUDENT=$1
ASSN=$2

STUDENTDIR=studentfiles/$STUDENT

rm -rf $STUDENTDIR/*-graded || exit 1

ASSNDIR=`find $STUDENTDIR -maxdepth 1 -iname "$ASSN*" | tail -1`
GRADEDIR="$ASSNDIR-graded"

mkdir -p $GRADEDIR

cp $ASSNDIR/* $GRADEDIR

# copy suplementary files...
#
# cp hw3/twoPowerEight.imp $GRADEDIR
# cp hw3/gcd.imp $GRADEDIR
#

GRADEFILE="$GRADEDIR/gradereport"

ASSNFILE=$ASSNDIR/$ASSN.lhs
if [ ! -e $ASSNFILE ]; then
   ASSNFILE=$ASSNDIR/$ASSN.hs
fi

GRADEFILE="$GRADEDIR/gradereport"

(echo "Grading $ASSN for $STUDENT" &&
ghc -iSOE/src:$ASSN -odir $GRADEDIR -o $GRADEDIR/grade$ASSN --make $ASSN/grade$ASSN.hs grade.hs $ASSNFILE $ASSN/Solution.hs 2>&1 &&
$GRADEDIR/grade$ASSN
) | tee $GRADEFILE

echo "GRADE REPORT WRITTEN"
