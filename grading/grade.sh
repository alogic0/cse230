#!/bin/bash

hw=$1

for STUDENT in `ls studentfiles`; do
  ./gradestudent.sh $STUDENT $hw
done
