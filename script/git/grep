#!/bin/sh

while read line
do
  NAME=`basename ${line}`
  cd $NAME
  echo "grepping: $NAME"
  git grep "$@"
  echo ""
  cd ..
done < projects
