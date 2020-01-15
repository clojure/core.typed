#!/bin/sh

set -e

START=$1
if [ -z $1 ]; then
  echo "Must provide folder"
  exit 1
fi

cd $START
COORD=`./script/current-coord.sh`
VERSION=`./script/current-version.sh`
cd ..

while read line
do
  NAME=`basename ${line}`
  cd $NAME
  echo "grepping: $NAME"
  ./script/update-dep.sh $COORD $VERSION
  echo ""
  cd ..
done < projects
