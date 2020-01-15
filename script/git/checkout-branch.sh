#!/bin/sh

set -e

BRANCH=$1
PREFIX="branches"
SCRIPTDIR="script"

if [ -z "$BRANCH" ]; then
  echo "Must provide branch name"
  exit 1;
fi

cp -r $SCRIPTDIR $PREFIX

while read line
do
  NAME=`basename ${line}`
  cd ${NAME}
  git worktree add -b ${BRANCH} ../${PREFIX}/${BRANCH}/${NAME}
  cd ..
done < projects
