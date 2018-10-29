#!/bin/sh

BRANCH=$1
PREFIX="../core.typed-branches"

if [ -z "$BRANCH" ]; then
  echo "Must provide branch name"
  exit 1;
fi

git worktree add -b ${BRANCH} ${PREFIX}/${BRANCH}
