#!/bin/sh

branch_name="$(git symbolic-ref HEAD 2>/dev/null)" ||
branch_name="(unnamed branch)"     # detached HEAD

branch_name=${branch_name##refs/heads/}

MASTER="master"

set -e

if [ $branch_name != "$MASTER" ]; then
  echo "Must be on $MASTER"
  exit 1;
fi

#git pull clojure --ff-only master
git pull typedclojure --ff-only master
git push typedclojure master
#git push clojure master
