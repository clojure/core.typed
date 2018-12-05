#!/bin/sh

set -e

clj -Ascript -m update-dep $1 $2

if [[ $(git status -s deps.edn) ]]
then
  clj -Spom
fi
