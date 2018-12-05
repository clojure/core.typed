#!/bin/sh

set -e

clojure -Ascript -m update-dep $1 $2

if [[ $(git status -s deps.edn) ]]
then
  clojure -Spom
fi
