#!/bin/sh

set -e

clojure -Sdeps '{:deps {org.clojure/tools.deps.alpha {:mvn/version "0.5.460"}}}' -Ascript:test -m update-dep $1 $2

if [[ $(git status -s deps.edn) ]]
then
  clojure -Atest -Spom
fi
