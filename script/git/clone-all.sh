#!/bin/bash

set -e

clone () {
  GROUPID=`dirname $1` 
  ARTIFACTID=`basename $1`
  if [ "$GITHUB_ACTIONS" = true ]; then
    # don't need push access, clone with HTTPS
    git clone https://github.com/typedclojure/$ARTIFACTID.git --origin typedclojure
  else
    git clone git@github.com:typedclojure/$ARTIFACTID.git --origin typedclojure
  fi
  cd $ARTIFACTID
  git branch --set-upstream-to typedclojure/master
  if [ "$GROUPID" == "org.clojure" ]; then
    git remote add clojure git@github.com:clojure/$ARTIFACTID.git
  fi
  cd ..
}

# https://stackoverflow.com/a/26759734
if [ -x "$(command -v parallel)" ]; then
  export -f clone
  cat projects | parallel clone {}
else
  # https://stackoverflow.com/a/1521498
  while read p; do
    clone "$p"
  done <projects
fi
