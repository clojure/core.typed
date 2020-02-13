#!/bin/sh

set -e

MVN_VERSION=$(mvn -o -q \
    -Dexec.executable=echo \
    -Dexec.args='${project.groupId}/${project.artifactId}' \
    --non-recursive \
    exec:exec)

echo $MVN_VERSION
