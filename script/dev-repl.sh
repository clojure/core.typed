#!/bin/bash

set -e

cd dev
clj -A:merge-deps
cd ../typed
clj -A:nREPL
