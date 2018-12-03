#!/bin/sh

git grep -l clojure\.core\.typed\.$1 | xargs sed -i '' "s/clojure\.core\.typed\.$1/clojure.core.typed.$2/g"
