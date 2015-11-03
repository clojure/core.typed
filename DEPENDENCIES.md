# Introduction

`core.typed` embeds its dependencies to prevent conflicts with other JARs,
or "JAR hell".

clojure.core.typed.deps.clojure.tools.analyzer
clojure.core.typed.deps.clojure.tools.analyzer.jvm
clojure.core.typed.deps.clojure.core.cache
clojure.core.typed.deps.clojure.core.contracts
clojure.core.typed.deps.clojure.core.unify
clojure.core.typed.deps.clojure.math.combinatorics
clojure.core.typed.deps.clojure.core.memoize
clojure.core.typed.deps.org.owl2.asm
clojure.core.typed.deps.clojure.tools.namespace
clojure.core.typed.deps.clojure.tools.reader

# How to update dependencies

To update a dependency, change the corresponding version in `project.clj`,
then run

```
./deps.sh
```

You should then manually inspect the output via `git diff` to make sure everything
makes sense. Finally, run the unit tests and commit the changes.
