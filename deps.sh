#!/bin/sh

# only invoke in project directory

rm -r target
rm -r module-check/src/main/java/clojure/core/typed/deps
rm -r module-check/src/main/clojure/clojure/core/typed/deps

DEBUG=1 lein source-deps :skip-javaclass-repackage true

cp -r target/srcdeps/clojure/ module-check/src/main/clojure/

# jvm.tools.analyzer 0.3.0 uses Type.java
mkdir -p module-check/src/main/java/clojure/core/typed/deps/org/objectweb/asm/
cp target/srcdeps/org/objectweb/asm/Type.java module-check/src/main/java/clojure/core/typed/deps/org/objectweb/asm/

sed -i -e 's/\zeorg\.objectweb\.asm/clojure.core.typed.deps/g' module-check/src/main/java/clojure/core/typed/deps/org/objectweb/asm/Type.java
sed -i -e 's/\zeorg\.objectweb\.asm/clojure.core.typed.deps/g' module-check/src/main/clojure/clojure/core/typed/deps/clojure/tools/analyzer/jvm/utils.clj
