#!/bin/sh

touch module-checker.jvm/src/test/clojure/clojure/core/typed/test/ctyp_$1.clj

echo "(ns clojure.core.typed.test.ctyp-${1}\n  (:require [clojure.test :refer :all]\n            [clojure.core.typed.test.test-utils :refer :all]))" >> module-check/src/test/clojure/clojure/core/typed/test/ctyp_$1.clj
