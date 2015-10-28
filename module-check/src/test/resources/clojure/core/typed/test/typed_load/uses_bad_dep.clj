(ns clojure.core.typed.test.typed-load.uses-bad-dep
  {:lang :core.typed}
  ;; this only type checks once, then caches like `require`
  (:require [clojure.core.typed.test.typed-load.bad-dep]))
