(ns clojure.core.typed.test.typed-load.bad-dep
  {:lang :core.typed})

;; this will only throw a type error on the first load
(inc 'a)
