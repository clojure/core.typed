(ns clojure.core.typed.test.CTYP-234.core
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.test.CTYP-234.dep :as other]))

(t/ann foo [other/MyType -> t/AnyInteger])
(defn foo
  [x]
  (:bar x))
