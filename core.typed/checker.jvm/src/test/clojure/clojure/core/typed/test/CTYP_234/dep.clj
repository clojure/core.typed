(ns clojure.core.typed.test.CTYP-234.dep
  {:core.typed {:collect-only true}}
  (:require [clojure.core.typed :as t]))

(t/defalias MyType
  (t/HMap :mandatory {:foo String
                      :bar t/AnyInteger}))
