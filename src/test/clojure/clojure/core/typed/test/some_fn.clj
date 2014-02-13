(ns clojure.core.typed.test.some-fn
  (:require [clojure.core.typed :as t]))

(let [s (t/ann-form (some-fn number? symbol?) (predicate (U Number t/Symbol)))]
  (t/print-env "f"))

(let [s (t/ann-form (every-pred symbol? number?) (predicate (I Number t/AnyInteger)))]
  (t/print-env "f"))

(comp :b :a)
