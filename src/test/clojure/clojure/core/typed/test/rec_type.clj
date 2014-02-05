(ns clojure.core.typed.test.rec-type
  (:require [clojure.core.typed :as t]))

(t/def-alias RuleSet (Rec [x] (t/Map Any (U [Any -> Any] x))))

(t/ann-form {:a (t/ann-form (fn [a] a)
                            [Any -> Any])}
            RuleSet)
