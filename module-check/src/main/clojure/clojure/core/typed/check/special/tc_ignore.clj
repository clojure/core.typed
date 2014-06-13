(ns clojure.core.typed.check.special.tc-ignore
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-rep :as r]))

(defn check-tc-ignore [check expr expected]
  (assoc expr
         ::t/tc-ignore true
         u/expr-type (r/ret r/-any)))

