(ns clojure.core.typed.check.local
  (:require [clojure.core.typed.local-result :as local-result]
            [clojure.core.typed.utils :as u]))

(defn check-local [{sym :name :as expr} expected]
  (assoc expr
         u/expr-type (local-result/local-result expr sym expected)))
