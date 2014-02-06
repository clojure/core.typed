(ns clojure.core.typed.test.rec-type
  (:require [clojure.core.typed :as t 
             :refer [def-alias ann-form ann]])
  (:import (clojure.lang IMapEntry)))

;(t/def-alias RuleSet (Rec [x] (t/Map Any (U [Any -> Any] x))))
;
;(t/ann-form {:a (t/ann-form (fn [a] a)
;                            [Any -> Any])}
;            RuleSet)

(def-alias Rule [Any -> (t/Option t/Keyword)])

(def-alias RuleSet
  (Rec [x]
       (t/Map Any Rule)))

(def-alias Data (t/Map Any Any))
(def-alias Report (t/Map Any Any))

(ann clean [RuleSet Data -> Data])
(defn clean [rules data]
  (reduce (ann-form (fn [cleaned-up kv]
                      (let [rule-path (key kv)
                            datum (val kv)]
                        (if-let [rule (get rules rule-path)]
                          (assoc cleaned-up rule-path datum)
                          cleaned-up)))
                    [Data (IMapEntry Any Any) -> Data])
          {} data))

(ann enforce [RuleSet Data -> (t/Option Report)])
(defn enforce [ruleset data]
  (let [result (reduce (ann-form (fn [report kv]
                                   (let [rule-path (key kv)
                                         rule (val kv)
                                         datum (get data rule-path)]
                                     (if-let [message (rule datum)]
                                       (assoc report rule-path message)
                                       report)))
                                 [Report (IMapEntry Any Rule) -> Report])
                       (reduce (ann-form (fn [total k]
                                           (if (not (contains? ruleset k))
                                             (assoc total k ::not-in-schema)
                                             total))
                                         [Report Any -> Report])
                               {} (keys data))
                       (seq ruleset))]
    (if (not (empty? result))
      result)))

