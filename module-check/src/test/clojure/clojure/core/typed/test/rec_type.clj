(ns clojure.core.typed.test.rec-type
  (:require [clojure.core.typed :as t])
  (:import (clojure.lang IMapEntry)))

;(t/defalias RuleSet (Rec [x] (t/Map Any (U [Any -> Any] x))))
;
;(t/ann-form {:a (t/ann-form (fn [a] a)
;                            [Any -> Any])}
;            RuleSet)

(t/defalias Rule [t/Any -> (t/Option t/Keyword)])

(t/defalias RuleSet
  (t/Rec [x]
       (t/Map t/Any (t/U Rule x))))

(t/defalias Report
  (t/Rec [x]
       (t/Map t/Any (t/U t/Keyword x))))

(t/defalias Data
  (t/Map t/Any t/Any))

(t/ann clean [RuleSet Data -> Data])
(defn clean [rules data]
  (reduce (t/ann-form (fn [cleaned-up kv]
                      (let [rule-path (key kv)
                            datum (val kv)]
                        (if-let [rule (get rules rule-path)]
                          (assoc cleaned-up rule-path datum)
                          cleaned-up)))
                    [Data (IMapEntry t/Any t/Any) -> Data])
          {} data))

;(t/ann enforce [RuleSet Data -> (t/Option Report)])
;(defn enforce [ruleset data]
;  (let [result (reduce (ann-form (fn [report kv]
;                                   (let [rule-path (key kv)
;                                         rule (val kv)
;                                         datum (get data rule-path)]
;                                     (if-let [message (rule datum)]
;                                       (assoc report rule-path message)
;                                       report)))
;                                 [Report (IMapEntry Any Rule) -> Report])
;                       (reduce (ann-form (fn [total k]
;                                           (if (not (contains? ruleset k))
;                                             (assoc total k ::not-in-schema)
;                                             total))
;                                         [Report Any -> Report])
;                               {} (keys data))
;                       (seq ruleset))]
;    (if (not (empty? result))
;      result)))


#_(t/ann enforce [RuleSet Data -> (t/Option Report)])
#_(defn enforce [ruleset data]
  (let [result (reduce (ann-form (fn [report kv]
                                   (let [rule-path (key kv)
                                         sub (val kv)
                                         datum (get data rule-path)]
                                     (if (map? sub)
                                       (if (map? datum)
                                         (if-let [sub-errors (enforce sub datum)]
                                           (assoc report rule-path sub-errors)
                                           report)
                                         (assoc report rule-path ::map-expected))
                                       (if-let [message (sub datum)]
                                         (assoc report rule-path message)
                                         report))))
                                 [Report (IMapEntry t/Any (t/U Rule RuleSet)) -> Report])
                       (reduce (ann-form (fn [total k]
                                           (if (not (contains? ruleset k))
                                             (assoc total k ::not-in-schema)
                                             total))
                                         [Report t/Any -> Report])
                               {} (keys data))
                       (seq ruleset))]
    (if (not (empty? result))
      result)))


