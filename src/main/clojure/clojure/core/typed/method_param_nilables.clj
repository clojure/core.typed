(ns clojure.core.typed.method-param-nilables
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.type-rep :as r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Param nilables

(defonce METHOD-PARAM-NILABLE-ENV 
  (atom {}
        :validator (con/hash-c? (every-pred namespace symbol?)
                                (con/hash-c? (some-fn #{:all} con/znat?)
                                             (some-fn #{:all} (con/set-c? con/znat?))))))

(defn reset-method-nilable-param-env! [m]
  (reset! METHOD-PARAM-NILABLE-ENV m)
  nil)

(defn add-method-nilable-param [sym a]
  (swap! METHOD-PARAM-NILABLE-ENV assoc sym a)
  nil)

(defn nilable-param? [sym arity param]
  (boolean 
    (when-let [nilables (@METHOD-PARAM-NILABLE-ENV sym)]
      (when-let [params (or (nilables :all)
                            (nilables arity))]
        (or (#{:all} params)
            (params param))))))

