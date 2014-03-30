(ns clojure.core.typed.chk.jvm.method-param-nilables
  (:require [clojure.core.typed.chk.common.utils :as u]
            [clojure.core.typed.chk.common.type-rep :as r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Param nilables

(defonce METHOD-PARAM-NILABLE-ENV (atom {}))
(set-validator! METHOD-PARAM-NILABLE-ENV (u/hash-c? (every-pred namespace symbol?)
                                                    (u/hash-c? (some-fn #{:all} u/nat?)
                                                               (some-fn #{:all} (u/set-c? u/nat?)))))

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

