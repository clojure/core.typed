(ns clojure.core.typed.method-return-nilables
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.type-rep :as r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Return non-nilables

(defonce METHOD-RETURN-NONNILABLE-ENV 
  (atom {}
        :validator (con/hash-c? (every-pred namespace symbol?)
                                (some-fn #(= :all %)
                                         (con/set-c? con/znat?)))))

(defn add-nonnilable-method-return [sym m]
  (swap! METHOD-RETURN-NONNILABLE-ENV assoc sym m)
  nil)

(defn reset-nonnilable-method-return-env! [m]
  (reset! METHOD-RETURN-NONNILABLE-ENV m)
  nil)

(defn nonnilable-return? [sym arity]
  (let [as (@METHOD-RETURN-NONNILABLE-ENV sym)]
    (boolean (or (= :all as)
                 (when as
                   (as arity))))))
