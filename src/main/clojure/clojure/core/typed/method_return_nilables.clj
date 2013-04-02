(ns clojure.core.typed.method-return-nilables
  (:require [clojure.core.typed
             [utils :as u]
             [type-rep :as r]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Return non-nilables

(defonce METHOD-RETURN-NONNILABLE-ENV (atom {}))
(set-validator! METHOD-RETURN-NONNILABLE-ENV (u/hash-c? (every-pred namespace symbol?)
                                                        (some-fn #(= :all %)
                                                                 (u/set-c? u/nat?))))

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
