(ns clojure.core.typed.check.loop
  (:require [clojure.core.typed.check.let :as let]
            [clojure.core.typed.check.recur-utils :as recur-u]))

(defn check-loop [check expr expected]
  (let [loop-bnd-anns recur-u/*loop-bnd-anns*]
    (binding [recur-u/*loop-bnd-anns* nil]
      (let/check-let check expr expected 
               {:expected-bnds loop-bnd-anns
                :loop? true}))))
