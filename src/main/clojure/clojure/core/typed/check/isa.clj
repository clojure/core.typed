(ns clojure.core.typed.check.isa
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed :as t]
            [clojure.core.typed.filter-protocols :as fprotocol]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.object-rep :as obj])
  (:import (clojure.core.typed.type_rep TCResult)))

;(t/ann tc-isa? [TCResult TCResult -> TCResult])
(defn tc-isa? 
  "Type check a call to isa?. Assumes global hierarchy.
  Also supports the case where both elements are vectors, but not recursively."
  [child-ret parent-ret]
  {:pre [(r/TCResult? child-ret)
         (r/TCResult? parent-ret)]
   :post [(r/TCResult? %)]}
  (t/letfn> [fs :- [TCResult TCResult -> '{:then fprotocol/IFilter :else fprotocol/IFilter}]
             (fs [child1 parent1]
                 {:pre [(r/TCResult? child1)
                        (r/TCResult? parent1)]
                  :post [((con/hmap-c? :then fl/Filter? :else fl/Filter?) %)]}
                 {:then (fo/-filter-at (r/ret-t parent1) (r/ret-o child1))
                  :else (fo/-not-filter-at (r/ret-t parent1) (r/ret-o child1))})]
    (let [child-t (r/ret-t child-ret)
          parent-t (r/ret-t parent-ret)
          fs (cond
               ; interesting case with (isa? [...] [...])
               ; use each pairing between child and parent
               (and (r/HeterogeneousVector? child-t)
                    (r/HeterogeneousVector? parent-t))
               (let [individual-fs (map fs (cu/hvec->rets child-t) (cu/hvec->rets parent-t))]
                 (fo/-FS (apply fo/-and (map :then individual-fs))
                         (apply fo/-or (map :else individual-fs))))
               ; simple (isa? child parent) 
               :else (let [{:keys [then else]} (fs child-ret parent-ret)]
                       (fo/-FS then else)))]
      (r/ret (c/Un r/-true r/-false) fs obj/-empty))))

