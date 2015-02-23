(ns ^:skip-wiki clojure.core.typed.check.isa
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed :as t]
            [clojure.core.typed.check-below :as below]
            [clojure.core.typed.filter-protocols :as fprotocol]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.path-rep :as path]
            [clojure.core.typed.tc-equiv :as equiv])
  (:import (clojure.core.typed.type_rep TCResult)))

(alter-meta! *ns* assoc :skip-wiki true)

;(t/ann tc-isa? [TCResult TCResult -> TCResult])
(defn tc-isa? 
  "Type check a call to isa?. Assumes global hierarchy.
  Also supports the case where both elements are vectors, but not recursively.
  Ensures result is below expected."
  [child-ret parent-ret expected]
  {:pre [(r/TCResult? child-ret)
         (r/TCResult? parent-ret)
         ((some-fn r/TCResult? nil?) parent-ret)]
   :post [(r/TCResult? %)]}
  (t/letfn> [fs :- [TCResult TCResult -> '{:then fprotocol/IFilter :else fprotocol/IFilter}]
             (fs [child1 parent1]
                 {:pre [(r/TCResult? child1)
                        (r/TCResult? parent1)]
                  :post [((con/hmap-c? :then fl/Filter? :else fl/Filter?) %)]}
                 (let [obj (r/ret-o child1)
                       ty (c/fully-resolve-type (r/ret-t parent1))]
                   (cond
                     ;; - if child1's object is terminated with a ClassPE and we have a class singleton
                     ;;   on the right, then we strip off the last path element and claim that object is
                     ;;   and instance of that class.
                     ;;TODO `last` has complexity linear in length of path, use better data structure
                     (and (obj/Path? obj)
                          (path/ClassPE? (obj/last-path-elem obj))
                          (r/Value? ty)
                          (class? (:val ty)))
                     (let [obj (obj/without-final-elem obj)
                           ty (c/RClass-of (:val ty))]
                       {:then (fo/-filter-at ty obj)
                        :else (fo/-not-filter-at ty obj)})

                     ;; - if we have a singleton type that is not a Class, then we're in equality mode
                     ;;   so the filters just claim the child1 is of type parent1.
                     (and (r/Value? ty)
                          (not (class? (:val ty)))
                          (equiv/equivable? ty))
                     {:then (fo/-filter-at ty obj)
                      :else (fo/-not-filter-at ty obj)}

                     ;; - otherwise, give up
                     :else
                     {:then fl/-top
                      :else fl/-top})))]
    (let [child-t (r/ret-t child-ret)
          parent-t (r/ret-t parent-ret)
          fs (cond
               ; interesting case with (isa? [...] [...])
               ; use each pairing between child and parent
               (and (r/HeterogeneousVector? child-t)
                    (r/HeterogeneousVector? parent-t)
                    (== (count (:types child-t))
                        (count (:types parent-t))))
               (let [individual-fs (map fs (cu/hvec->rets child-t) (cu/hvec->rets parent-t))]
                 (fo/-FS (apply fo/-and (map :then individual-fs))
                         (apply fo/-or (map :else individual-fs))))
               ; simple (isa? child parent) 
               :else (let [{:keys [then else]} (fs child-ret parent-ret)]
                       (fo/-FS then else)))]
      (below/maybe-check-below
        (r/ret (c/Un r/-true r/-false) fs obj/-empty)
        expected))))

