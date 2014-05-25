(ns clojure.core.typed.check.nth
  (:require [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.check.utils :as cu])
  (:import (clojure.lang ISeq Seqable)))

(defn ^:private nth-type [types idx-t default-t]
  {:pre [(every? r/Type? types)
         (r/Type? idx-t)
         ((some-fn nil? r/Type?) default-t)]
   :post [(r/Type? %)]}
  (apply c/Un
         (doall
          (for [t types]
            (if-let [res-t (cond
                            (r/Nil? t) (or default-t r/-nil)
                            ;; nil on out-of-bounds and no default-t
                            :else (nth (:types t) (:val idx-t) default-t))]
              res-t
              (err/int-error (str "Cannot get index " (:val idx-t)
                                  " from type " (prs/unparse-type t))))))))

(defn ^:private nth-positive-filter-default-truthy [target-o default-o]
  {:pre [(obj/RObject? target-o)
         (obj/RObject? default-o)]
   :post [(fl/Filter? %)]}
  (fo/-and (fo/-filter-at (c/Un r/-nil (c/RClass-of ISeq [r/-any]))
                          target-o)
           (fo/-not-filter-at (c/Un r/-false r/-nil)
                              default-o)))

(defn ^:private nth-positive-filter-default-falsy [target-o default-o idx]
  {:pre [(obj/RObject? target-o)
         (obj/RObject? default-o)
         (con/nat? idx)]
   :post [(fl/Filter? %)]}
  (fo/-and (fo/-filter-at (c/In (c/RClass-of Seqable [r/-any])
                                (r/make-CountRange (inc idx)))
                          target-o)
           (fo/-filter-at (c/Un r/-false r/-nil)
                          default-o)))

(defn ^:private nth-positive-filter-default [target-o default-o idx]
  {:pre [(obj/RObject? target-o)
         (obj/RObject? default-o)
         (con/nat? idx)]
   :post [(fl/Filter? %)]}
  (fo/-or (nth-positive-filter-default-truthy target-o default-o)
          (nth-positive-filter-default-falsy target-o default-o idx)))

(defn ^:private nth-positive-filter-no-default [target-o idx]
  {:pre [(obj/RObject? target-o)
         (con/nat? idx)]
   :post [(fl/Filter? %)]}
  (fo/-filter-at (c/In (c/RClass-of Seqable [r/-any])
                       (r/make-CountRange (inc idx)))
                 target-o))

(defn ^:private nth-filter [target-expr default-expr idx-t default-t]
  {:pre [(r/TCResult? (u/expr-type target-expr))
         ((some-fn nil? r/TCResult?) (u/expr-type default-expr))
         (r/Type? idx-t)
         ((some-fn nil? r/Type?) default-t)]
   :post [(fl/Filter? %)]}
  (let [idx (:val idx-t)
        target-o (r/ret-o (u/expr-type target-expr))
        default-o (when default-expr
                    (r/ret-o (u/expr-type default-expr)))

        filter+ (if default-t
                  (nth-positive-filter-default target-o default-o idx)
                  (nth-positive-filter-no-default target-o idx))]
    (fo/-FS filter+
            ;; not sure if there's anything worth encoding here
            fl/-top)))

(defn invoke-nth [check-fn {:keys [args] :as expr} expected & {:keys [cargs]}]
  {:pre [((some-fn nil? vector?) cargs)]}
  (let [_ (assert (#{2 3} (count args)) (str "nth takes 2 or 3 arguments, actual " (count args)))
        [te ne de :as cargs] (or cargs (mapv check-fn args))
        types (let [ts (c/fully-resolve-type (r/ret-t (u/expr-type te)))]
                (if (r/Union? ts)
                  (:types ts)
                  [ts]))
        num-t (r/ret-t (u/expr-type ne))
        default-t (when de
                    (r/ret-t (u/expr-type de)))]
    (cond
      (and (r/Value? num-t)
           (integer? (:val num-t))
           (every? (some-fn r/Nil?
                            r/HeterogeneousVector?
                            r/HeterogeneousList?
                            r/HeterogeneousSeq?)
                   types))
      (assoc expr
             :args cargs
             u/expr-type (r/ret (nth-type types num-t default-t)
                                (nth-filter te de num-t default-t)))
      :else cu/not-special)))
