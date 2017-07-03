(ns clojure.core.typed.check.nth
  (:require [clojure.core.typed :as t] 
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.path-rep :as pe]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.check.method :as method]
            [clojure.core.typed.debug :refer [dbg]]
            [clojure.core.typed.check.utils :as cu])
  (:import (clojure.lang ISeq Seqable)))

(defn ^:private expr->type [expr]
  (if expr (-> expr u/expr-type r/ret-t)))

(defn ^:private expr->object [expr]
  (if expr (-> expr u/expr-type r/ret-o)))

(defn ^:private expression? [expr]
  (r/TCResult? (u/expr-type expr)))

(defn nth-type [types idx default-t]
  {:pre [(every? r/Type? types)
         (con/znat? idx)
         ((some-fn nil? r/Type?) default-t)]
   :post [(r/Type? %)]}
  (apply c/Un
         (doall
          (for [t types]
            (if-let [res-t (cond
                            (r/Nil? t) (or default-t r/-nil)
                            ;; nil on out-of-bounds and no default-t
                            :else (nth (:types t) idx default-t))]
              res-t
              (err/int-error (str "Cannot get index " idx
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
         (con/znat? idx)]
   :post [(fl/Filter? %)]}
  (fo/-and (fo/-filter-at (c/In (c/RClass-of Seqable [r/-any])
                                (r/make-CountRange (inc idx)))
                          target-o)
           (fo/-filter-at (c/Un r/-false r/-nil)
                          default-o)))

(defn ^:private nth-positive-filter-default [target-o default-o idx]
  {:pre [(obj/RObject? target-o)
         (obj/RObject? default-o)
         (con/znat? idx)]
   :post [(fl/Filter? %)]}
  (fo/-or (nth-positive-filter-default-truthy target-o default-o)
          (nth-positive-filter-default-falsy target-o default-o idx)))

(defn ^:private nth-positive-filter-no-default [target-o idx]
  {:pre [(obj/RObject? target-o)
         (con/znat? idx)]
   :post [(fl/Filter? %)]}
  (fo/-filter-at (c/In (c/RClass-of Seqable [r/-any])
                       (r/make-CountRange (inc idx)))
                 target-o))

(defn ^:private nth-filter [target-expr default-expr idx default-t]
  {:pre [(expression? target-expr)
         ((some-fn nil? expression?) default-expr)
         (con/znat? idx)
         ((some-fn nil? r/Type?) default-t)]
   :post [(fl/Filter? %)]}
  (let [target-o (expr->object target-expr)
        default-o (expr->object default-expr)

        filter+ (if default-t
                  (nth-positive-filter-default target-o default-o idx)
                  (nth-positive-filter-no-default target-o idx))]
    (fo/-FS filter+
            ;; not sure if there's anything worth encoding here
            fl/-top)))

(defn ^:private nth-object [target-expr idx]
  {:pre [(expression? target-expr)
         (con/znat? idx)]
   :post [(obj/RObject? %)]}
  (let [target-o (expr->object target-expr)]
    (if (obj/Path? target-o)
      (update-in target-o [:path] concat [(pe/NthPE-maker idx)])
      target-o)))

(def nat-value? (every-pred r/Value? (comp con/znat? :val)))

(defn nth-function-type [n]
  {:pre [(con/znat? n)]
   :post [(r/Type? %)]}
  (let [; gensyms are too ugly to read in errors
        x 'x
        y 'y]
    (impl/with-clojure-impl
      (prs/parse-type
        `(t/All [~x ~y]
          (t/IFn 
            [(t/U (clojure.lang.Indexed ~x) (t/SequentialSeqable ~x)) t/Int :-> ~x]
            [(t/I (t/U (clojure.lang.Indexed ~x) (t/SequentialSeqable ~x))
                  (t/CountRange ~(inc n)))
             (t/Val ~n) t/Any :-> ~x]
            [(t/U (clojure.lang.Indexed ~x) (t/SequentialSeqable ~x) nil) t/Int ~y :-> (t/U ~x ~y)]
            [(t/U (clojure.lang.Indexed ~x) (t/SequentialSeqable ~x) nil) t/Int :-> (t/U ~x nil)]))))))

(defn invoke-nth [check-fn {:keys [args] :as expr} expected & {:keys [cargs]}]
  {:pre [((some-fn nil? vector?) cargs)
         (#{:static-call} (:op expr))]
   :post [(or (#{cu/not-special} %)
              (-> % u/expr-type r/TCResult?))]}
  (let [_ (assert (#{2 3} (count args)) (str "nth takes 2 or 3 arguments, actual " (count args)))
        [te ne de :as cargs] (or cargs (mapv check-fn args))
        types (let [ts (c/fully-resolve-type (expr->type te))]
                (if (r/Union? ts)
                  (map c/fully-resolve-type (:types ts))
                  [ts]))
        num-t (expr->type ne)
        default-t (expr->type de)]
    (cond
      (and (nat-value? num-t)
           (every? (some-fn r/Nil?
                            r/HeterogeneousVector?
                            r/HeterogeneousList?
                            r/HeterogeneousSeq?)
                   types))
      (let [idx (:val num-t)]
        (assoc expr
          :args cargs
          u/expr-type (r/ret (nth-type types idx default-t)
                             (nth-filter te de idx default-t)
                             (nth-object te idx))))

      ; rewrite nth type to be more useful when we have an exact (and interesting) index.
      (nat-value? num-t)
      (method/check-invoke-method
        check-fn expr expected
        :method-override (nth-function-type (-> num-t :val))
        :cargs cargs)
      :else cu/not-special)))
