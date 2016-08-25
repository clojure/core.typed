(ns clojure.core.typed.check.nthnext
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.check.utils :as cu]))

(defn ^:private shift-hsequential [target-t num]
  (let [shift (fn [property] (vec (nthrest (property target-t) num)))]
    (r/-hseq (shift :types)
             :filters (shift :fs)
             :objects (shift :objects)
             :rest (:rest target-t)
             :drest (:drest target-t))))

(defn ^:private nthnext-type [num target-t]
  {:pre [(integer? num)]}
  (let [no-more-fixed-types (>= num (count (:types target-t)))]
    (cond
     (and no-more-fixed-types (:rest target-t))
     (c/Un r/-nil (shift-hsequential target-t num))

     no-more-fixed-types
     r/-nil

     :else
     (shift-hsequential target-t num))))

(defn check-specific-next [check-fn {:keys [args] :as expr} expected
                           & {:keys [cargs nnexts nargs
                                     target-t]}]
  {:pre [(integer? nnexts)
         (vector? cargs)
         (integer? nargs)
         ((some-fn r/Type? nil?) target-t)]}
  (if-not (#{nargs} (count cargs))
    cu/not-special
    (let [_ (assert (r/Type? target-t))
          target-t (c/fully-resolve-type target-t)
          target-types (if (r/Union? target-t)
                         (:types target-t)
                         [target-t])]
      ;(prn "HSequentials" (group-by c/AnyHSequential? target-types))
      (if (every? c/AnyHSequential? target-types)
        (let [ts (map (partial nthnext-type nnexts) target-types)]
          (-> expr
              (update-in [:fn] check-fn)
              (assoc
                :args cargs
                u/expr-type (r/ret (apply c/Un ts)
                                   (cond
                                     (every? r/Nil? ts) (fo/-false-filter)
                                     (every? c/AnyHSequential? ts) (fo/-true-filter)
                                     :else (fo/-simple-filter))))))
        cu/not-special))))

(defn check-nthnext [check-fn {:keys [args] :as expr} expected & {:keys [cargs]}]
  (assert (vector? cargs))
  (if-not (#{2} (count cargs))
    cu/not-special
    (let [[ctarget cn :as cargs] cargs
          target-t (c/fully-resolve-type (-> ctarget u/expr-type r/ret-t))
          num-t (-> cn u/expr-type r/ret-t)]
      (if (and (r/Value? num-t)
               (integer? (:val num-t)))
        (check-specific-next
          check-fn expr expected
          :nnexts (:val num-t) 
          :nargs 2
          :target-t (r/ret-t (u/expr-type ctarget))
          :cargs cargs)
        cu/not-special))))

(defn check-next [check-fn {:keys [args] :as expr} expected & {:keys [cargs]}]
  (assert (vector? cargs))
  (check-specific-next check-fn expr expected 
                       :nnexts 1 
                       :nargs 1 
                       :target-t (when (seq cargs)
                                   (r/ret-t (u/expr-type (first cargs))))
                       :cargs cargs))

(defn check-seq [check-fn {:keys [args] :as expr} expected & {:keys [cargs]}]
  (assert (vector? cargs))
  (check-specific-next check-fn expr expected 
                       :nnexts 0
                       :nargs 1
                       :target-t (when (seq cargs)
                                   (r/ret-t (u/expr-type (first cargs))))
                       :cargs cargs))
