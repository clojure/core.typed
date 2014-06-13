(ns clojure.core.typed.check.nthnext
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.check.utils :as cu]))

(defn ^:private shift-hsequential [target-t num]
  (let [shift (fn [property] (vec (nthrest (property target-t) num)))]
    (r/-hseq (shift :types)
             :filters (shift :fs)
             :objects (shift :objects)
             :rest (:rest target-t)
             :drest (:drest target-t))))

(defn ^:private nthnext-type [num-t target-t]
  (let [num (:val num-t)
        no-more-fixed-types (>= num (count (:types target-t)))]
    (cond
     (and no-more-fixed-types (:rest target-t))
     (c/Un r/-nil (shift-hsequential target-t num))

     no-more-fixed-types
     r/-nil

     :else
     (shift-hsequential target-t num))))

(defn check-nthnext [check-fn {:keys [args] :as expr} expected & {:keys [cargs]}]
  (let [[ctarget cn :as cargs] (or cargs (mapv check-fn args))]
    (if-not (#{2} (count cargs))
      cu/not-special
      (let [target-t (c/fully-resolve-type (-> ctarget u/expr-type r/ret-t))
            target-types (if (r/Union? target-t)
                           (:types target-t)
                           [target-t])
            num-t (-> cn u/expr-type r/ret-t)]
        (if (and (every? c/AnyHSequential? target-types)
                 (r/Value? num-t)
                 (integer? (:val num-t)))
          (-> expr
              (update-in [:fn] check-fn)
              (assoc
                  :args cargs
                  u/expr-type (r/ret (apply c/Un (map (partial nthnext-type num-t) target-types)))))
          cu/not-special)))))
