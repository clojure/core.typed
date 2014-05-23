(ns clojure.core.typed.check.nth
  (:require [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.check.utils :as cu])
  (:import (clojure.lang ISeq Seqable)))

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
             u/expr-type (r/ret (apply c/Un
                                   (doall
                                     (for [t types]
                                       (if-let [res-t (cond
                                                        (r/Nil? t) (or default-t r/-nil)
                                                        ; nil on out-of-bounds and no default-t
                                                        :else (nth (:types t) (:val num-t) default-t))]
                                         res-t
                                         (err/int-error (str "Cannot get index " (:val num-t)
                                                           " from type " (prs/unparse-type t)))))))
                            (let [nnth (:val num-t)
                                  target-o (r/ret-o (u/expr-type te))
                                  default-o (when de
                                              (r/ret-o (u/expr-type de)))
                                  ;; We handle filters for both arities of nth here, with and without default
                                  ;;
                                  ;;With default:
                                  ;; if this is a true value either:
                                  ;;  * target is nil or seq and default is true
                                  ;;  * target is seqable, default is false
                                  ;;    and target is at least (inc nnth) count
                                  default-fs+ (fo/-or (fo/-and (fo/-filter-at (c/Un r/-nil (c/RClass-of ISeq [r/-any])) 
                                                                              target-o)
                                                               (fo/-not-filter-at (c/Un r/-false r/-nil) 
                                                                                  default-o))
                                                      (fo/-and (fo/-filter-at (c/In (c/RClass-of Seqable [r/-any])
                                                                                    (r/make-CountRange (inc nnth)))
                                                                              target-o)
                                                               (fo/-filter-at (c/Un r/-false r/-nil) 
                                                                              default-o)))
                                  ;;Without default:
                                  ;; if this is a true value: 
                                  ;;  * target is seqable of at least nnth count
                                  nodefault-fs+ (fo/-filter-at (c/In (c/RClass-of Seqable [r/-any])
                                                                     (r/make-CountRange (inc nnth)))
                                                               target-o)]
                              (fo/-FS (if default-t
                                        default-fs+
                                        nodefault-fs+)
                                      ; not sure if there's anything worth encoding here
                                      fl/-top))))
      :else cu/not-special)))
