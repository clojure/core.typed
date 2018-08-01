(ns clojure.core.typed.check.special.loop
  (:require [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.check.recur-utils :as recur-u]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.analyzer2 :as ana2]
            [clojure.core.typed.utils :as u]))

; corresponds to a c.c.t/loop macro.
; Extra the :ann annotations for loop variables and propagate to actual loop construct checking
; via `recur-u/*loop-bnd-anns*`.
(defn check-special-loop
  [check expr expected]
  {:pre [(#{3} (count (:statements expr)))]}
  (let [{[_ _ vexpr :as statements] :statements frm :ret, :keys [env], :as expr}
        (-> expr
            (update-in [:statements 2] ana2/run-passes))
        ; tools.analyzer does constanst folding
        tsyns (case (:op vexpr)
                :const (let [{{tsyns-quoted :ann} :val} vexpr
                             _ (assert (and (seq? tsyns-quoted)
                                            (#{'quote} (first tsyns-quoted)))
                                       (pr-str tsyns-quoted))
                             tsyns (second tsyns-quoted)]
                         tsyns)
                :map (let [{ks :keys vs :vals} vexpr
                           tsyns (reduce (fn [_ [k v]]
                                           (when (= :const (:op k))
                                             (when (= :ann (:val k))
                                               (assert (= :quote (:op v)))
                                               (reduced (get-in v [:expr :val])))))
                                         nil
                                         (map vector ks vs))]
                       tsyns))
        _ (assert (map? tsyns))
        tbindings (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
                    (mapv (comp prs/parse-type :type) (:params tsyns)))
        cfrm ;loop may be nested, type the first loop found
        (binding [recur-u/*loop-bnd-anns* tbindings]
          (check frm expected))]
    (assoc expr
           :ret cfrm
           u/expr-type (u/expr-type cfrm))))
