(ns clojure.core.typed.check.special.loop
  (:require [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.check.recur-utils :as recur-u]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.utils :as u]))

; corresponds to a c.c.t/loop macro.
; Extra the :ann annotations for loop variables and propagate to actual loop construct checking
; via `recur-u/*loop-bnd-anns*`.
(defn check-special-loop
  [check {[_ _ {{tsyns-quoted :ann} :val} :as statements] :statements frm :ret, :keys [env], :as expr} expected]
  {:pre [(#{3} (count statements))]}
  (let [ ; tools.analyzer preserves quotes
        _ (impl/impl-case
            :clojure (assert (and (seq? tsyns-quoted)
                                  (#{'quote} (first tsyns-quoted)))
                             (pr-str tsyns-quoted))
            :cljs nil)
        tsyns (impl/impl-case
                :clojure (second tsyns-quoted)
                :cljs (assert nil "TODO"))
        _ (assert (map? tsyns))
        tbindings (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
                    (mapv (comp prs/parse-type :type) (:params tsyns)))
        cfrm ;loop may be nested, type the first loop found
        (binding [recur-u/*loop-bnd-anns* tbindings]
          (check frm expected))]
    (assoc expr
           :ret cfrm
           u/expr-type (u/expr-type cfrm))))
