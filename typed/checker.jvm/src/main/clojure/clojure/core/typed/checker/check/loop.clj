;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.loop
  (:require [clojure.core.typed.checker.check.let :as let]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.check.special.ann-form :as ann-form]
            [clojure.core.typed.checker.check.recur-utils :as recur-u]))

(defn inline-annotations [expr]
  {:pre [(= :loop (:op expr))]
   :post [(or (nil? %)
              (and (seq %)
                   (every? r/Type? %)))]}
  (let [;; cljs.analyzer :binding's don't have forms yet
        names (map (some-fn :form :name) (:bindings expr))
        _ (assert (every? symbol? names))
        maybe-anns (map (comp (fn [m]
                                ;(prn "meta" m)
                                (when-let [[_ tsyn] (find m :clojure.core.typed/ann)]
                                  (ann-form/parse-annotation tsyn expr)))
                              meta)
                        names)
        normalize (when (some identity maybe-anns)
                    ;; annotate unannotated vars with Any
                    (seq (map (fn [t] (or t r/-any)) maybe-anns)))
                    ]
    normalize))

;; `recur-u/*loop-bnd-anns*` is populated in `clojure.core.typed.checker.check.special.loop`
(defn check-loop [check expr expected]
  (let [loop-bnd-anns recur-u/*loop-bnd-anns*
        inlines (inline-annotations expr)
        _ (when (and loop-bnd-anns inlines)
            (err/int-error "Cannot provide both an annotation with t/loop and inline loop"))
        ;_ (prn "inlines" inlines)
        anns (or loop-bnd-anns inlines)]
    (binding [recur-u/*loop-bnd-anns* nil]
      (let/check-let check expr expected 
               {:expected-bnds anns
                :loop? true}))))
