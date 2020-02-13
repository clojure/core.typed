;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.get
  (:require [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.check.invoke-kw :as invoke-kw]
            [clojure.core.typed.checker.check.utils :as cu]))

(defn invoke-get
  "Given an unanalyzed :host-call or :invoke invocation
  of clojure.lang.RT/get or clojure.core/get (respectively)
  with fully type checked :args, checks expr at expected type
  and returns a fully analyzed expression with a TCResult u/expr-type entry,
  or returns nil."
  [check-expr {:keys [args] :as expr} expected]
  {:pre [(#{:invoke :host-call} (:op expr))
         (every? (every-pred (comp #{:unanalyzed} :op)
                             (complement u/expr-type))
                 args)
         ((some-fn nil? r/TCResult?) expected)]
   :post [((some-fn nil?
                    (comp r/TCResult? u/expr-type))
           %)]}
  (when (#{2 3} (count args))
    (when (keyword? (-> args second :form))
      (let [expr (cond-> (-> expr
                             (update :args #(mapv check-expr %)))
                   (#{:host-call} (:op expr))
                   (update :target check-expr))
            [ttarget kwr tdefault] (map u/expr-type (:args expr))]
        (assoc expr
               u/expr-type (invoke-kw/invoke-keyword
                             expr
                             kwr
                             ttarget
                             tdefault
                             expected))))))
