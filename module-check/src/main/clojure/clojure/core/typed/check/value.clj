(ns clojure.core.typed.check.value
  (:require [clojure.core.typed.constant-type :as const]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.check-below :as below]
            [clojure.core.typed.type-rep :as r]))

(defn flow-for-value []
  (let [props (:props (lex/lexical-env))
        flow (r/-flow (apply fo/-and fl/-top props))]
    flow))

(defn filter-for-value [val]
  (if val
    (fo/-FS fl/-top fl/-bot)
    (fo/-FS fl/-bot fl/-top)))

(defn unquote-val
  "Convert the syntax representation of a unevaluated value to
  an actual evaluated value.
  
  eg. ['a] is represented as [(quote a)] and evaluates to [a]"
  [val]
  (letfn [(unwrap-quote [val]
            (if (and (seq? val)
                     ('#{quote} (first val)))
              (second val)
              (unquote-val val)))]
    (cond
      (vector? val) (mapv unwrap-quote val)
      (map? val) (reduce-kv
                   (fn [m k v]
                     (assoc m (unwrap-quote k) (unwrap-quote v)))
                   {}
                   val)
      :else (if (and (seq? val)
                     ('#{quote} (first val)))
              (second val)
              val))))

(defn check-value
  "Given a :const node and an expected type returns a new :const
  node annotated with its type.
  
  quoted? should be true if this :const node is nested inside a
  :quote node, otherwise should be false"
  [{:keys [val] :as expr} expected quoted?]
  {:pre [(#{:const} (:op expr))
         ((some-fn nil? r/TCResult?) expected)]
   :post [(-> % u/expr-type r/TCResult?)]}
  ;(prn "check-value" val expected)
  (binding [vs/*current-expr* expr]
    (let [val (if quoted?
                val
                (unquote-val val))
          inferred-ret (r/ret (const/constant-type val)
                              (filter-for-value val)
                              obj/-empty
                              (flow-for-value))]
      (assoc expr
             u/expr-type (below/maybe-check-below inferred-ret expected)))))
