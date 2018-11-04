;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; adapted from tools.analyzer.js
(ns clojure.core.typed.analyzer2.jvm.pre-analyze
  (:require [clojure.core.typed.analyzer2.js.utils
             :refer [desugar-ns-specs validate-ns-specs ns-resource ns->relpath res-path]]
            [clojure.core.typed.analyzer2.pre-analyze :as pre])
  (:import cljs.tagged_literals.JSValue))

; (U ':deftype ':defrecord) Any Config -> AST
(defn parse-type
  [op [_ name fields pmasks body :as form] {:keys [ns] :as env}]
  (let [fields-expr (mapv (fn [name]
                            {:env     env
                             :form    name
                             :name    name
                             :mutable (:mutable (meta name))
                             :local   :field
                             :op      :binding})
                          fields)
        protocols (-> name meta :protocols)

        _ (swap! *env* assoc-in [:namespaces ns :mappings name]
                 {:op        :var
                  :type      true
                  :name      name
                  :ns        ns
                  :fields    fields
                  :protocols protocols})

        body-expr (pre/pre-analyze-child
                    body
                    (assoc env :locals (zipmap fields (map dissoc-env fields-expr))))]

    {:op        op
     :env       env
     :form      form
     :name      name
     :fields    fields-expr
     :body      body-expr
     :pmasks    pmasks
     :protocols protocols
     :children  [:fields :body]}))

;; no ~{foo} support since cljs itself doesn't use it anywhere
(defn pre-parse-js*
  [[_ jsform & args :as form] env]
  (when-not (string? jsform)
    (throw (ex-info "Invalid js* form"
                    (merge {:form form}
                           (-source-info form env)))))
  (let [segs  (loop [segs [] ^String s jsform]
                (let [idx (.indexOf s "~{")]
                  (if (= -1 idx)
                    (conj segs s)
                    (recur (conj segs (subs s 0 idx))
                           (subs s (inc (.indexOf s "}" idx)))))))
        exprs (mapv #(pre/pre-analyze-child % (ctx env :ctx/expr)) args)]
    (merge
     {:op       :js
      :env      env
      :form     form
      :segs     segs}
     (when args
       {:args     exprs
        :children [:args]}))))

(defn pre-parse-case*
  [[_ test tests thens default :as form] env]
  (assert (symbol? test) "case* must switch on symbol")
  (assert (every? vector? tests) "case* tests must be grouped in vectors")
  (let [expr-env (ctx env :expr)
        test-expr (pre/pre-analyze-child test expr-env)
        nodes (mapv (fn [tests then]
                      {:op       :case-node
                       ;; no :form, this is a synthetic grouping node
                       :env      env
                       :tests    (mapv (fn [test]
                                         {:op       :case-test
                                          :form     test
                                          :env      expr-env
                                          :test     (pre/pre-analyze-child test expr-env)
                                          :children [:test]})
                                       tests)
                       :then     {:op       :case-then
                                  :form     test
                                  :env      env
                                  :then     (-analyze then env)
                                  :children [:then]}
                       :children [:tests :then]})
                    tests thens)
        default-expr (-analyze default env)]
    (assert (every? (fn [t] (and (= :const (-> t :test :op))
                           ((some-fn number? string?) (:form t))))
               (mapcat :tests nodes))
            "case* tests must be numbers or strings")
    {:op       :case
     :form     form
     :env      env
     :test     (assoc test-expr :case-test true)
     :nodes    nodes
     :default  default-expr
     :children [:test :nodes :default]}))

(defn pre-parse-ns
  [[_ name & args :as form] env]
  (when-not (symbol? name)
    (throw (ex-info (str "Namespaces must be named by a symbol, had: "
                         (.getName ^Class (class name)))
                    (merge {:form form}
                           (-source-info form env)))))
  (let [[docstring & args] (if (string? (first args))
                             args
                             (cons nil args))
        [metadata & args]  (if (map? (first args))
                             args
                             (cons {} args))
        name (vary-meta name merge metadata)
        ns-opts (doto (desugar-ns-specs args form env)
                  (validate-ns-specs form env)
                  (populate-env name env))]
    (set! *ns* name)
    (merge
     {:op      :ns
      :env     env
      :form    form
      :name    name
      :depends (set (keys (:require ns-opts)))}
     (when docstring
       {:doc docstring})
     (when metadata
       {:meta metadata}))))

(defn pre-parse-def
  [[_ sym & rest :as form] env]
  (let [ks #{:ns :name :doc :arglists :file :line :column}
        meta (meta sym)
        m (merge {}
                 (update-vals (select-keys meta ks) (fn [x] (list 'quote x)))
                 (when (:test meta)
                   {:test `(.-cljs$lang$test ~sym)}))]
    (pre/pre-analyze-child (with-meta `(def ~(with-meta sym m) ~@rest) (meta form)) env)))

;; can it be :literal ?
(defn pre-parse-js-value
  [form env]
  (let [val (.val ^JSValue form)
        items-env (ctx env :expr)]
    (if (map? val)
      ;; keys should always be symbols/kewords, do we really need to analyze them?
      {:op       :js-object
       :env      env
       :keys     (mapv #(pre/pre-analyze-child % items-env) (keys val))
       :vals     (mapv #(pre/pre-analyze-child % items-env) (vals val))
       :form     form
       :children [:keys :vals]}
      {:op       :js-array
       :env      env
       :items    (mapv #(pre/pre-analyze-child % items-env) val)
       :form     form
       :children [:items]})))

(defn pre-parse
  "Extension to clojure.core.typed.analyzer2.pre-analyze/-pre-parse for JS special forms"
  [form env]
  (cond
    (instance? JSValue form) (pre-parse-js-value form env)
    :else
    ((case (first form)
       deftype*   #(pre-parse-deftype* :deftype %1 %2)
       defrecord* #(pre-parse-deftype* :defrecord %1 %2)
       case*      pre-parse-case*
       ns         pre-parse-ns
       def        pre-parse-def
       js*        pre-parse-js*
       #_:else    pre/-pre-parse)
     form env)))
