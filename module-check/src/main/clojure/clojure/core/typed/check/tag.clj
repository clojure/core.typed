(ns clojure.core.typed.check.tag
  (:require [clojure.tools.analyzer.jvm.utils :as ana-u]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.coerce-utils :as coerce]))

(defn check-tag [expr tag-sym]
  {:pre [(symbol? tag-sym)]
   :post [(nil? %)]}
  (let [actual-tag (:tag expr)
        expected-tag (ana-u/maybe-class tag-sym)
        _ (if expected-tag
            (when-not (isa? actual-tag expected-tag)
              (binding [vs/*current-expr* expr]
                (err/tc-delayed-error (str "Expected tag " tag-sym
                                           ", actual: " (coerce/Class->symbol actual-tag)))))
            (err/tc-delayed-error
              (str "Cannot resolve tag: " tag-sym)))]
    nil))
