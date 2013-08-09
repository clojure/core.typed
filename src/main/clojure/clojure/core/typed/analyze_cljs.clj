(ns clojure.core.typed.analyze-cljs
  (:require [cljs.tools.analyzer :as analyze]
            [cljs.tools.analyzer.hygienic :as hygienic]
            [clojure.core.typed.utils :as u :refer [p]]))

(defn ast-for-form-in-ns
  "Returns an AST node for the form 
  analyzed in the given namespace"
  [nsym form]
  (-> (analyze/analyze-form-in-ns nsym form)
      hygienic/ast-hy))

(defn ast-for-form
  "Returns an AST node for the form"
  [form]
  (-> (analyze/analyze-form form)
      hygienic/ast-hy))

(defn ast-for-ns 
  "Returns a vector of AST nodes contained
  in the given namespace symbol nsym"
  [nsym]
  {:pre [(symbol? nsym)]}
  (map hygienic/ast-hy (analyze/analyze-ns nsym)))
