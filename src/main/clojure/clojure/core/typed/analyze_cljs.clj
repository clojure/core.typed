(ns ^:skip-wiki clojure.core.typed.analyze-cljs
  (:refer-clojure :exclude [extenders])
  (:require [cljs.tools.analyzer :as analyze]
            [cljs.tools.analyzer.hygienic :as hygienic]
            [clojure.core.typed.utils :as u :refer [p]]))

(alter-meta! *ns* assoc :skip-wiki true)

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

(defn extenders
  "Returns a set of descendants for a protocol"
  [psym]
  {:pre [(symbol? psym)
         (namespace psym)]}
  (or (get-in @cljs.analyzer/namespaces 
              [(symbol (namespace psym)) :defs 
               (symbol (name psym)) :impls])
      #{}))

(defn analyze-qualified-symbol 
  "Return a var expr that the fully qualified symbol names"
  [sym]
  {:pre [(symbol? sym)] 
   :post [(= :var (:op %))]}
  (cljs.analyzer/analyze-symbol (cljs.analyzer/empty-env) sym))

;(analyze-qualified-symbol 'cljs.core/ISeq)
;(analyze-qualified-symbol 'cljs.core.SubVec)

