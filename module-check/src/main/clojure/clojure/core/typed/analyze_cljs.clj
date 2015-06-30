(ns ^:skip-wiki clojure.core.typed.analyze-cljs
  (:refer-clojure :exclude [extenders])
  (:require [clojure.core.typed.current-impl :as impl]
            [cljs.analyzer :as ana]
            [clojure.core.typed.deps.cljs.jvm.tools.analyzer :as jana]
            [clojure.core.typed.deps.cljs.jvm.tools.analyzer.hygienic :as hyg]))

(alter-meta! *ns* assoc :skip-wiki true)

(defn ast-for-form-in-ns
  "Returns an AST node for the form 
  analyzed in the given namespace"
  [nsym form]
  (-> (jana/analyze-form-in-ns nsym form)
      hyg/ast-hy))

(defn ast-for-form
  "Returns an AST node for the form"
  [form {:keys [expected eval-fn] :as opt}]
  ;; TODO support bindings-atom, as in c.c.t.analyze-clj
  ;; TODO propagate analyzer env from opt
  (let [ast (-> (jana/analyze-form form)
                hyg/ast-hy)]
    (if eval-fn
      (eval-fn opt ast)
      ast)))

(defn ast-for-ns 
  "Returns a vector of AST nodes contained
  in the given namespace symbol nsym"
  [nsym]
  {:pre [(symbol? nsym)]}
  (map hyg/ast-hy (jana/analyze-ns nsym)))

(defn extenders
  "Returns a set of descendants for a protocol"
  [psym]
  {:pre [(symbol? psym)
         (namespace psym)]}
  (or (get-in (ana/get-namespace (symbol (namespace psym)))
              [:defs (symbol (name psym)) :impls])
      #{}))

(defn analyze-qualified-symbol 
  "Return a var expr that the fully qualified symbol names"
  [sym]
  {:pre [(symbol? sym)] 
   :post [(= :var (:op %))]}
  (ana/analyze-symbol (ana/empty-env) sym))

;(analyze-qualified-symbol 'cljs.core/ISeq)
;(analyze-qualified-symbol 'cljs.core.SubVec)

