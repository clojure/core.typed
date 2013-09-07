(ns ^:skip-wiki clojure.core.typed.analyze-cljs
  (:refer-clojure :exclude [extenders])
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.load-cljs :as load]))

(alter-meta! *ns* assoc :skip-wiki true)

(defn assert-cljs-dep []
  (load/load-cljs)
  (assert (and (find-ns 'cljs.tools.analyzer)
               (find-ns 'cljs.tools.analyzer.hygienic)
               (find-ns 'cljs.analyzer))
          "Clojurescript dependency mising"))

(defn ast-for-form-in-ns
  "Returns an AST node for the form 
  analyzed in the given namespace"
  [nsym form]
  (assert-cljs-dep)
  (let [analyze-form-in-ns (impl/v 'cljs.tools.analyzer/analyze-form-in-ns)
        ast-hy (impl/v 'cljs.tools.analyzer.hygienic/ast-hy)]
    (-> (analyze-form-in-ns nsym form)
        ast-hy)))

(defn ast-for-form
  "Returns an AST node for the form"
  [form]
  (assert-cljs-dep)
  (let [analyze-form (impl/v 'cljs.tools.analyzer/analyze-form)
        ast-hy (impl/v 'cljs.tools.analyzer.hygienic/ast-hy)]
    (-> (analyze-form form)
        ast-hy)))

(defn ast-for-ns 
  "Returns a vector of AST nodes contained
  in the given namespace symbol nsym"
  [nsym]
  {:pre [(symbol? nsym)]}
  (assert-cljs-dep)
  (let [analyze-ns (impl/v 'cljs.tools.analyzer/analyze-ns)
        ast-hy (impl/v 'cljs.tools.analyzer.hygienic/ast-hy)]
    (map ast-hy (analyze-ns nsym))))

(defn extenders
  "Returns a set of descendants for a protocol"
  [psym]
  {:pre [(symbol? psym)
         (namespace psym)]}
  (assert-cljs-dep)
  (let [namespaces (impl/v 'cljs.analyzer/namespaces)]
    (or (get-in @namespaces 
                [(symbol (namespace psym)) :defs 
                 (symbol (name psym)) :impls])
        #{})))

(defn analyze-qualified-symbol 
  "Return a var expr that the fully qualified symbol names"
  [sym]
  {:pre [(symbol? sym)] 
   :post [(= :var (:op %))]}
  (assert-cljs-dep)
  (let [analyze-symbol (impl/v 'cljs.analyzer/analyze-symbol)
        empty-env (impl/v 'cljs.analyzer/empty-env)]
    (analyze-symbol (empty-env) sym)))

;(analyze-qualified-symbol 'cljs.core/ISeq)
;(analyze-qualified-symbol 'cljs.core.SubVec)

