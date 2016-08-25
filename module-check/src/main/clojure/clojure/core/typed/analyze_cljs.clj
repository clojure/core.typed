(ns ^:skip-wiki clojure.core.typed.analyze-cljs
  (:refer-clojure :exclude [extenders])
  (:require [clojure.core.typed.current-impl :as impl]))

(defn ast-for-form-in-ns
  "Returns an AST node for the form 
  analyzed in the given namespace"
  [nsym form]
  (require 'cljs.jvm.tools.analyzer)
  (require 'cljs.jvm.tools.analyzer.hygienic)
  (-> ((impl/v 'cljs.jvm.tools.analyzer/analyze-form-in-ns) nsym form)
      ((impl/v 'cljs.jvm.tools.analyzer.hygienic/ast-hy)
                 )))

(defn ast-for-form
  "Returns an AST node for the form"
  [form {:keys [expected eval-fn] :as opt}]
  ;; TODO support bindings-atom, as in c.c.t.analyze-clj
  ;; TODO propagate analyzer env from opt
  (require 'cljs.jvm.tools.analyzer)
  (require 'cljs.jvm.tools.analyzer.hygienic)
  (let [ast (-> ((impl/v 'cljs.jvm.tools.analyzer/analyze-form) form)
                ((impl/v 'cljs.jvm.tools.analyzer.hygienic/ast-hy)
                 ))]
    (if eval-fn
      (eval-fn opt ast)
      ast)))

(defn ast-for-ns 
  "Returns a vector of AST nodes contained
  in the given namespace symbol nsym"
  [nsym]
  {:pre [(symbol? nsym)]}
  (require 'cljs.jvm.tools.analyzer)
  (require 'cljs.jvm.tools.analyzer.hygienic)
  (map (impl/v 'cljs.jvm.tools.analyzer.hygienic/ast-hy)
       ((impl/v 'cljs.jvm.tools.analyzer/analyze-ns) nsym)))

(defn extenders
  "Returns a set of descendants for a protocol"
  [psym]
  {:pre [(symbol? psym)
         (namespace psym)]}
  (require 'cljs.analyzer)
  (or (get-in ((impl/v 'cljs.analyzer/get-namespace) (symbol (namespace psym)))
              [:defs (symbol (name psym)) :impls])
      #{}))

(defn analyze-qualified-symbol 
  "Return a var expr that the fully qualified symbol names"
  [sym]
  {:pre [(symbol? sym)] 
   :post [(= :var (:op %))]}
  (require 'cljs.analyzer)
  ((impl/v 'cljs.analyzer/analyze-symbol) 
   ((impl/v 'cljs.analyzer/empty-env)) sym))

;(analyze-qualified-symbol 'cljs.core/ISeq)
;(analyze-qualified-symbol 'cljs.core.SubVec)

