(ns ^:skip-wiki clojure.core.typed.analyze-clj
  (:require [clojure.tools.analyzer :as analyze]
            [clojure.tools.analyzer.hygienic :as hygienic]
            [clojure.core.typed.utils :as u]))

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
  (u/p :analyze/ast-for-ns
  (with-open [pbr (analyze/pb-reader-for-ns nsym)]
    (let [astv (->> (analyze/analyze-ns nsym :reader pbr)
                    (map hygienic/ast-hy)
                    vec)]
      astv))))
