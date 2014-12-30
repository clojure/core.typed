(ns clojure.core.typed.import-macros
  (:require [clojure.core :as core]))

;copied from ClojureScript
(defmacro import-macros [ns [& vars]]
  (core/let [ns (find-ns ns)
             vars (map (core/fn [vsym]
                         {:pre [(symbol? vsym)]
                          :post [(instance? clojure.lang.Var %)]}
                         (let [v (ns-resolve ns vsym)]
                           (assert v (str "Internal error: " vsym " does not exist"))
                           v))
                       vars)
             syms (map (core/fn [^clojure.lang.Var v] 
                         {:pre [(instance? clojure.lang.Var v)]
                          :post [(symbol? %)]}
                         (core/-> v .sym (with-meta {:macro true})))
                       vars)
             defs (map (core/fn [sym var]
                         {:pre [(symbol? sym)
                                (instance? clojure.lang.Var var)]}
                         `(do (def ~sym (deref ~var))
                              ;for AOT compilation
                              (alter-meta! (var ~sym) 
                                           merge
                                           (dissoc (meta ~var) :ns :name)
                                           {:macro true})))
                       syms vars)]
    `(do ~@defs
         :imported)))
