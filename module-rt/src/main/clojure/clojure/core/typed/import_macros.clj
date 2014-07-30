(ns clojure.core.typed.import-macros
  (:require [clojure.core :as core]))

;copied from ClojureScript
(defmacro import-macros [ns [& vars]]
  (core/let [ns (find-ns ns)
             vars (map #(ns-resolve ns %) vars)
             syms (map (core/fn [^clojure.lang.Var v] (core/-> v .sym (with-meta {:macro true}))) vars)
             defs (map (core/fn [sym var]
                         `(do (def ~sym (deref ~var))
                              ;for AOT compilation
                              (alter-meta! (var ~sym) 
                                           merge
                                           (dissoc (meta ~var) :ns :name)
                                           {:macro true})))
                       syms vars)]
    `(do ~@defs
         :imported)))
