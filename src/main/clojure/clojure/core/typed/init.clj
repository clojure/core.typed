(ns clojure.core.typed.init
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.profiling :as p]))

(defonce ^:private attempted-loading? (atom false))
(defonce ^:private successfully-loaded? (atom false))

(defn loaded? []
  @successfully-loaded?)

(defn load-impl []
  (cond 
    (and @attempted-loading?
         (not @successfully-loaded?))
    (throw (Exception. 
             (str "There was previously an unrecoverable internal error while loading core.typed." 
                  " Please restart your process.")))

    (and @successfully-loaded? @attempted-loading?)
    nil

    :else
    (do
      (try
        (reset! attempted-loading? true)
        (require '[clojure.core.typed.utils]
                 '[clojure.core.typed.type-rep]
                 '[clojure.core.typed.type-ctors]
                 '[clojure.core.typed.filter-rep]
                 '[clojure.core.typed.filter-ops]
                 '[clojure.core.typed.subst]
                 '[clojure.core.typed.path-rep]
                 '[clojure.core.typed.object-rep]
                 '[clojure.core.typed.fold-rep]
                 '[clojure.core.typed.fold-default]
                 '[clojure.core.typed.parse-unparse]
                 '[clojure.core.typed.lex-env]
                 '[clojure.core.typed.var-env]
                 '[clojure.core.typed.parse-unparse]
                 '[clojure.core.typed.current-impl]
                 '[clojure.core.typed.dvar-env]
                 '[clojure.core.typed.datatype-ancestor-env]
                 '[clojure.core.typed.datatype-env]
                 '[clojure.core.typed.protocol-env]
                 '[clojure.core.typed.method-override-env]
                 '[clojure.core.typed.ctor-override-env]
                 '[clojure.core.typed.method-return-nilables]
                 '[clojure.core.typed.method-param-nilables]
                 '[clojure.core.typed.declared-kind-env]
                 '[clojure.core.typed.name-env]
                 '[clojure.core.typed.rclass-env]
                 '[clojure.core.typed.mm-env]
                 '[clojure.core.typed.constant-type]
                 '[clojure.core.typed.parse-unparse]
                 '[clojure.core.typed.frees]
                 '[clojure.core.typed.free-ops]
                 '[clojure.core.typed.cs-gen]
                 '[clojure.core.typed.trans]
                 '[clojure.core.typed.inst]
                 '[clojure.core.typed.subtype]
                 '[clojure.core.typed.array-ops]
                 '[clojure.core.typed.check]
                 '[clojure.core.typed.collect-phase]
                 '[clojure.core.typed.base-env]
                 '[clojure.core.typed.ns-deps]
                 '[clojure.core.typed.reset-env]
                 '[clojure.core.typed.tvar-env]
                 '[clojure.core.typed.tvar-bnds]
                 '[clojure.core.typed.rclass-ancestor-env]
                 '[clojure.reflect]
                 ;cljs
                 '[clojure.core.typed.collect-cljs]
                 '[clojure.core.typed.check-cljs]
                 '[clojure.core.typed.jsnominal-env]
                 '[clojure.core.typed.base-env-cljs]
                 '[clojure.core.typed.base-env-helper-cljs])
        (catch Exception e
          (reset! successfully-loaded? false)
          (throw e)))
      (reset! successfully-loaded? true)
      (@(ns-resolve (find-ns 'clojure.core.typed.reset-env) 'reset-envs!))
      nil)))
