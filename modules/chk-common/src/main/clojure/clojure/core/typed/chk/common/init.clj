(ns clojure.core.typed.chk.common.init
  (:require [clojure.core.typed.rt.jvm.current-impl :as impl]
            [clojure.core.typed.chk.common.profiling :as p]
            [clojure.java.io :as io]))

(defonce ^:private attempted-loading? (atom false))
(defonce ^:private successfully-loaded? (atom false))

(defonce ^:private cljs-present? (atom false))

(defn loaded? []
  @successfully-loaded?)

(defn cljs? []
  @cljs-present?)

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
        (require '[clojure.core.typed.chk.common.utils]
                 '[clojure.core.typed.chk.common.type-rep]
                 '[clojure.core.typed.chk.common.type-ctors]
                 '[clojure.core.typed.chk.common.filter-rep]
                 '[clojure.core.typed.chk.common.filter-ops]
                 '[clojure.core.typed.chk.common.subst]
                 '[clojure.core.typed.chk.common.path-rep]
                 '[clojure.core.typed.chk.common.object-rep]
                 '[clojure.core.typed.chk.common.fold-rep]
                 '[clojure.core.typed.chk.common.fold-default]
                 '[clojure.core.typed.chk.common.parse-unparse]
                 '[clojure.core.typed.chk.common.lex-env]
                 '[clojure.core.typed.chk.common.var-env]
                 '[clojure.core.typed.chk.common.parse-unparse]
                 '[clojure.core.typed.rt.jvm.current-impl]
                 '[clojure.core.typed.chk.common.dvar-env]
                 '[clojure.core.typed.chk.common.datatype-ancestor-env]
                 '[clojure.core.typed.chk.common.datatype-env]
                 '[clojure.core.typed.chk.common.protocol-env]
                 '[clojure.core.typed.impl.jvm.method-override-env]
                 '[clojure.core.typed.impl.jvm.ctor-override-env]
                 '[clojure.core.typed.impl.jvm.method-return-nilables]
                 '[clojure.core.typed.impl.jvm.method-param-nilables]
                 '[clojure.core.typed.chk.common.declared-kind-env]
                 '[clojure.core.typed.chk.common.name-env]
                 '[clojure.core.typed.impl.jvm.rclass-env]
                 '[clojure.core.typed.chk.common.mm-env]
                 '[clojure.core.typed.impl.jvm.constant-type]
                 '[clojure.core.typed.chk.common.parse-unparse]
                 '[clojure.core.typed.chk.common.frees]
                 '[clojure.core.typed.chk.common.free-ops]
                 '[clojure.core.typed.chk.common.cs-gen]
                 '[clojure.core.typed.chk.common.trans]
                 '[clojure.core.typed.chk.common.inst]
                 '[clojure.core.typed.chk.common.subtype]
                 '[clojure.core.typed.rt.jvm.array-ops]
                 '[clojure.core.typed.chk.common.check]
                 '[clojure.core.typed.chk.common.collect-phase]
                 '[clojure.core.typed.impl.jvm.base-env]
                 '[clojure.core.typed.chk.common.ns-deps]
                 '[clojure.core.typed.chk.common.reset-env]
                 '[clojure.core.typed.chk.common.tvar-env]
                 '[clojure.core.typed.chk.common.tvar-bnds]
                 '[clojure.core.typed.impl.jvm.rclass-ancestor-env]
                 '[clojure.reflect])
        (when (io/resource "cljs/analyzer.clj")
          (do
            (println "Found ClojureScript, loading ...")
            (flush)
            (require
              '[cljs.analyzer]
              '[clojure.core.typed.impl.js.collect-cljs]
              '[clojure.core.typed.chk.common.check-cljs]
              '[clojure.core.typed.impl.js.jsnominal-env]
              '[clojure.core.typed.impl.js.base-env-cljs]
              '[clojure.core.typed.impl.js.base-env-helper-cljs])
            (reset! cljs-present? true)
            (println "Finished loading ClojureScript")
            (flush)))
        (catch Exception e
          (reset! successfully-loaded? false)
          (throw e)))
      (reset! successfully-loaded? true)
      (println "Building core.typed base environments ...")
      (flush)
      (impl/with-clojure-impl
        ((impl/v 'clojure.core.typed.chk.common.reset-env/reset-envs!)))
      (when (cljs?)
        (impl/with-cljs-impl
          ((impl/v 'clojure.core.typed.chk.common.reset-env/reset-envs!))))
      (println "Finished building base environments")
      (flush)
      nil)))
