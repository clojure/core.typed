(ns clojure.core.typed.var-env
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed :as t]
            [clojure.set :as set]))

(def ^:dynamic *current-var-annotations* nil)
(def ^:dynamic *current-nocheck-var?* nil)
(def ^:dynamic *current-used-vars* nil)
(def ^:dynamic *current-checked-var-defs* nil)

(defonce CLJ-VAR-ANNOTATIONS (atom {} :validator (u/hash-c? (every-pred symbol? namespace) r/Type?)))
(defonce CLJ-NOCHECK-VAR? (atom #{} :validator (u/set-c? (every-pred symbol? namespace))))
(defonce CLJ-USED-VARS (atom #{} :validator (u/set-c? (every-pred symbol? namespace))))
(defonce CLJ-CHECKED-VAR-DEFS (atom #{} :validator (u/set-c? (every-pred symbol? namespace))))

(defonce CLJS-VAR-ANNOTATIONS (atom {} :validator (u/hash-c? (every-pred symbol? namespace) r/Type?)))
(defonce CLJS-NOCHECK-VAR? (atom #{} :validator (u/set-c? (every-pred symbol? namespace))))
(defonce CLJS-USED-VARS (atom #{} :validator (u/set-c? (every-pred symbol? namespace))))
(defonce CLJS-CHECKED-VAR-DEFS (atom #{} :validator (u/set-c? (every-pred symbol? namespace))))

(defonce CLJS-JSVAR-ANNOTATIONS (atom {} :validator (u/hash-c? symbol? r/Type?)))

(defmacro with-lexical-env [env & body]
  `(binding [lex/*lexical-env* ~env]
     ~@body))

(defn assert-var-env []
  (assert *current-var-annotations*)
  (assert *current-nocheck-var?*)
  (assert *current-used-vars*)
  (assert *current-checked-var-defs*))

(defn add-var-type [sym type]
  (assert-var-env)
  (when (contains? @*current-var-annotations* sym)
    (println "WARNING: Duplicate var annotation: " sym)
    (flush))
  (swap! *current-var-annotations* #(assoc % sym type))
  nil)

(defn check-var? [sym]
  (assert-var-env)
  (not (contains? @*current-nocheck-var?* sym)))

(defn checked-var-def? [sym]
  (assert-var-env)
  (contains? @*current-checked-var-defs* sym))

(defn used-var? [sym]
  (assert-var-env)
  (contains? @*current-used-vars* sym))

(defn add-nocheck-var [sym]
  (assert-var-env)
  (swap! *current-nocheck-var?* conj sym)
  nil)

(defn add-used-var [sym]
  (assert-var-env)
  (swap! *current-used-vars* conj sym)
  nil)

(defn add-checked-var-def [sym]
  (assert-var-env)
  (swap! *current-checked-var-defs* conj sym)
  nil)

(defn vars-with-unchecked-defs []
  (assert-var-env)
  (set/difference @*current-used-vars*
                  @*current-checked-var-defs*
                  @*current-nocheck-var?*))

(defn reset-var-type-env! [m nocheck]
  (assert-var-env)
  (reset! *current-var-annotations* m)
  (reset! *current-nocheck-var?* nocheck)
  (reset! *current-used-vars* #{})
  (reset! *current-checked-var-defs* #{})
  nil)

(defn reset-jsvar-type-env! [m]
  (reset! CLJS-JSVAR-ANNOTATIONS m)
  nil)

(defn lookup-Var-nofail [nsym]
  (assert-var-env)
  (or (t/when-let-fail [e *current-var-annotations*]
        (@e nsym))
      (when (impl/checking-clojurescript?)
        (@CLJS-JSVAR-ANNOTATIONS nsym))))

(defn lookup-Var [nsym]
  {:post [%]}
  (assert-var-env)
  (if-let [t (lookup-Var-nofail nsym)]
    t
    (u/int-error
      (str "Untyped var reference: " nsym))))

(defn type-of-nofail [sym]
  {:pre [(symbol? sym)]
   :post [((some-fn nil? r/Type?) %)]}
  (if (and (not (namespace sym))
           (not-any? #{\.} (str sym))) 
    (lex/lookup-local sym)
    (lookup-Var-nofail sym)))

(defn type-of [sym]
  {:pre [(symbol? sym)]
   :post [(r/Type? %)]}
  (if-let [t (type-of-nofail sym)]
    t
    (throw (Exception. (str (when vs/*current-env*
                              (str (:line vs/*current-env*) ": "))
                            "Reference to untyped binding: " sym
                            "\nHint: Has the annotation for " sym
                            " been added via check-ns, cf or typed-deps?")))))
