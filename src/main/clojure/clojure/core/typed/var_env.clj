(ns clojure.core.typed.var-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed :as t]
            [clojure.set :as set]))

(defonce ^:dynamic *current-var-annotations* nil)
(defonce ^:dynamic *current-nocheck-var?* nil)
(defonce ^:dynamic *current-used-vars* nil)
(defonce ^:dynamic *current-checked-var-defs* nil)

(defonce CLJ-VAR-ANNOTATIONS (atom {} :validator (con/hash-c? (every-pred symbol? namespace) (some-fn delay? r/Type?))))
(defonce CLJ-NOCHECK-VAR? (atom #{} :validator (con/set-c? (every-pred symbol? namespace))))
(defonce CLJ-USED-VARS (atom #{} :validator (con/set-c? (every-pred symbol? namespace))))
(defonce CLJ-CHECKED-VAR-DEFS (atom #{} :validator (con/set-c? (every-pred symbol? namespace))))

(defonce CLJS-VAR-ANNOTATIONS (atom {} :validator (con/hash-c? (every-pred symbol? namespace) r/Type?)))
(defonce CLJS-NOCHECK-VAR? (atom #{} :validator (con/set-c? (every-pred symbol? namespace))))
(defonce CLJS-USED-VARS (atom #{} :validator (con/set-c? (every-pred symbol? namespace))))
(defonce CLJS-CHECKED-VAR-DEFS (atom #{} :validator (con/set-c? (every-pred symbol? namespace))))

(defonce CLJS-JSVAR-ANNOTATIONS (atom {} :validator (con/hash-c? symbol? r/Type?)))

(defn current-var-annotations []
  (let [env *current-var-annotations*]
    (assert env "No var annotations env bound")
    env))

(defn current-nocheck-var? []
  (let [env *current-nocheck-var?*]
    (assert env "No var nocheck env bound")
    env))

(defn current-used-vars []
  (let [env *current-used-vars*]
    (assert env "No used var env bound")
    env))

(defn current-checked-var-defs []
  (let [env *current-checked-var-defs*]
    (assert env "No checked var env bound")
    env))

(defmacro with-lexical-env [env & body]
  `(binding [vs/*lexical-env* ~env]
     ~@body))

(defn var-annotations []
  @(current-var-annotations))

(defn var-no-checks []
  @(current-nocheck-var?))

(defn used-vars []
  @(current-used-vars))

(defn checked-vars []
  @(current-checked-var-defs))

(defn add-var-type [sym type]
  (when-let [old-t (@(current-var-annotations) sym)]
    (when (not= old-t type)
      (println "WARNING: Duplicate var annotation: " sym)
      (flush)))
  (swap! (current-var-annotations) assoc sym type)
  nil)

(defn check-var? [sym]
  (not (contains? @(current-nocheck-var?) sym)))

(defn checked-var-def? [sym]
  (contains? @(current-checked-var-defs) sym))

(defn used-var? [sym]
  (contains? @(current-used-vars) sym))

(defn add-nocheck-var [sym]
  (swap! (current-nocheck-var?) conj sym)
  nil)

(defn remove-nocheck-var [sym]
  (swap! (current-nocheck-var?) disj sym)
  nil)

(defn add-used-var [sym]
  (swap! (current-used-vars) conj sym)
  nil)

(defn add-checked-var-def [sym]
  (swap! (current-checked-var-defs) conj sym)
  nil)

(defn vars-with-unchecked-defs []
  (set/difference @(current-used-vars)
                  @(current-checked-var-defs)
                  @(current-nocheck-var?)))

(defn reset-var-type-env! [m nocheck]
  (reset! (current-var-annotations) m)
  (reset! (current-nocheck-var?) nocheck)
  (reset! (current-used-vars) #{})
  (reset! (current-checked-var-defs) #{})
  nil)

(defn reset-jsvar-type-env! [m]
  (reset! CLJS-JSVAR-ANNOTATIONS m)
  nil)

(defn lookup-Var-nofail [nsym]
  {:post [((some-fn nil? r/Type?) %)]}
  (or (let [e (current-var-annotations)]
        (force (@e nsym)))
      (when (impl/checking-clojurescript?)
        (@CLJS-JSVAR-ANNOTATIONS nsym))))

(defn lookup-Var [nsym]
  {:post [((some-fn nil? r/Type?) %)]}
  (if-let [t (lookup-Var-nofail nsym)]
    t
    (err/int-error
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
    (err/int-error (str (when vs/*current-env*
                          (str (:line vs/*current-env*) ": "))
                        "Reference to untyped binding: " sym
                        "\nHint: Add the annotation for " sym
                        " via check-ns or cf"))))
