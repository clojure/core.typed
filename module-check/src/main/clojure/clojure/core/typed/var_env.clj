(ns clojure.core.typed.var-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed :as t]
            [clojure.set :as set]
            [clojure.core.typed.env :as env]))

(def current-var-annotations-kw ::current-var-annotations)
(def current-nocheck-var?-kw ::current-nocheck-var?)
(def current-used-vars-kw ::current-used-vars)
(def current-checked-var-defs-kw ::current-checked-var-defs)

(defonce ^:dynamic *current-nocheck-var?* nil)
(defonce ^:dynamic *current-used-vars* nil)
(defonce ^:dynamic *current-checked-var-defs* nil)

(defn clj-var-annotations []
  (get @(impl/clj-checker) current-var-annotations-kw {}))

(defn clj-nocheck-var? []
  (get @(impl/clj-checker) current-nocheck-var?-kw {}))

(defn clj-used-vars []
  (get @(impl/clj-checker) current-used-vars-kw {}))

(def var-annotations-con (con/hash-c? (every-pred symbol? namespace) (some-fn delay? r/Type?)))
(def nocheck-var-con (con/set-c? (every-pred symbol? namespace)))
(def used-vars-con (con/set-c? (every-pred symbol? namespace)))
(def checked-var-defs-con (con/set-c? (every-pred symbol? namespace)))

;(defonce CLJ-VAR-ANNOTATIONS (atom {} :validator (con/hash-c? (every-pred symbol? namespace) (some-fn delay? r/Type?))))
;(defonce CLJ-NOCHECK-VAR? (atom #{} :validator (con/set-c? (every-pred symbol? namespace))))
;(defonce CLJ-USED-VARS (atom #{} :validator (con/set-c? (every-pred symbol? namespace))))
;(defonce CLJ-CHECKED-VAR-DEFS (atom #{} :validator (con/set-c? (every-pred symbol? namespace))))

(defonce CLJS-VAR-ANNOTATIONS (atom {} :validator (con/hash-c? (every-pred symbol? namespace) r/Type?)))
;(defonce CLJS-NOCHECK-VAR? (atom #{} :validator (con/set-c? (every-pred symbol? namespace))))
;(defonce CLJS-USED-VARS (atom #{} :validator (con/set-c? (every-pred symbol? namespace))))
;(defonce CLJS-CHECKED-VAR-DEFS (atom #{} :validator (con/set-c? (every-pred symbol? namespace))))

(defonce CLJS-JSVAR-ANNOTATIONS (atom {} :validator (con/hash-c? symbol? r/Type?)))

(defn current-checked-var-defs []
  (let [env *current-checked-var-defs*]
    (assert env "No checked var env bound")
    env))

(defmacro with-lexical-env [env & body]
  `(binding [vs/*lexical-env* ~env]
     ~@body))

(defn var-annotations []
  {:post [(map? %)]}
  (get (env/deref-checker) current-var-annotations-kw {}))

(defn var-no-checks []
  {:post [(set? %)]}
  (get (env/deref-checker) current-nocheck-var?-kw #{}))

(defn used-vars []
  {:post [(set? %)]}
  (get (env/deref-checker) current-used-vars-kw #{}))

(defn checked-vars []
  {:post [(set? %)]}
  (get (env/deref-checker) current-checked-var-defs-kw #{}))

(defn add-var-type [sym type]
  (when-let [old-t ((var-annotations) sym)]
    (when (not= old-t type)
      (println "WARNING: Duplicate var annotation: " sym)
      (flush)))
  (env/swap-checker! assoc-in [current-var-annotations-kw sym] type)
  nil)

(defn check-var? [sym]
  (not (contains? (var-no-checks) sym)))

(defn checked-var-def? [sym]
  (contains? (checked-vars) sym))

(defn used-var? [sym]
  (contains? (used-vars) sym))

(defn add-nocheck-var [sym]
  (env/swap-checker! update current-nocheck-var?-kw (fnil conj #{}) sym)
  nil)

(defn remove-nocheck-var [sym]
  (env/swap-checker! update current-nocheck-var?-kw (fnil disj #{}) sym)
  nil)

(defn add-used-var [sym]
  (env/swap-checker! update current-used-vars-kw (fnil conj #{}) sym)
  nil)

(defn add-checked-var-def [sym]
  (env/swap-checker! update current-checked-var-defs-kw (fnil conj #{}) sym)
  nil)

(defn vars-with-unchecked-defs []
  (set/difference (used-vars)
                  (checked-vars)
                  (var-no-checks)))

(defn reset-current-var-annotations! [m]
  (env/swap-checker! assoc current-var-annotations-kw m)
  nil)

(defn reset-current-nocheck-var?! [nocheck]
  (env/swap-checker! assoc current-nocheck-var?-kw nocheck)
  nil)

(defn reset-current-used-vars! [s]
  (env/swap-checker! assoc current-used-vars-kw s)
  nil)

(defn reset-current-checked-var-defs! [s]
  (env/swap-checker! assoc current-checked-var-defs-kw s)
  nil)

(defn reset-var-type-env! [m nocheck]
  (reset-current-var-annotations! m)
  (reset-current-nocheck-var?! nocheck)
  (reset-current-used-vars! #{})
  (reset-current-checked-var-defs! #{})
  nil)

(defn reset-jsvar-type-env! [m]
  (reset! CLJS-JSVAR-ANNOTATIONS m)
  nil)

(defn jsvar-annotations []
  {:post [%]}
  @CLJS-JSVAR-ANNOTATIONS)

(defn lookup-Var-nofail [nsym]
  {:post [((some-fn nil? r/Type?) %)]}
  (or (let [e (var-annotations)]
        (force (e nsym)))
      (when (impl/checking-clojurescript?)
        ((jsvar-annotations) nsym))))

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
