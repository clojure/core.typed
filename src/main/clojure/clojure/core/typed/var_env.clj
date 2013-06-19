(ns clojure.core.typed.var-env
  (:require (clojure.core.typed
             [utils :as u]
             [type-rep :as r]
             [lex-env :as lex]
             [util-vars :as vs])
            [clojure.set :as set]))

(defonce VAR-ANNOTATIONS (atom {} :validator (u/hash-c? (every-pred symbol? namespace) r/Type?)))
(defonce NOCHECK-VAR? (atom #{} :validator (u/set-c? (every-pred symbol? namespace))))
(defonce USED-VARS (atom #{} :validator (u/set-c? (every-pred symbol? namespace))))
(defonce CHECKED-VAR-DEFS (atom #{} :validator (u/set-c? (every-pred symbol? namespace))))

(defmacro with-lexical-env [env & body]
  `(binding [lex/*lexical-env* ~env]
     ~@body))

(defn add-var-type [sym type]
  (when (contains? @VAR-ANNOTATIONS sym)
    (println "WARNING: Duplicate var annotation: " sym)
    (flush))
  (swap! VAR-ANNOTATIONS #(assoc % sym type))
  nil)

(defn check-var? [sym]
  (not (contains? @NOCHECK-VAR? sym)))

(defn checked-var-def? [sym]
  (contains? @CHECKED-VAR-DEFS sym))

(defn used-var? [sym]
  (contains? @USED-VARS sym))

(defn add-nocheck-var [sym]
  (swap! NOCHECK-VAR? conj sym)
  nil)

(defn add-used-var [sym]
  (swap! USED-VARS conj sym)
  nil)

(defn add-checked-var-def [sym]
  (swap! CHECKED-VAR-DEFS conj sym)
  nil)

(defn vars-with-unchecked-defs []
  (set/difference @USED-VARS
                  @CHECKED-VAR-DEFS
                  @NOCHECK-VAR?))

(defn reset-var-type-env! [m nocheck]
  (reset! VAR-ANNOTATIONS m)
  (reset! NOCHECK-VAR? nocheck)
  (reset! USED-VARS #{})
  (reset! CHECKED-VAR-DEFS #{})
  nil)

(def ^:dynamic *var-annotations*)

(defn lookup-Var [nsym]
  (assert (contains? @*var-annotations* nsym) 
          (str (when vs/*current-env*
                 (str (:line vs/*current-env*) ": "))
            "Untyped var reference: " nsym))
  (@*var-annotations* nsym))

(defn lookup-Var-nofail [nsym]
  (@*var-annotations* nsym))

(defn type-of [sym]
  {:pre [(symbol? sym)]
   :post [(or (r/Type? %)
              (r/TCResult? %))]}
  (cond
    (not (namespace sym)) (if-let [t (lex/lookup-local sym)]
                            t
                            (throw (Exception. (str (when vs/*current-env*
                                                      (str (:line vs/*current-env*) ": "))
                                                    "Reference to untyped binding: " sym
                                                    "\nHint: Has the annotation for " sym
                                                    " been added via check-ns, cf or typed-deps?"))))
    :else (lookup-Var sym)))
