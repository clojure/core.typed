(ns clojure.core.typed.var-env
  (:require [clojure.core.typed
             [type-rep :as r]
             [lex-env :as lex]
             [util-vars :as vs]]))

(defonce VAR-ANNOTATIONS (atom {}))

(defmacro with-lexical-env [env & body]
  `(binding [lex/*lexical-env* ~env]
     ~@body))

(set-validator! VAR-ANNOTATIONS #(and (every? (every-pred symbol? namespace) (keys %))
                                      (every? r/Type? (vals %))))

(defn add-var-type [sym type]
  (swap! VAR-ANNOTATIONS #(assoc % sym type))
  nil)

(defn reset-var-type-env! [m]
  (reset! VAR-ANNOTATIONS m)
  nil)

(def ^:dynamic *var-annotations*)

(defn lookup-Var [nsym]
  (assert (contains? @*var-annotations* nsym) 
          (str (when vs/*current-env*
                 (str (:line vs/*current-env*) ": "))
            "Untyped var reference: " nsym))
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
                                                    "Reference to untyped binding: " sym))))
    :else (lookup-Var sym)))
