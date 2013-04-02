(ns clojure.core.typed.lex-env
  (:require [clojure.core.typed
             [utils :as u]
             [type-rep :as r]
             [filter-rep :as fr]]))

;(ann (predicate (APersistentMap Symbol Any)))
(def lex-env? (u/hash-c? (every-pred symbol? (complement namespace)) r/Type?))

(u/defrecord PropEnv [l props]
  "A lexical environment l, props is a list of known propositions"
  [(lex-env? l)
   (set? props)
   (every? fr/Filter? props)])

(defn -PropEnv [l props]
  (->PropEnv l (if (set? props)
                 props
                 (into #{} props))))

(def ^:dynamic *lexical-env* (-PropEnv {} #{}))
(set-validator! #'*lexical-env* PropEnv?)

(defn lookup-local [sym]
  (-> *lexical-env* :l sym))

(defn merge-locals [env new]
  (-> env
    (update-in [:l] #(merge % new))))

(defmacro with-locals [locals & body]
  `(binding [*lexical-env* (merge-locals *lexical-env* ~locals)]
     ~@body))
