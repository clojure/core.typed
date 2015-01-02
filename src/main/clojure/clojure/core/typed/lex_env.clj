(ns ^:skip-wiki clojure.core.typed.lex-env
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.filter-rep :as fr]
            [clojure.core.typed.object-rep :as obj]))

(alter-meta! *ns* assoc :skip-wiki true)

(def lex-env? (con/hash-c? con/local-sym? r/Type?))
(def prop-set? (con/set-c? fr/Filter?))
(def alias-env? (con/hash-c? con/local-sym? obj/RObject?))

(u/defrecord PropEnv [l props aliases]
  "A lexical environment l, props is a list of known propositions"
  [(lex-env? l)
   (prop-set? props)
   (alias-env? aliases)])

(defn -PropEnv 
  ([] (-PropEnv {} #{} {}))
  ([l props]
   (-PropEnv l props {}))
  ([l props aliases]
   (->PropEnv l 
              (if (set? props)
                props
                (into #{} props))
              aliases)))

(defonce ^:dynamic *lexical-env* (-PropEnv))
(set-validator! #'*lexical-env* (fn [a]
                                  (or (PropEnv? a)
                                      ;work around for recompilation issues with AOT
                                      (= "clojure.core.typed.lex_env.PropEnv"
                                         (.getName (class a))))))

(defn lookup-local [sym]
  (-> *lexical-env* :l sym))

(defn merge-locals [env new]
  (-> env
    (update-in [:l] #(merge % new))))

(defmacro with-locals [locals & body]
  `(binding [*lexical-env* (merge-locals *lexical-env* ~locals)]
     ~@body))

; take an environment and (depending on the new object given) either record
; and alias to an existing local or extend the type env directly.
(defn extend-env [env id t o]
  {:pre [(PropEnv? env)
         (con/local-sym? id)
         (r/Type? t)
         (obj/RObject? o)]
   :post [(PropEnv? %)]}
  (assoc-in env [:l id] t)
  #_(cond
    ; no aliasing to add
    (obj/EmptyObject? o)
    (-> env
        (assoc-in [:l id] t))

    (obj/Path? o)
    (-> env
        (assoc-in [:aliases id] o))))
