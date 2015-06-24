(ns ^:skip-wiki clojure.core.typed.lex-env
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.filter-rep :as fr]
            [clojure.core.typed.object-rep :as obj]))

(alter-meta! *ns* assoc :skip-wiki true)

(def lex-env? (con/hash-c? con/local-sym? (some-fn r/Type? r/Unique?)))
(def prop-set? (con/set-c? fr/Filter?))
(def alias-env? (con/hash-c? con/local-sym? obj/RObject?))

(defonce ^:dynamic *used-locals* (atom {} :validator ((con/hash-c? symbol? con/boolean?) (some-fn delay? r/Type? r/Unique?))))

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

(defn lookup-alias [sym & {:keys [env]}]
  {:pre [(con/local-sym? sym)
         ((some-fn nil? PropEnv?) env)]
   :post [(obj/RObject? %)]}
  (or (get-in (or env *lexical-env*) [:aliases sym])
      (obj/-id-path sym)))

(defn lookup-local [sym]
  {:pre [(con/local-sym? sym)]
   :post [((some-fn nil? r/Type? r/Unique?) %)]}
  (if (and 
        (= (get-in *used-locals* [:l sym]) true)
        (r/Unique? (get-in *lexical-env* [:l sym])))
    (throw (Exception. "variable used twice"))
    (do
      (swap! *used-locals* assoc sym true)
      (get-in *lexical-env* [:l sym]))))

(defn merge-locals [env new]
  {:pre [(PropEnv? env)]
   :post [(PropEnv? %)]}
  ;(swap! *used-locals* assoc (get-in *used-locals* [:l]) false)
  (-> env
      (update-in [:l] merge new)))

(defmacro with-locals [locals & body]
  `(binding [*lexical-env* (merge-locals *lexical-env* ~locals)]
     ~@body))

; take an environment and (depending on the new object given) either record
; and alias to an existing local or extend the type env directly.
(defn extend-env [env id t o]
  {:pre [(PropEnv? env)
         (con/local-sym? id)
         ((some-fn r/Type? r/Unique?) t)
         (obj/RObject? o)]
   :post [(PropEnv? %)]}
  (cond
    ; no aliasing to add
    (obj/EmptyObject? o)
    (-> env
        (assoc-in [:l id] t))

    (obj/Path? o)
    (if (seq (:path o))
      (-> env
          (assoc-in [:aliases id] o))
      ; if we have an empty path, add a "normal" entry to our
      ; type environment. Not sure why this is needed, Andrew K added
      ; it to TR because tests were failing.
      (-> env
          (assoc-in [:l (:id o)] t)
          (assoc-in [:aliases id] o))
      )))
