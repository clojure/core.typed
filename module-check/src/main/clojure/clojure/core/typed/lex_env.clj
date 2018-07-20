(ns ^:skip-wiki clojure.core.typed.lex-env
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.path-rep :as pr]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.indirect-utils :as indu]
            [clojure.core.typed.indirect-ops :as ind]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.filter-rep :as fr]
            [clojure.core.typed.path-type :as path-type]
            [clojure.core.typed.object-rep :as obj]))

(def lex-env? (con/hash-c? con/local-sym? r/Type?))
(def prop-set? (con/set-c? fr/Filter?))
(def alias-env? (con/hash-c? con/local-sym? obj/RObject?))

(u/def-type PropEnv [l props aliases]
  "A lexical environment l, props is a list of known propositions"
  [(lex-env? l)
   (prop-set? props)
   (alias-env? aliases)])

(defn -PropEnv 
  ([] (-PropEnv {} #{} {}))
  ([l props]
   (-PropEnv l props {}))
  ([l props aliases]
   (PropEnv-maker l 
              (if (set? props)
                props
                (into #{} props))
              aliases)))

(defn init-lexical-env []
  (-PropEnv))

(defn lexical-env []
  vs/*lexical-env*)

(set-validator! #'vs/*lexical-env* (fn [a]
                                     (or (nil? a)
                                         (PropEnv? a)
                                         ;work around for recompilation issues with AOT
                                         (= "clojure.core.typed.lex_env.PropEnv"
                                            (.getName (class a))))))

(defn lookup-alias [sym & {:keys [env]}]
  {:pre [(con/local-sym? sym)
         ((some-fn nil? PropEnv?) env)]
   :post [(obj/RObject? %)]}
  (or (get-in (or env (lexical-env)) [:aliases sym])
      (obj/-id-path sym)))

(defn lookup-local [sym]
  {:pre [(con/local-sym? sym)]
   :post [((some-fn nil? r/Type?) %)]}
  (let [; see if sym is an alias for an object
        ; if not (-id-path sym) is returned
        obj (lookup-alias sym)
        [alias-path alias-id] (cond
                                (obj/Path? obj) [(:path obj) (:id obj)]
                                (obj/EmptyObject? obj) [nil sym]
                                :else (err/int-error (str "what is this?" (pr-str obj))))
        _ (assert (pr/path-elems? alias-path))
        _ (assert (fr/name-ref? alias-id))
        lt (get-in (lexical-env) [:l alias-id])]
    (when lt
      (path-type/path-type lt alias-path))))

(defn merge-locals [env new]
  {:pre [(PropEnv? env)]
   :post [(PropEnv? %)]}
  (-> env
      (update-in [:l] merge new)))

(defmacro with-locals [locals & body]
  `(binding [vs/*lexical-env* (merge-locals (lexical-env) ~locals)]
     ~@body))

; take an environment and (depending on the new object given) either record
; and alias to an existing local or extend the type env directly.
(defn extend-env [env id t o]
  {:pre [(PropEnv? env)
         (con/local-sym? id)
         (r/Type? t)
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

(indu/add-indirection ind/PropEnv? PropEnv?)
