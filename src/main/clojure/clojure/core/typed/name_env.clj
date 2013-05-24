(ns clojure.core.typed.name-env
  (:require [clojure.core.typed
             [type-rep :as r]
             [utils :as u]
             [datatype-env :as dtenv]
             [protocol-env :as prenv]]
            [clojure.core.typed :as t :refer [fn>]])
  (:import (clojure.lang Symbol IPersistentMap Keyword)
           #_(clojure.core.typed.type_rep )))

(t/typed-deps clojure.core.typed.type-rep
              clojure.core.typed.utils
              clojure.core.typed.protocol-env
              clojure.core.typed.datatype-env)

(t/def-alias NameEnv
  "Environment mapping names to types. Keyword values are special."
  (IPersistentMap Symbol (U Keyword r/TCType)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Name Env

(t/ann declared-name-type Keyword)
(t/ann protocol-name-type Keyword)
(t/ann datatype-name-type Keyword)

(def declared-name-type ::declared-name)
(def protocol-name-type ::protocol-name)
(def datatype-name-type ::datatype-name)

(t/ann temp-binding Keyword)
(def temp-binding ::temp-binding)

(t/tc-ignore
(doseq [k [declared-name-type protocol-name-type datatype-name-type]]
  (derive k temp-binding))
  )

(t/ann TYPE-NAME-ENV (t/Atom1 NameEnv))
(defonce TYPE-NAME-ENV (atom {}))
(t/tc-ignore
(set-validator! TYPE-NAME-ENV #(and (every? (every-pred (some-fn namespace 
                                                                 (fn [k] (some (fn [a] (= \. a)) (str k))))
                                                        symbol?) 
                                            (keys %))
                                    (every? (some-fn r/Type? (fn [a] (isa? a temp-binding)))
                                            (vals %))))
  )

(t/ann update-name-env! [NameEnv -> nil])
(defn update-name-env! [nme-env]
  (swap! TYPE-NAME-ENV (fn> [n :- NameEnv] 
                         (merge n nme-env)))
  nil)

(t/ann reset-name-env! [NameEnv -> nil])
(defn reset-name-env! [nme-env]
  (reset! TYPE-NAME-ENV nme-env)
  nil)

(t/ann ^:nocheck get-type-name [Symbol -> (U nil Keyword r/TCType)])
(defn get-type-name 
  "Return the name with var symbol sym.
  Returns nil if not found."
  [sym]
  (@TYPE-NAME-ENV sym))

(t/ann ^:nocheck add-type-name [Symbol (U Keyword r/TCType) -> nil])
(defn add-type-name [sym ty]
  (swap! TYPE-NAME-ENV assoc sym (if (r/Type? ty)
                                   (vary-meta ty assoc :from-name sym)
                                   ty))
  nil)

(t/ann ^:nocheck declare-name* [Symbol -> nil])
(defn declare-name* [sym]
  {:pre [(symbol? sym)
         (namespace sym)]}
  (add-type-name sym declared-name-type)
  nil)

(t/ann ^:nocheck declare-protocol* [Symbol -> nil])
(defn declare-protocol* [sym]
  {:pre [(symbol? sym)
         (some #(= \. %) (str sym))]}
  (add-type-name sym protocol-name-type)
  nil)

(t/ann ^:nocheck declare-datatype* [Symbol -> nil])
(defn declare-datatype* [sym]
  (add-type-name sym datatype-name-type)
  nil)

(t/ann ^:nocheck resolve-name* [Symbol -> r/TCType])
(defn resolve-name* [sym]
  (let [t (@TYPE-NAME-ENV sym)]
    (cond
      (= protocol-name-type t) (prenv/resolve-protocol sym)
      (= datatype-name-type t) (dtenv/resolve-datatype sym)
      (= declared-name-type t) (throw (IllegalArgumentException. (str "Reference to declared but undefined name " sym)))
      (r/Type? t) (vary-meta t assoc :source-Name sym)
      :else (throw (IllegalArgumentException. (u/error-msg "Cannot resolve name " sym
                                                           (when t
                                                             (str " (Resolved to instance of)" (class t)))))))))
