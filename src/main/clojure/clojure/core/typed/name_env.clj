(ns clojure.core.typed.name-env
  (:require [clojure.core.typed
             [type-rep :as r]
             [utils :as u]
             [datatype-env :as dtenv]
             [protocol-env :as prenv]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Name Env

(def declared-name-type ::declared-name)
(def protocol-name-type ::protocol-name)
(def datatype-name-type ::datatype-name)

(def temp-binding ::temp-binding)

(doseq [k [declared-name-type protocol-name-type datatype-name-type]]
  (derive k temp-binding))

(def TYPE-NAME-ENV (atom {}))
(set-validator! TYPE-NAME-ENV #(and (every? (every-pred (some-fn namespace 
                                                                 (fn [k] (some (fn [a] (= \. a)) (str k))))
                                                        symbol?) 
                                            (keys %))
                                    (every? (some-fn r/Type? (fn [a] (isa? a temp-binding)))
                                            (vals %))))

(defn update-name-env! [nme-env]
  (swap! TYPE-NAME-ENV merge nme-env)
  nil)

(defn reset-name-env! [nme-env]
  (reset! TYPE-NAME-ENV nme-env)
  nil)

(defn add-type-name [sym ty]
  (swap! TYPE-NAME-ENV assoc sym (if (r/Type? ty)
                                   (vary-meta ty assoc :from-name sym)
                                   ty))
  nil)

(defn declare-name* [sym]
  {:pre [(symbol? sym)
         (namespace sym)]}
  (add-type-name sym declared-name-type)
  nil)

(defn declare-protocol* [sym]
  {:pre [(symbol? sym)
         (some #(= \. %) (str sym))]}
  (add-type-name sym protocol-name-type)
  nil)

(defn declare-datatype* [sym]
  (add-type-name sym datatype-name-type)
  nil)

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
