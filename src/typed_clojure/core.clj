(ns typed-clojure.core
  (:require [cljs.compiler :as cljs])
  (:require [trammel.core :as c]))

;; Primitives

(defprotocol ITypePredicate
  (type-predicate [this]))

(defmacro def-type [name fields-contracts & {:keys [pred]}]
  (let [fields (map first fields-contracts)
        contracts (-> (map second fields-contracts) vec)
        fields-defaults (-> (interleave fields (repeat nil)) vec)]
    `(c/defconstrainedtype  ;; TODO Should be defconstrainedtype, waiting on bugfix from Trammel
       ~name
       ~fields-defaults
       ~contracts
       ITypePredicate
       (type-predicate [this] ~pred))))

;; Union Type

;; TODO
;; rep/type-rep.rkt, line 344
(def-type Union [elems ]
  ITypePredicate
  (type-predicate [this] ;TODO
                  ))

(def empty-union (->Union nil))

(defn Un 
  ([] empty-union)
  ([t] t)
  ([t & types]
   (->Union (set (cons t types)))))

;; Numeric Types

(def-type Zero []
          :pred #(= 0 %1))
(def-type One []
          :pred #(= 1 %1))

(def-type PositiveInteger []
          :pred (comp pos? integer?))

(def-type NegativeInteger []
          :pred (comp neg? integer?))

(def -Integer (Un NegativeInteger Zero PositiveInteger))

;; Primitives

(defmacro def-typed [[name _ type] & body]
  `(def ~(with-meta name {::type type})
     ~@body))

(def-typed (a :- -Integer) 
           1)
