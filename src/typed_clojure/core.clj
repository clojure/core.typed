(ns typed-clojure.core
  (:require [cljs.compiler :as cljs])
  (:require [trammel.core :as c]))

;; Primitives

(defprotocol IType
  (type-fields [this])
  (type-predicate [this]))

(defn type? [t]
  (contains? (ancestors (class t)) Type))

(def Type ::Type)

(defmacro def-type [name & body]
  `(do
     (c/defconstrainedtype ~name
                           ~@body)
     (derive ~name Type)))

;; Union Type

(def-type Union 
          [elems]
          [(every? type? elems)])

(defn Un [& types]
  (->Union types))

;; Numeric Types

(def-type Base 
          [name pred]
          [(symbol? name)])

(def Zero (->Base 'Zero #(= 0 %1)))
(def One  (->Base 'One #(= 1 %1)))
(def PositiveInteger (->Base 'PositiveInteger (comp pos? integer?)))
(def NegativeInteger (->Base 'NegativeInteger (comp neg? integer?)))

(def -Integer (Un NegativeInteger Zero PositiveInteger))

;; Functions

(def-type Function 
          [dom rng]
          [(every? type? dom)
           (type? rng)])

;; Primitives

(defmacro def-typed [name type & body]
  (let [name ^{::type `~type} name]
    `(def ~name
       ~@body)))

(defmacro defn-typed [name args-dom rng & body]
  (let [args (-> (map first args-dom) vec)
        dom (map (comp deref resolve second) args-dom)
        rng (-> rng resolve deref)
        type (->Function dom rng)
        name ^{::type type} name]
    `(defn ~name ~args
       ~@body)))

;; Examples

(defn-typed asdf [[a -Integer]] -Integer
           1)

(def-typed a -Integer
           1)
