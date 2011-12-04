(ns typed-clojure.core
  (:require [cljs.compiler :as cljs])
  (:require [trammel.core :as c]))

;; Primitives

(defprotocol IType
  (type-fields [this])
  (type-predicate [this]))

(defn type? [t]
  (isa? (class t) Type))

(def Type ::Type)

(defmacro def-type-constructor [name & body]
  `(do
     (c/defconstrainedtype ~name
                           ~@body)
     (derive ~name Type))) ;; TODO Should this be the global hierarchy?

;; Union Type

(def-type-constructor 
  Union 
  [elems]
  [(every? type? elems)])

(defn Un [& types]
  (->Union types))

;; Numeric Types

(def-type-constructor 
  Base 
  [name pred]
  [(symbol? name)])

(def Zero (->Base 'Zero #(= 0 %1)))
(def One  (->Base 'One #(= 1 %1)))
(def PositiveInteger (->Base 'PositiveInteger (comp pos? integer?)))
(def NegativeInteger (->Base 'NegativeInteger (comp neg? integer?)))

(def -Integer (Un NegativeInteger Zero PositiveInteger))

;; Functions

(def-type-constructor 
  Function 
  [dom rng]
  [(every? type? dom)
   (type? rng)])

;; Type ann database

(def type-anns (atom {}))

(defn add-type-ann! [id type]
  (swap! type-anns assoc id type))

(defn get-type-ann [id]
  (if-let [t (@type-anns id)]
    t
    (throw (Exception. (str "No type annotation for " id)))))

;; Primitives

(defn register-def-type-ann [def-form]
  (let [[_ name type] def-form]
    (add-type-ann! `~name type)))

;; Languages as Libraries, Sam TH
(defn type-check 
  "Type check a form"
  [form]
  (let [anal (cljs/analyze {} form)]
    (case (:op anal)
      :var (get-type-ann `~form)
      :def (register-def-type-ann))))

(defmacro def-typed [name type & body]
  (let [form `(def ~name ~@body)]
    (type-check form)
    `~form))

(defmacro defn-typed [name args-dom rng & body]
  (let [args (-> (map first args-dom) vec)
        dom (map (comp deref resolve second) args-dom)
        rng (-> rng resolve deref)
        type (->Function dom rng)]
    `(def-typed ~name ~type
       (fn-typed ~args-dom
         ~@body))))

;; TODO how do fn's store type information? in args?
(defmacro fn-typed [name ann-formals & body]
  (let [args (-> (map first ann-formals) vec)
        dom (map (comp deref resolve second) ann-formals)
        args (map #(with-meta %1 {::type %2}) args dom)
        type (->Function dom (type-check `(do ~@body)))
        form `(fn ~name ~args 
                ~@body)]
    (type-check form)
    `~form))

(fn-typed [[a -Integer]]
  a)

;; Examples

(defn-typed asdf [[a -Integer]] -Integer
           1)

(def-typed a -Integer
           1)
