(ns typed-clojure.types
  (:require [trammel.core :as c]))

;; Primitives

(def Type ::Type)

(defprotocol IType
  (type-fields [this])
  (type-predicate [this])
  (subtype [this super]))

(defn type? [t]
  (isa? (class t) Type))

(defmacro def-type-constructor [name fields contracts & body]
  `(do
     (c/defconstrainedtype ~name
                           ~fields
                           ~contracts

                           java.lang.Comparable
                           (compareTo [this# that#]
                                      (cond
                                        (= this# that#) 0
                                        (subtype this# that#) -1
                                        :else 1))
                           ~@body)
     (derive ~name Type))) ;; TODO Should this be the global hierarchy?

;; Union Type

(declare union?)

(def-type-constructor 
  Union 
  [elems]
  [(every? type? elems)]

  Object
  (toString [_] (str "(Un " (apply str (interpose " " elems)) ")"))
  (equals [_ that] (and (union? that)
                        (= (.elems ^Union that) elems)))
  
  IType
  (subtype [this super]
            (if (union? super)
              (every? true? (map #(subtype %1 super) elems))
              false)))

(defn union? [a]
  (instance? Union a))

(defn Un [& types]
  (->Union types)) ;; TODO sort types uniformly

;; Numeric Types

(def-type-constructor 
  Base 
  [name pred]
  [(symbol? name)]
  
  Object
  (toString [_] (str name))
  (equals [this that] (identical? this that))
  
  IType
  (subtype [this super]
            (cond 
              (= this super) true
              (union? super) (boolean (some true? (map #(subtype this %1) (.elems ^Union super))))
              :else false)))

(def Zero (->Base 'Zero #(= 0 %1)))
(def PositiveInteger (->Base 'PositiveInteger (comp pos? integer?)))
(def NegativeInteger (->Base 'NegativeInteger (comp neg? integer?)))

(def IntegerT (Un NegativeInteger Zero PositiveInteger))
(def FloatT (->Base 'FloatT float?))

;; TODO subtyping for TopT
(def TopT (->Base 'TopT (constantly true)))

(def NumberT (Un IntegerT FloatT))

;; Functions

(declare arity?)

;; Arity is not a type
(c/defconstrainedtype 
  Arity 
  [dom rng]
  [(every? type? dom)
   (type? rng)]
  
  Object
  (toString [_] (apply str (concat (apply str (interpose " " dom)) (str " -> " rng))))
  (equals [_ that] (and (arity? that)
                        (= (.dom that) dom)
                        (= (.rng that) rng)))

  ;; TODO copied from def-type, should refactor out
  java.lang.Comparable
  (compareTo [this that]
             (cond
               (= this that) 0
               (subtype this that) -1
               :else 1))

  IType
  (subtype [this supertype]
           (if (arity? supertype)
             ;; One function type is a subtype of another if they have the same number of arguments, 
             ;; the subtype’s arguments are more permissive (is a supertype), and the subtype’s 
             ;; result type is less permissive (is a subtype). 
             ;; For example, (Any -> String) is a subtype of (Number -> (U String #f)).
             (or (= this supertype)
                 (let [sub-dom dom
                       sub-rng rng
                       sup-dom (.dom ^Arity supertype)
                       sup-rng (.rng ^Arity supertype)]
                   (and (= (count sub-dom)
                           (count sup-dom))
                        (= sub-rng
                           sup-rng)
                        (every? true? (map subtype sup-dom sub-dom))
                        (subtype sub-rng sup-rng))))
             false)))

(defn arity? [a]
  (instance? Arity a))

(declare function?)

(def-type-constructor 
  Function 
  [arrs]
  [(every? arity? arrs)]
  
  Object
  (toString [_] (str "(Fn " (apply str (map #(str "(" %1 ")") arrs)) ")"))
  (equals [_ that] (and (function? that)
                        (= (.arrs that) arrs)))
  
  IType
  (subtype [this supertype]
           (if (function? supertype)
             ;; One function type is a subtype of another if they have the same number of arguments, 
             ;; the subtype’s arguments are more permissive (is a supertype), and the subtype’s 
             ;; result type is less permissive (is a subtype). 
             ;; For example, (Any -> String) is a subtype of (Number -> (U String #f)).
             (let [sub-arrs (sort arrs)
                   sup-arrs (sort (.arrs ^Function supertype))]
               (and (= (count sub-arrs)
                       (count sup-arrs))
                    (every? true? (map subtype sub-arrs sup-arrs))))
             false)))

(defn function? [a]
  (instance? Function a))

;; Utilities

(defn emit-type [t]
  (cond
    (type? t) t
    (symbol? t) (-> t resolve deref)
    (= 'Un (first t)) (apply Un (map emit-type (rest t)))   ;; Union
    (= 'Fn (first t)) (->Function (map emit-type (rest t))) ;; Function
    (= '-> (-> t butlast last)) (->Arity (map emit-type (-> t butlast butlast)) (emit-type (last t)))   ;; Arity
    :else (assert (symbol? t) (str "Invalid type syntax " t))))
