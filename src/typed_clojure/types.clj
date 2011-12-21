(ns typed-clojure.types
  (:require [trammel.core :as c]))

;; Filter rep

(c/defconstrainedtype 
  PathElem
  [selector]
  [(fn? selector)])

(def path-elem? (partial instance? PathElem))

(def Filter ::Filter)

(defmacro def-filter [name fields contracts & body]
  `(do
     (c/defconstrainedtype ~name
                           ~fields
                           ~contracts

                           ~@body)
     (derive ~name Filter)))

(defn filter? [t]
  (isa? (class t) Filter))

(declare type?)

(def-filter
  Top
  []
  [])

(def top? (partial instance? Top))

(def-filter
  Bot
  []
  [])

(def bot? (partial instance? Bot))

(def-filter 
  TypeFilter 
  [ty path vname]
  [(type? ty)
   (every? path-elem? path)
   (symbol? vname)])

(def-filter
  NotTypeFilter
  [ty path vname]
  [(type? ty)
   (every? path-elem? path)
   (symbol? vname)])

(def-filter
  ImpFilter
  [a c]
  [(filter? a)
   (filter? c)])

(def-filter
  AndFilter
  [fs]
  [(and (seq fs)
        (every? filter? fs))])

(def-filter
  OrFilter
  [fs]
  [(and (seq fs)
        (every? filter? fs))])

(def-filter
  FilterSet
  [thn els]
  [(filter? thn)
   (filter? els)])

(c/defconstrainedfn 
  -imp 
  [p1 p2]
  [(filter? p1) (filter? p2) => filter?]
  (cond
    (bot? p1) Top
    (top? p1) p2
    :else (->ImpFilter p1 p2)))

;; TODO study original -or imp
(c/defconstrainedfn
  -or
  [& fs]
  [(every? filter? fs) => filter?]
  (cond
    (empty? fs) Bot
    (= 1 (count fs)) (first fs)
    :else (->OrFilter fs)))

;; TODO study original -and imp
(c/defconstrainedfn
  -and
  [& fs]
  [(every? filter? fs) => filter?]
  (cond
    (empty? fs) Bot
    (= 1 (count fs)) (first fs)
    :else (->AndFilter fs)))

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

(defn +union [& types]
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

(def +zero (->Base 'Zero #(= 0 %1)))
(def +positive-integer (->Base 'PositiveInteger (comp pos? integer?)))
(def +negative-integer (->Base 'NegativeInteger (comp neg? integer?)))

(def +integer (+union +negative-integer +zero +positive-integer))
(def +float (->Base 'FloatT float?))

;; TODO subtyping for TopT
(def +top (->Base 'TopT (constantly true)))

(def +number (+union +integer +float))

;; Functions

(declare arity?)

(defn space-sep [xs]
  (apply str (interpose " " xs)))

;; Arity is not a type
(c/defconstrainedtype 
  Arity 
  [dom rng]
  [(every? type? dom)
   (type? rng)]
  
  Object
  (toString [_] (apply str (concat (space-sep dom) (str " -> " rng))))
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

(defmacro +Fun [& arities]
  `(->Function ~(->
                  (map (fn [t] 
                         `(->Arity ~(-> t butlast butlast vec) ~(last t)))
                       arities)
                  vec)))
