(ns typed-clojure.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global type environment

(defprotocol ITypeEnv
  (extend-env! [this name type])
  (get-env [this]))

(deftype TypeEnv [env]
  ITypeEnv
  (get-env [this] env)
  (extend-env! [this name type]
    (swap! env assoc name type)))

(def the-env (TypeEnv. (atom {})))

(defn extend-env [name type]
  (extend-env! the-env name type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type utilities

(defn make-union [& types]
  (vec types))

(defmacro defnt [name & body]
  (extend-env name (-> (meta name) :-))
  `(defn ~name ~@body))

(defn type-declaration [name type]
  (extend-env name type))

(defmacro make-type [name]
  `(do
     (deftype ~name [])
     (new ~name)))

(defmacro make-type-alias [name type]
  `(make-type ~name))

(deftype Function [sig])
(defn make-Function [& types]
  (Function. types))

(deftype List [type])
(defn make-Listof [sig]
  (List. sig))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive types

;; TODO Work out proposition representation

(make-type Any)
(make-type None)

(type-declaration + (make-Function Number Number Number))
(type-declaration - (make-Function Number Number Number))

;(make-pred-ty symbol? Symbol)

(defnt ^{:- (make-Function Number Number)}
  f
  [x]
  (if (number? x)
    (+ x)
    (- x)))
