(ns typed-clojure.simple
  (:import (clojure.lang IPersistentCollection IPersistentMap)
           (typed_clojure.checker Fun))
  (:use [typed-clojure.checker :only [deftypeT +T new-type union fun arity]]))

(+T return-number (fun (arity [Long] Long)))
(defn return-number [a]
  a)

(+T return-string (fun (arity [String] String)))
(defn return-string [a]
  "a")

(+T my-atom clojure.lang.Atom)
(def my-atom (atom {}))

(+T my-empty-map clojure.lang.IPersistentMap)
(def my-empty-map {})
(+T my-empty-vec clojure.lang.IPersistentVector)
(def my-empty-vec [])
(+T my-empty-list clojure.lang.IPersistentList)
(def my-empty-list '())

(+T head (fun (arity [(union nil clojure.lang.IPersistentCollection)] (union nil Object))))
(defn head [c]
  (first c))

(first nil)
(first [1])
(rest nil)
(next nil)

(+T error-first (fun (arity [] (fun (arity [] nil)))))
(defn error-first []
  (fn []))

(+T tail (fun (arity [IPersistentCollection] IPersistentCollection)))
(defn tail [c]
  (rest c))

(deftypeT MyType
  [[a :- Number]
   [b :- String]])

(+T construct (fun (arity [Number String] MyType)))
(defn construct [n s]
  (MyType. n s))

(+T destruct (fun (arity [IPersistentCollection] (union nil Object))))
(defn destruct [[a]]
  a)

(+T destruct2 (fun (arity [IPersistentCollection] (union nil Object))))
(defn destruct2 [{:keys [a b c] :as d}]
  a)

(+T method1 (fun (arity [Class] (union nil Object))))
(defn method1 [a]
  (.getName ^Class a))

(+T hash1 (fun (arity [:& Object] Object)))
(defn hash1 [& as]
  (apply hash-map as))

(+T let1 (fun (arity [Number] (union nil Boolean))))
(defn let1 [n]
  (let [b (+ n 1)
        c (= b n)]
    c))

(+T fn1 (fun (arity [Number]
                    (fun (arity [Number] Long)))))
(defn fn1 [n]
  (fn [^{:+T Number} n] 1))

;(+T loop1 (fun (arity [Number] Boolean)))
;(defn loop1 [n]
;  (loop [b [1 2 3]]
;    b))
