(ns typed-clojure.simple
  (:use [typed-clojure.checker :only [deftypeT +T Any]])
  (:import (clojure.lang IPersistentCollection IPersistentMap Atom IPersistentVector
                         IPersistentList)
           (typed_clojure.checker Fun Nothing)))

(+T return-number [Long -> Long])
(defn return-number [a]
  a)

(+T return-string [String -> String])
(defn return-string [a]
  "a")

(+T my-atom Atom)
(def my-atom (atom {}))

(+T my-empty-map IPersistentMap)
(def my-empty-map {})
(+T my-empty-vec IPersistentVector)
(def my-empty-vec [])
(+T my-empty-list IPersistentList)
(def my-empty-list '())

(+T head [(U nil IPersistentCollection) -> Any])
(defn head [c]
  (first c))

(first nil)
(first [1])
(rest nil)
(next nil)

(+T error-first [-> [-> nil]])
(defn error-first []
  (fn [] nil))

(+T tail [IPersistentCollection -> IPersistentCollection])
(defn tail [c]
  (rest c))

(deftypeT MyType
  [[a :- Number]
   [b :- String]])

(+T construct [Number String -> MyType])
(defn construct [n s]
  (MyType. n s))

(+T destruct [IPersistentCollection -> Any])
(defn destruct [[a]]
  a)

(+T destruct2 [IPersistentCollection -> Any])
(defn destruct2 [{:keys [a b c] :as d}]
  a)

(+T method1 [Class -> (U nil String)])
(defn method1 [a]
  (.getName ^Class a))

(+T hash1 [& Any -> Object])
(defn hash1 [& as]
  (apply hash-map as))

(+T let1 [Number -> (U nil Boolean)])
(defn let1 [n]
  (let [b (+ n 1)
        c (= b n)]
    c))

(+T fn1 [Number -> [Number -> Long]])
(defn fn1 [n]
  (fn [^{:+T Number} n] 1))

(+T recur1 [String -> (U Nothing nil)])
(defn recur1 [a]
  (when true
    (recur a)))

(+T recur2 [String -> String])
(defn recur2 [a]
  (if false
    (recur a)
    "a"))

(+T loop1 [Number -> Boolean])
(defn loop1 [n]
  (loop [^{:+T (U Long IPersistentVector)}
         b [1 2 3]]
    (if false
      (recur 1)
      b)))
