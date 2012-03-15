(ns typed-clojure.simple
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

(+T error-first (fun (arity [] typed_clojure.checker.Fun)))
#_(defn error-first []
  (fn []))

#_(+T tail (fun (arity [clojure.lang.IPersistentCollection] Object)))
#_(defn tail [c]
  (rest c))
