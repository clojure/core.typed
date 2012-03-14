(ns typed-clojure.simple
  (:use [typed-clojure.checker :only [deftypeT +T new-type union fun arity]]))

(+T return-number (fun (arity [Number] Long)))
(defn return-number [a]
  a)

