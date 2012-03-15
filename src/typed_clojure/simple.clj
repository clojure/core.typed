(ns typed-clojure.simple
  (:use [typed-clojure.checker :only [deftypeT +T new-type union fun arity]]))

(+T return-number (fun (arity [Long] Long)))
(defn return-number [a]
  a)

