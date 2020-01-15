(ns clojure.core.typed.test.ring
  (:require [clojure.core.typed :refer [ann check-ns ann-form print-env cf]])
  (:import (clojure.lang IPersistentMap IPersistentVector)))

(comment
(ann assoc-conj
     (All [x] (Fn [(IPersistentMap Any (U x (IPersistentVector x))) Any x -> 
                   (IPersistentMap Any (U x (IPersistentVector x)))]
                  [(IPersistentVector (U x (IPersistentVector x))) Integer x ->
                   (IPersistentVector (U x (IPersistentVector x)))])))
(defn assoc-conj
  "Associate a key with a value in a map. If the key already exists in the map,
  a vector of values is associated with the key."
  [map key val]
  (ann-form map (IPersistentMap Any (U x (IPersistentVector x))))
  (print-env "top")
  (assoc map key
         (if-let [cur (get map key)]
           (do
             (print-env "map")
             (if (vector? cur)
               (do (print-env "cur")
                 (conj cur val))
               [cur val])
             )
           val)))

(check-ns)
  )
