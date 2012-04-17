(ns typed-clojure.base
  (:import (clojure.lang Symbol Namespace IPersistentMap Var))
  (:require [typed-clojure.attempt2 :refer [+T]]))

(+T clojure.core/in-ns [Symbol -> Namespace])
(+T clojure.core/resolve 
    (Fun [Symbol -> (U nil Var Class)]
         [IPersistentMap Symbol -> (U nil Var Class)]))
