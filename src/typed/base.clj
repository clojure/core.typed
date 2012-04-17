(ns typed.base
  (:import (clojure.lang Symbol Namespace IPersistentMap Var))
  (:require [typed.core :refer [+T]]))

(+T clojure.core/in-ns [Symbol -> Namespace])
(+T clojure.core/resolve 
    (Fun [Symbol -> (U nil Var Class)]
         [IPersistentMap Symbol -> (U nil Var Class)]))
