(ns typed.base
  (:import (clojure.lang Symbol Namespace IPersistentMap Var Keyword Namespace))
  (:require [typed.core :refer [+T Any IParseType]]))

(+T clojure.core/in-ns [Symbol -> Namespace])
(+T clojure.core/resolve 
    (Fun [Symbol -> (U nil Var Class)]
         [IPersistentMap Symbol -> (U nil Var Class)]))
(+T clojure.core/str [& Any * -> String])
(+T clojure.core/refer [Symbol & Any * -> nil])
(+T clojure.core/require [& Any * -> nil])
(+T clojure.core/symbol (Fun [(U Symbol String) -> Symbol]
                             [String String -> Symbol]))
(+T clojure.core/name [(U String Symbol Keyword) -> String])
(+T clojure.core/ns-name [Namespace -> String])
(+T clojure.core/*ns* Namespace)
(+T clojure.core/+ [& Number * -> Number])
