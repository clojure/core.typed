(ns typed.base
  (:import (clojure.lang Symbol Namespace IPersistentMap Var Keyword Namespace ISeq Seqable
                         Atom IRef IObj))
  (:require [typed.core :refer [+T Any IParseType Nothing]]))

(+T typed.core/add-ns-dep [Symbol Symbol -> nil])
(+T typed.core/*add-type-ann-fn* [Symbol IParseType -> nil])
(+T typed.core/check-namespace [Symbol -> nil])

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
(+T clojure.core/prn [& Any * -> nil])
(+T clojure.core/first [Seqable -> Any])
(+T clojure.core/rest [Seqable -> ISeq])
(+T clojure.core/next [Seqable -> ISeq])
(+T clojure.core/every? [[Nothing -> Any] Seqable -> Boolean])
(+T clojure.core/symbol? [Any -> Boolean])
(+T clojure.core/keys [(Mapof [Any Any]) -> ISeq])
(+T clojure.core/vals [(Mapof [Any Any]) -> ISeq])
(+T clojure.core/set? [Any -> Boolean])
(+T clojure.core/atom [Any & Any * -> Atom])
(+T clojure.core/set-validator! [IRef (U nil [Nothing -> Any]) -> nil])
(+T clojure.core/partial [[Nothing & Nothing * -> Any] -> [& Nothing * -> Any]])
(+T clojure.core/with-meta [IObj (Mapof [Any Any])-> IObj])
