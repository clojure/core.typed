(ns clojure.core.typed.test.protocol-scoping
  (:require [clojure.core.typed :as t]))

(t/ann-protocol NonPoly

                nonpoly
                [NonPoly -> Any])

(t/defprotocol> NonPoly
  (nonpoly [this]))

(t/ann-datatype DNP [])
(deftype DNP []
  NonPoly
  (nonpoly [this] this))

(t/ann-protocol [[foo :variance :covariant]]
                Foo

                bar-
                [(Foo foo) -> foo])

(t/defprotocol> Foo
  (bar- [this]))

(t/ann-datatype FooD [t :- t/Symbol]
                :extends
                [(Foo t/Symbol)])

(deftype FooD [t]
  Foo
  (bar- [this] t))

(t/ann-form (bar- (->FooD 'a))
            t/Symbol)
