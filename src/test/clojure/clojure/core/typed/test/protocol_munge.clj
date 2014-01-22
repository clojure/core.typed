(ns clojure.core.typed.test.protocol-munge
  (:require [clojure.core.typed :as t]))

(t/ann-protocol Foo
                my_dash [Foo -> Number])
(t/defprotocol> Foo
  (my_dash [this]))

(t/ann-protocol Bar
                my_dash_interface [Bar -> t/Symbol])
(definterface Bar
  (my_dash_interface []))

(t/ann-datatype FooT [])
(deftype FooT []
  Foo
  (my_dash [this] 1)
  Bar
  (my_dash_interface [this] 'a))
