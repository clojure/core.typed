(ns clojure.core.typed.test.fail.record-poly-no-optional
  (:require [clojure.core.typed :as t]))

; all map->* keys are mandatory in polymorphic records

(t/ann-record [[foo :variance :invariant]] Foo [b :- (U nil Number)])
(t/tc-ignore
(defrecord Foo [b])
  )

(map->Foo {})
