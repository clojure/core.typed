(ns clojure.core.typed.test.fail.record-poly-no-optional
  (:require [clojure.core.typed :as t]))

; all map->* keys are mandatory in polymorphic records

(t/ann-precord Foo [[foo :variance :invariant]] [b :- (U nil Number)])
; FIXME ann-precord needs to be migrated to ann-record
(t/tc-ignore
(defrecord Foo [b])
  )

(map->Foo {})
