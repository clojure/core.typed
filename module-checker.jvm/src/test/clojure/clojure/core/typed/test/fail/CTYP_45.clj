(ns clojure.core.typed.test.fail.CTYP-45
  (:require [clojure.core.typed :as t]))

; using defprotocol shouldn't throw an internal error

(t/ann-protocol MyProto
                foo
                [MyProto -> nil])
(defprotocol MyProto
  (foo [my-proto]))
