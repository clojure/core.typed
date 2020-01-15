(ns clojure.core.typed.test.succeed.CTYP-42-record-extend-protocol
  (:require [clojure.core.typed :as t]
            [clojure.repl :as repl]))

(t/defprotocol PMaths
  (mult-by-two [this] :- PMaths))

(t/ann-record SpecialNumber [x :- Number])
(defrecord SpecialNumber [x]
  PMaths
  (mult-by-two [this] (assoc this :x (* (:x this) 2))))
