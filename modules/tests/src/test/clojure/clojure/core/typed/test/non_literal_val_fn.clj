(ns clojure.core.typed.test.non-literal-val-fn
  (:require [clojure.core.typed :as t])
  (:import (clojure.lang Keyword Symbol)))

(t/ann kw-invoke [Keyword -> (U nil Number)])
(defn kw-invoke [k]
  (k {:a 1 :b 2}))

(t/ann sym-invoke [Symbol -> (U nil Number)])
(defn sym-invoke [k]
  (k {:a 1 :b 2}))

(t/ann either-invoke [(U Keyword Symbol) -> (U nil Number)])
(defn either-invoke [k]
  (k {:a 1 :b 2}))
