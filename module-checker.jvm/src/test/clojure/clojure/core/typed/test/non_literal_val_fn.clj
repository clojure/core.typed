(ns clojure.core.typed.test.non-literal-val-fn
  (:require [clojure.core.typed :as t]))

(t/ann kw-invoke [t/Kw -> (t/U nil t/Num)])
(defn kw-invoke [k]
  (k {:a 1 :b 2}))

(t/ann sym-invoke [t/Sym -> (t/U nil t/Num)])
(defn sym-invoke [k]
  (k {:a 1 :b 2}))

(t/ann either-invoke [(t/U t/Kw t/Sym) -> (t/U nil t/Num)])
(defn either-invoke [k]
  (k {:a 1 :b 2}))
