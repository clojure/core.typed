(ns clojure.core.typed.test.ctyp-294.typed
  {:lang :core.typed
   :core.typed {:experimental #{:infer-vars
                                :infer-locals}}}
  (:require 
    [clojure.core.typed :as t]
    [clojure.core.typed.test.ctyp-294.untyped :as u]))

#(inc (t/ann-form u/bar t/Int))

#(inc (u/foo 1))

;#_#(inc (u/foo (inc 'a)))

(t/ann bar (t/IFn ;[t/Str :-> t/Str]
                  [t/Int :-> t/Int]))
(defn bar [x]
  x)

#(inc (bar (t/ann-form u/foo t/Int)))

(map inc [u/bar])

(fn [a] (inc a))
(filter (fn [a] (inc a)) [1 2 3])
(filter (fn [a]
          {:pre [(integer? a)]}
          (inc a))
        [1 2 3])
(inc @#'u/bar)
