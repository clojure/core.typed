(ns clojure.core.typed.test.filter-combine
  (:require [clojure.core.typed :as t :refer [ann-form check-ns print-filterset]]))

; macroexpansion of `or` is understood
(fn [a]
  (when (or (string? a)
            (symbol? a))
    (ann-form a (t/U t/Sym String))))

;exceptional control flow
(fn [a]
  {:pre [(or (string? a)
             (symbol? a))]}
  (ann-form a (t/U t/Sym String)))

;TODO
(comment
(t/fn [a :- (U nil '{:d Number})]
  {:pre [(:d a)]}
  (ann-form (:d a) Number))
  )
