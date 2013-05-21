(ns clojure.core.typed.test.filter-combine
  (:require [clojure.core.typed :refer [ann-form check-ns print-filterset fn>]])
  (:import (clojure.lang Symbol)))

; macroexpansion of `or` is understood
(fn [a]
  (when (or (string? a)
            (symbol? a))
    (ann-form a (U Symbol String))))

(fn [a]
  {:pre [(or (string? a)
             (symbol? a))]}
  (ann-form a (U Symbol String)))

;TODO
(comment
(fn> [a :- (U nil '{:d Number})]
  {:pre [(:d a)]}
  (ann-form (:d a) Number))
  )
