(ns clojure.core.typed.test.tools-analyzer
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.checker.jvm.analyze-clj :as ana]
            #_[clojure.core.typed.checker.utils :as u]
            #_[clojure.core.typed.current-impl :as impl]))

#_(comment
(defprotocol D)

(->
  (ana/ast-for-form
    '(letfn [(a [c] b)
             (b [c] a)]))
  :bindings
  first
  :init
  :methods
  first
  :body
  :ret
  :name
  )

(impl/with-clojure-impl
(u/emit-form-fn
(ana/ast-for-form
  '(fn a [b])
  )))
  )

;; records
(deftype FooDT3 [])
(assert (cast FooDT3 (->FooDT3)))
;(assert (instance? FooDT (->FooDT)))
;(.normal ^FooDT (->FooDT 2 1))
