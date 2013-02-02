(ns typed.test.mm
  (:import (clojure.lang IPersistentMap))
  (:require [typed.core :refer [def-alias ann check-ns print-env cf]]
            [analyze.core :refer [ast]]
            [clojure.repl :refer [pst]]))

(def-alias Expr
  (U '{:op ':test1
       :a Number
       :b Number}
     '{:op ':test2}))

(ann MapToString [Expr -> String])

; Expected type for :op
; -> (All [x] [Any -> x :object {:id 0 :path [(Key :op)]}
; Dispatch 
(defmulti MapToString :op)

;(isa? (:op 0th) :test1)
(defmethod MapToString :test1
  [a]
  (print-env "mm")
  )
