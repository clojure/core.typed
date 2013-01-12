(ns typed.test.mm
  (:import (clojure.lang IPersistentMap))
  (:require [typed.core :refer [def-alias ann check-ns print-env cf]]
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

;(isa? :test1 (:op 0th))
(defmethod MapToString :test1
  [a]
  (print-env "mm")
  (str a))
