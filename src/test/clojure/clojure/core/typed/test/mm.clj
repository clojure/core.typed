(ns clojure.core.typed.test.mm
  (:import (clojure.lang IPersistentMap))
  (:require [clojure.core.typed :refer [def-alias ann check-ns print-env cf ann-form]]
            [clojure.tools.analyzer :refer [ast]]
            [clojure.repl :refer [pst]]))

(def-alias Expr
  (U '{:op ':test1
       :a Number
       :b Number}
     '{:op ':test2}))

(ann single-dispatch [Expr -> Any])

; Expected type for :op
; -> (All [x] [Any -> x :object {:id 0 :path [(Key :op)]}
(defmulti single-dispatch :op)

; check a has been refined correctly
(defmethod single-dispatch :test1
  [a]
  (ann-form a '{:op ':test1}))

(ann multi-dipatch [Expr Expr -> Any])
(defmulti multi-dipatch (fn [a b]
                          [(:op a) (:op b)]))

(defmethod multi-dipatch [:test1 :test2]
  [a b]
  (ann-form a '{:op ':test1})
  (ann-form b '{:op ':test2}))
