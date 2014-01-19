(ns clojure.core.typed.test.mm
  (:import (clojure.lang IPersistentMap))
  (:require [clojure.core.typed :as t :refer [def-alias ann check-ns print-env cf ann-form]]
            [clojure.jvm.tools.analyzer :refer [ast]]
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

(defmethod multi-dipatch :default
  [a b]
  ; this shouldn't emit a warning about ClassPE
  'foo)

(t/ann-datatype FooDT [])
(deftype FooDT [])

(t/ann-record FooRec [a :- Number])
(defrecord FooRec [a])

(ann multi-dipatch2 [Any -> Any])
(defmulti multi-dipatch2 (fn [a]
                           [(+ 2 3 4 2) (class a)]))

(defmethod multi-dipatch2 [1 Number]
  [a]
  (ann-form a Number)
  (+ 1 a))

(defmethod multi-dipatch2 [1 FooDT] [a] (ann-form a FooDT))
(defmethod multi-dipatch2 [1 FooRec] [a] (ann-form a FooRec))
(defmethod multi-dipatch2 [::anyfoo FooRec] [a] (ann-form a FooRec))

(defmethod multi-dipatch2 [::anyfoo FooRec] 
  [{:keys [a]}]
  (ann-form a Number))

(defmethod multi-dipatch2 [::anyfoo FooRec] 
  [arg]
  (ann-form arg FooRec)
  (let [{:keys [a]} arg]
    (ann-form a Number)))
