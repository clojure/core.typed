(ns clojure.core.typed.test.mm
  (:require [clojure.core.typed :as t]))

(t/defalias Expr
  (t/U '{:op ':test1
         :a Number
         :b Number}
       '{:op ':test2}))

(t/ann single-dispatch [Expr -> t/Any])

; Expected type for :op
; -> (All [x] [t/Any -> x :object {:id 0 :path [(Key :op)]}
(defmulti single-dispatch :op)

; check a has been refined correctly
(defmethod single-dispatch :test1
  [a]
  (t/ann-form a '{:op ':test1}))

(t/ann multi-dipatch [Expr Expr -> t/Any])
(defmulti multi-dipatch (fn [a b]
                          [(:op a) (:op b)]))

(defmethod multi-dipatch [:test1 :test2]
  [a b]
  (t/ann-form a '{:op ':test1})
  (t/ann-form b '{:op ':test2}))

(defmethod multi-dipatch :default
  [a b]
  ; this shouldn't emit a warning about ClassPE
  'foo)

(t/ann-datatype FooDT [])
(deftype FooDT [])

(t/ann-record FooRec [a :- Number])
(defrecord FooRec [a])

(t/ann multi-dipatch2 [t/Any -> t/Any])
(defmulti multi-dipatch2 (fn [a]
                           [(+ 2 3 4 2) (class a)]))

(defmethod multi-dipatch2 [1 Number]
  [a]
  (t/ann-form a Number)
  (+ 1 a))

(defmethod multi-dipatch2 [1 FooDT] [a] (t/ann-form a FooDT))
(defmethod multi-dipatch2 [1 FooRec] [a] (t/ann-form a FooRec))
(defmethod multi-dipatch2 [::anyfoo FooRec] [a] (t/ann-form a FooRec))

(defmethod multi-dipatch2 [::anyfoo FooRec] 
  [{:keys [a]}]
  (t/ann-form a Number))

(defmethod multi-dipatch2 [::anyfoo FooRec] 
  [arg]
  (t/ann-form arg FooRec)
  (let [{:keys [a]} arg]
    (t/ann-form a Number)))
