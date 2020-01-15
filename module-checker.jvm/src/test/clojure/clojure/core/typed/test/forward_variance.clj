(ns clojure.core.typed.test.forward-variance
  (:require [clojure.core.typed :as t]))

; # Problem
;
; 

; TODO what happens if (def Foo) occurs in this namespaces?
;(t/declare-datatypes 
;  [Foo :variances [:covariant :invariant]]
;  Bar)
;
;(t/declare-aliases
;  [Foo :variances [:covariant :invariant]
;   :rank [* * -> *]])
;

;(binding [t/*collect-on-eval* false]
;(t/ann-datatype [[x :variance :covariant
;                  :< (Foo Any Any)]]
;                Bar)
;
;(t/ann-datatype Foo)
;)
