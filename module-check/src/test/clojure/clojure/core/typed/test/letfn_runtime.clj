(ns clojure.core.typed.test.letfn-runtime
  (:use clojure.test)
  (:require [clojure.core.typed :as t]))

#_(deftest letfn-syntax-test
  (is (=
       (letfn [(a [b] b)]
         (a 1))
       (t/letfn
         [(a [b] b)]
         (a 1))
       (t/letfn :- Any
         [(a [b] b)]
         (a 1))
       (t/letfn :- Any
         [(a [b :- Bar] :- Any b)]
         (a 1))
       (t/letfn
         [(a [b :- Bar] b)]
         (a 1))
       (t/letfn
         [(a ([b :- Bar] :- Any b))]
         (a 1))
       (t/letfn
         [(a ([b :- Bar] :- Any b))]
         (a 1))
       (t/letfn
         [(a ([b :- Bar] b))]
         (a 1))
       (t/letfn
         [(a [b :- Bar] b)]
         (a 1))
       (t/letfn
         [a :- [Any -> Bar]
          (a [b] b)]
         (a 1))
       (t/letfn
         [a :- [Any -> Bar]
          (a [b :- Bar] :- Any b)]
         (a 1))
       )))

#_(deftest def-syntax-test
  (is (= @(def a 1)
         @(t/def a 1)
         @(t/def a "docstring" 1)
         @(t/def a "docstring" :- Foo 1)
         @(t/def :- Foo a 1))))

#_(deftest defn-syntax-test
  ; this unit test is useless atm
  (is (= (defn a [b] b)
         (t/defn a [b] b)
         (t/defn a "docstring" [b] b)
         (t/defn a [b :- Any] b)
         (t/defn a [b :- Any] :- Any b)
         (t/defn a "docstring" [b :- Any] :- Any b)
         (t/defn a "docstring" [b :- Any] :- Any {:pre []} b)
         (t/defn a "docstring" {:attr-map nil} [b :- Any] :- Any {:pre []} b)
         (t/defn a ([b :- Any] b))
         (t/defn a ([b :- Any] :- Any b))
         (t/defn a "docstring" ([b :- Any] :- Any b))
         (t/defn a "docstring" {:attr-map nil} ([b :- Any] :- Any b)))))

#_(deftest do-syntax-test
  (is (= (do 1 2)
         (t/do 1 2)
         (t/do :- Any 1 2))))

#_(deftest let-syntax-test
  (is (= (let [a 1] a))))
