(ns clojure.core.typed.test.CTYP-144
  (:require [clojure.core.typed.test.test-utils :refer :all]
            [clojure.core.typed.checker.jvm.analyze-clj :as ana]
            [clojure.test :refer :all]))

(deftest instance-method-test
  (is-tc-e
    (do (defprotocol IRandom
          (-next-int
            [this limit :- Integer] :- Integer))

        (ann-record SeededRandom [seed :- Int
                                  rng :- java.util.Random])

        (defrecord SeededRandom [seed ^java.util.Random rng]
          IRandom
          (-next-int 
            [this limit] 
            (ann-form limit Integer))))))

#_(defprotocol IRandom
  (-next-int [this limit]))

#_(-> 
  (ana/ast-for-form
    '(defrecord SeededRandom [seed ^java.util.Random rng]
       IRandom
       (-next-int [this limit])))
  :body
  :statements
  (nth 2)
  :methods
  (nth 29)
  :methods
  first
  keys
  )
