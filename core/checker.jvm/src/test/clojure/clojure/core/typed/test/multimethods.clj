(ns clojure.core.typed.test.multimethods
  (:require [clojure.core.typed.test.test-utils :refer :all]
            [clojure.core.typed :refer [check-ns]]
            [clojure.test :refer :all]))

(deftest multimethod-test
  (is (check-ns 'clojure.core.typed.test.mm))
  (is-tc-e (do (ann f [Any -> Any])
               (defmulti f class)
               (defmethod f Number [n] (inc n))))
  (is-tc-e (do (ann f [Any -> Any])
               (defmulti f (fn [a] (class a)))
               (defmethod f Number [n] (inc n))))
  (is-tc-e (do (ann f [Any Any -> Any])
               (defmulti f (fn [a b]
                             [(class a) (class b)]))
               (defmethod f [Number Number] [n1 n2] (+ n1 n2)))))
