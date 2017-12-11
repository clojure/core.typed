(ns clojure.core.typed.test.analyzer2-jvm
  (:require [clojure.test :refer :all]
            [clojure.core.typed.analyzer2.jvm :as ana]
            [clojure.tools.analyzer.jvm :as taj]
            [clojure.tools.analyzer.jvm.utils :as ju]))

(defmacro ast' [form]
  `(ana/analyze '~form))

(defmacro ast [form]
  `(ana/analyze+eval '~form))

(deftest analyzer-test
  (is (= 1
         (:result (ast 1))))
  (is (= 2 
         (:result (ast (-> 1 inc)))))
  (is (= 1
         (:result (ast (let [a 1] a)))))
  (is (= 1
         (:result (ast (loop [a 1] a)))))
  (is (= 1
         (:result (ast (do (def a 1)
                           a)))))
  (is (= 1
         (:result (ast (do (deftype Abc [a])
                           (.a (->Abc 1)))))))
  (is (= true
         (:result (ast (do (ns foo) (= 1 1))))))
  (is (= "a"
         (:result (ast (.toString (reify Object (toString [this] "a")))))))
  (is (= 2 (:result (ast (#(inc %) 1)))))
  (is (->
        (ast (do (ns bar
                   (:require [clojure.core.typed :as t]))
                 (t/ann-form 'foo 'a)))
        :ret))
  (is (not= :maybe-class (:op (ast Number))))
  )

(deftest async-test
  (is (-> (ast (do (ns asdfasdf
                     (:require [clojure.core.async :as a]
                               [clojure.core.typed.async :refer [go chan]]))
                   #(go)))
          :result)))
