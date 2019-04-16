(ns clojure.core.typed.analyzer.jvm-test
  (:require [clojure.test :refer :all]
            [clojure.tools.analyzer.passes.jvm.emit-form :refer [emit-form]]
            [clojure.tools.analyzer.jvm.utils :as ju]
            [clojure.tools.analyzer.jvm :as taj]
            [clojure.tools.analyzer.ast :as ast]
            [clojure.core.typed.analyzer.jvm :as ana]))

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
  #_
  (is (->
        (ast (do (ns bar
                   (:require [clojure.core.typed :as t]))
                 (t/ann-form 'foo 'a)))
        :ret))
  (is (= [:const Number]
         ((juxt :op :val) (ast Number))))
  (is (= [:const clojure.lang.Compiler]
         ((juxt :op :val) (ast clojure.lang.Compiler))))
  (is (= [:static-field 'LOADER]
         ((juxt :op :field) (ast clojure.lang.Compiler/LOADER))))
  )

(deftest local-tag-test
  (is (= java.lang.String
         (:tag (ast "asdf"))))
  (is (= [:const java.lang.String]
         (-> (ast (let [a "asdf"]))
             :bindings
             first
             :init
             ((juxt :op :tag)))))
  (is (= [:binding java.lang.String]
         (-> (ast (let [a "asdf"]))
             :bindings
             first
             ((juxt :op :tag)))))
  (is (= [:local java.lang.String]
         (-> (ast (let [a "asdf"]
                    a))
             :body
             :ret
             ((juxt :op :tag)))))
  (is (= java.lang.String
         (:tag (ast (let [a "asdf"]
                      a)))))
  )

#_
(deftest async-test
  (is (-> (ast (do (ns asdfasdf
                     (:require [clojure.core.async :as a]
                               [clojure.core.typed.async :refer [go chan]]))
                   #(go)))
          :result)))

(deftest deftype-test
  (is (some?
        (binding [*ns* *ns*]
          (eval `(ns ~(gensym)))
          (ast
            (deftype A []
              Object
              (toString [_] (A.) "a")))))))

(deftest uniquify-test
  (let [ret (ast' (let [a 1]
                    (let [a 2]
                      a)))]
    (is (= (let [sym (-> ret :body :ret :bindings first :name)]
             (is (symbol? sym))
             sym)
           (-> ret :body :ret :body :ret :name)))
    (is (not= 'a (-> ret :body :ret :body :ret :name)))))
