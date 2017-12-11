(ns clojure.core.typed.test.analyzer2-jvm
  (:require [clojure.test :refer :all]
            [clojure.core.typed.analyzer2.jvm :as ana]
            [clojure.tools.analyzer.jvm :as taj]))

(defmacro ast [form]
  `(ana/analyze+eval '~form))

(deftest analyzer-test
	(is (ast 1))
	(is (ast (-> 1 +)))
	(is (ast (let [a 1] a)))
	(is (ast (loop [a 1] a)))
	(is (ast (do (def a 1)
							 a)))
	(is (ast (do (deftype Abc [])
							 (->Abc))))
	(is (ast (do (ns foo) isa?)))
	(is (ast (reify Object (toString [this] "a"))))
	(is (->
        (ast (do (ns bar
                   (:require [clojure.core.typed :as t]))
                 (t/ann-form 'foo 'a)))
        :ret))
	)
