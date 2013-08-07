(ns clojure.core.typed.test.cljs
  (:require [clojure.test :refer :all]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.parse-unparse :as prs])
  (:import (clojure.lang ISeq ASeq IPersistentVector Atom IPersistentMap
                         Keyword ExceptionInfo Symbol Var)))

(defmacro cljs [& body]
  `(impl/with-cljs-impl
     ~@body))

(defmacro is-cljs [& body]
  `(is (cljs ~@body)))

(deftest parse-prims-cljs-test
  (is (= (prs/parse-cljs 'number)
         (r/NumberCLJS-maker)))
  (is (= (prs/parse-cljs 'int)
         (r/IntegerCLJS-maker)))
  (is (= (prs/parse-cljs 'boolean)
         (r/BooleanCLJS-maker)))
  (is (= (prs/parse-cljs 'object)
         (r/ObjectCLJS-maker)))
  (is (= (prs/parse-cljs 'string)
         (r/StringCLJS-maker))))

(deftest parse-array-cljs-test
  (is (= (prs/parse-cljs '(Array number))
         (r/ArrayCLJS-maker (prs/parse-cljs 'number)
                            (prs/parse-cljs 'number)))))

(deftest unparse-prims-cljs-test
  (is-cljs (= 'number
              (prs/unparse-type (prs/parse-cljs 'number))))
  (is-cljs (= 'boolean
              (prs/unparse-type (prs/parse-cljs 'boolean))))
  (is-cljs (= 'int
              (prs/unparse-type (prs/parse-cljs 'int))))
  (is-cljs (= '(Array number)
              (prs/unparse-type (prs/parse-cljs '(Array number)))))
  (is-cljs (= '(Array2 number boolean)
              (prs/unparse-type (prs/parse-cljs '(Array2 number boolean))))))
