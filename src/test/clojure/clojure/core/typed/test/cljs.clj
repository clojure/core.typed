(ns clojure.core.typed.test.cljs
  (:require [clojure.test :refer :all]
            [cljs.core.typed :as t]
            [clojure.core.typed.type-ctors :as c]
            [cljs.analyzer]
            [cljs.compiler :as comp]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.util-cljs :as ucljs])
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

(deftest subtype-prims-cljs-test
  (is-cljs (sub/subtype? (r/-val 1) (prs/parse-cljs 'number))))

(deftest ann-test
  (is (t/cf (cljs.core.typed/ann foo number))))

(deftest check-ns-test
  (is (t/check-ns 'cljs.core.typed.test.ann)))

(deftest resolve-type-test
  (is (= (:name (comp/with-core-cljs
                  (ucljs/resolve-var 'cljs.user 'cljs.core/IMap)))
          'cljs.core/IMap)))

(deftest parse-protocol-test 
  (is (prs/parse-cljs '(cljs.core/IMap number number))))

(deftest Protocol-of-test
  (is-cljs (c/Protocol-of 'cljs.core/IMap [(r/NumberCLJS-maker)
                                           (r/NumberCLJS-maker)])))

(deftest heterogeneous-ds-test
  (is (t/cf [1 2]
            '[number number]))
  (is (t/cf [1 2]
            (cljs.core/IVector number)))
  (is (t/cf {:a 1}
            '{:a number}))
  (is (t/cf {1 1}
            (cljs.core/IMap number number)))
  (is (t/cf #{1}
            (cljs.core/ISet number)))
  (is (t/cf (let [a 1] #{1 a})
            (cljs.core/ISet number))))

(deftest js*-test
  (is (t/cf (+ 1 1))))

(deftest fn-test
  (is (t/cf (fn a [b] a)))
  (is (t/cf (fn [a] a)
            (All [x] [x -> x]))))

(deftest inst-test
  (is (t/cf (let [f (-> (fn [a] a)
                        (cljs.core.typed/ann-form (All [x] [x -> x])))]
              ((cljs.core.typed/inst f number) 1)))))

(deftest letfn-test
  (is (t/cf (cljs.core.typed/letfn> [a :- (All [x] [x -> x])
                                     (a [b] b)]
              (a 1)))))

(deftest async-test
  (is (t/check-ns 'cljs.core.typed.async)))

(deftest inline-annotation-test
  ; code from David Nolen's blog
  (is (t/cf 
        (defn ^{:ann '[(U nil (cljs.core/ISeqable Any)) Any -> int]}
          index-of [xs x]
          (let [len (count xs)]
            (cljs.core.typed/loop> [i :- int, 0]
              (if (< i len)
                (if (= (nth xs i) x)
                  i
                  (recur (inc i)))
                -1)))))))

;(t/check-ns 'cljs.core.typed.test.dom)
