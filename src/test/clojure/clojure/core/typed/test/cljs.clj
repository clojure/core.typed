(ns clojure.core.typed.test.cljs
  (:require [clojure.core.typed.test.cljs-utils :refer :all]
            [clojure.test :refer :all]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.subtype :as sub]
            [cljs.core.typed :as t]
            [clojure.core.typed.util-cljs :as ucljs]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.parse-unparse :as prs]))

(deftest parse-prims-cljs-test
  (is-cljs (= (prs/parse-cljs 'number)
              (r/NumberCLJS-maker)))
  (is-cljs (= (prs/parse-cljs 'int)
              (r/IntegerCLJS-maker)))
  (is-cljs (= (prs/parse-cljs 'boolean)
              (r/BooleanCLJS-maker)))
  (is-cljs (= (prs/parse-cljs 'object)
              (r/ObjectCLJS-maker)))
  (is-cljs (= (prs/parse-cljs 'string)
              (r/StringCLJS-maker))))

(deftest parse-array-cljs-test
  (is-cljs (= (prs/parse-cljs '(Array number))
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
  (is-cf (cljs.core.typed/ann foo number)))

(deftest check-ns-test
  (is-cljs (t/check-ns* 'cljs.core.typed.test.ann)))

(deftest resolve-type-test
  (is-cljs 
    (= (:name (ucljs/resolve-var 'cljs.user 'cljs.core/IMap))
       'cljs.core/IMap)))

(deftest parse-protocol-test 
  (is-cljs (prs/parse-cljs '(cljs.core/IMap number number))))

(deftest Protocol-of-test
  (is-cljs (c/Protocol-of 'cljs.core/IMap [(r/NumberCLJS-maker)
                                           (r/NumberCLJS-maker)])))

(deftest heterogeneous-ds-test
  (is-cf [1 2]
         '[number number])
  (is-cf [1 2]
         (cljs.core/IVector number))
  (is-cf {:a 1}
         '{:a number})
  (is-cf {1 1}
         (cljs.core/IMap number number))
  (is-cf #{1}
         (cljs.core/ISet number))
  (is-cf (let [a 1] #{1 a})
         (cljs.core/ISet number)))

(deftest js*-test
  (is-cf (+ 1 1)))

(deftest fn-test
  (is-cf (fn a [b] a))
  (is-cf (fn [a] a)
         (All [x] [x -> x])))

(deftest inst-test
  (is-cf (let [f (-> (fn [a] a)
                     (cljs.core.typed/ann-form (All [x] [x -> x])))]
           ((cljs.core.typed/inst f number) 1))))

(deftest letfn-test
  (is-cf (cljs.core.typed/letfn> [a :- (All [x] [x -> x])
                                  (a [b] b)]
           (a 1))))

(deftest async-test
  (is-cljs (t/check-ns* 'cljs.core.typed.async)))

(deftest inline-annotation-test
  ; code from David Nolen's blog
  (is-cf 
    (defn ^{:ann '[(U nil (cljs.core/ISeqable Any)) Any -> int]}
      index-of [xs x]
      (let [len (count xs)]
        (cljs.core.typed/loop> 
          [i :- int, 0]
          (if (< i len)
            (if (= (nth xs i) x)
              i
              (recur (inc i)))
            -1))))))

#_(clojure.core.typed.analyze-cljs/ast-for-form '(fn [x] (instance? Atom x)))

(deftest simple-polymorphic-test
  (is-cljs (t/check-ns* 'cljs.core.typed.test.identity)))

(deftest value-supertype-test
  (is-cf 'a cljs.core/Symbol)
  (is-cf :a cljs.core/Keyword)
  (is-cf 1 int)
  (is-cf 1.1 number)
  (is-cf 1 number)
  (is-cf true boolean))

;(t/check-ns* 'cljs.core.typed.test.dnolen.utils.dom)
;(t/check-ns* 'cljs.core.typed.test.dnolen.utils.reactive)
;(t/check-ns* 'cljs.core.typed.test.dnolen.utils.helpers)
;(t/check-ns* 'cljs.core.typed.async)
