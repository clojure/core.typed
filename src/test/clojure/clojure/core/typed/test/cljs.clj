(ns clojure.core.typed.test.cljs
  (:require [clojure.core.typed.test.cljs-utils :refer :all]
            [clojure.test :refer :all]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.subtype :as sub]
            [cljs.core.typed :as t]
            [clojure.core.typed.util-cljs :as ucljs]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.parse-unparse :as prs]

            [clojure.core.typed.base-env-common :refer [delay-and-cache-env]
             :as common]
            [clojure.core.typed.var-env :as var-env]))

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
  (is-tc-e (t/ann foo number)))

(deftest check-ns-test
  (is-cljs (t/check-ns* 'cljs.core.typed.test.ann)))

(deftest parse-protocol-test
  (is-cljs (prs/parse-cljs '(cljs.core/IMap number number))))

(deftest Protocol-of-test
  (is-cljs (c/Protocol-of 'cljs.core/IMap [(r/NumberCLJS-maker)
                                           (r/NumberCLJS-maker)])))

(deftest heterogeneous-ds-test
  (is-tc-e [1 2]
           '[number number])
  (is-tc-e [1 2]
           (IVector number))
  (is-tc-e {:a 1}
           '{:a number})
  (is-tc-e {1 1}
           (IMap number number))
  (is-tc-e #{1}
           (ISet number))
  (is-tc-e (let [a 1] #{1 a})
           (ISet number)))

(deftest js*-test
  (is-tc-e (+ 1 1)))

(deftest fn-test
  (is-tc-e (fn a [b] a))
  (is-tc-e (fn [a] a)
           (t/All [x] [x -> x])))

(deftest inst-test
  (is-tc-e (let [f (-> (fn [a] a)
                       (t/ann-form (t/All [x] [x -> x])))]
             ((t/inst f number) 1))))

(deftest letfn-test
  (is-tc-e (t/letfn> [a :- (t/All [x] [x -> x])
                      (a [b] b)]
                     (a 1))))

#_(deftest async-test
    (is-cljs (t/check-ns* 'cljs.core.typed.async)))

(deftest inline-annotation-test
                                        ; code from David Nolen's blog
  (is-tc-e
   (defn ^{:ann '[(t/U nil (ISeqable t/Any)) t/Any -> int]}
     index-of [xs x]
     (let [len (count xs)]
       (t/loop>
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
  (is-tc-e 'a Symbol)
  (is-tc-e :a Keyword)
  (is-tc-e 1 int)
  (is-tc-e 1.1 number)
  (is-tc-e 1 number)
  (is-tc-e true boolean))

(deftest ns-deps-test
  (is (t/check-ns* 'cljs.core.typed.test.dep-one))
  (is (t/check-ns* 'cljs.core.typed.test.dep-two)))

(deftest hvec-infer
  (is-tc-e (fn [a]
             (a [1 2]))
           [[(cljs.core/IVector Any) -> Any]
            -> Any])
  (is-tc-e (fn [a]
             (a [1 2]))
           [(t/All [x] [(cljs.core/IVector x) -> x])
            -> Any]))

(deftest seq-test
  (is-tc-e [1 2 3] (t/Coll int))
  (is-tc-e [1 2 3] (t/Seqable int))  ;;not sure if it should be...
  (is-tc-e (seq [1 2 3]) (t/NonEmptyASeq int)))

                                        ;(t/check-ns* 'cljs.core.typed.test.dnolen.utils.dom)
                                        ;(t/check-ns* 'cljs.core.typed.test.dnolen.utils.reactive)
                                        ;(t/check-ns* 'cljs.core.typed.test.dnolen.utils.helpers)
                                        ;(t/check-ns* 'cljs.core.typed.async)


(deftest core-fns-test
  (t/check-ns* 'cljs.core.typed.test.ympbyc.test-base-env))


(deftest annotation-coverage
  (let [n-core-vars    (count (ns-map 'cljs.core))
        n-common-anns  (count common/common-var-annotations)
        n-cljs-anns    (count @var-env/CLJS-VAR-ANNOTATIONS)]
    (or (= n-core-vars n-cljs-anns)
        (do
          (println (str "number of vars in cljs.core:       " n-core-vars))
          (println (str "number of vars in base-env-common: " n-common-anns))
          (println (str "number of specific cljs vars:      " (- n-cljs-anns
                                                                 n-common-anns)))
          (println (str "Coverage: "
                        (* 100 (float (/ n-cljs-anns  n-core-vars)))
                        "% ("
                        (- n-core-vars n-cljs-anns)
                        " vars are missing its annotations)"))
          (println (clojure.set/difference (set (map name (keys (ns-map 'cljs.core))))
                                           (set (map name (keys @var-env/CLJS-VAR-ANNOTATIONS)))))
          false))))
