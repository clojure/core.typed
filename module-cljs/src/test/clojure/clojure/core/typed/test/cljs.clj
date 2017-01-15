(ns clojure.core.typed.test.cljs
  (:require [clojure.core.typed.test.cljs-utils :refer :all]
            [clojure.test :refer :all]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.subtype :as sub]
            [cljs.core.typed :as t]
            [clojure.core.typed.util-cljs :as ucljs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.parse-unparse :as prs]

            [clojure.core.typed.base-env-common :refer [delay-and-cache-env]
             :as common]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.test.cljs-core :as core-test]
            [clojure.core.typed.analyzer-api-intercept :as fake-ana-api]))

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

(deftest throw-test
  (is-tc-e (throw (JSError. "foo"))
           Nothing))

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
             ((t/inst f t/Number) 1)
          t/Number)))

(deftest letfn-test
  (is-tc-e (t/letfn> [a :- (t/All [x] [x -> x])
                      (a [b] b)]
                     (a 1))))

;;commenting-out this test because just :require -ing cljs.core.async fails with internal error
#_(deftest async-test
  (is-cljs (t/check-ns* 'cljs.core.typed.async)))

(deftest inline-annotation-test
  ; code from David Nolen's blog
  ;FIXME
  #_(is-tc-e
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
  (is-tc-e true boolean)
  (is-tc-e "a" string))

(deftest ns-deps-test
  (is (t/check-ns* 'cljs.core.typed.test.dep-one))
  (is (t/check-ns* 'cljs.core.typed.test.dep-two)))

(deftest hvec-infer
  (is-tc-e (fn [a]
             (a [1 2]))
           [[(cljs.core/IVector t/Any) -> t/Any]
            -> t/Any])
  (is-tc-e (fn [a]
             (a [1 2]))
           [(t/All [x] [(cljs.core/IVector x) -> x])
            -> t/Any]))

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

(deftest ctyp-255-cljs-test
  (testing "unparsing protocols is fully qualified in :unknown"
    (is-cljs (= (prs/unparse-type (c/Protocol-of 'cljs.core/ISet [r/-any]))
                '(cljs.core/ISet clojure.core.typed/Any)))))


(def nodes #{:binding :case :case-node :case-test :case-then :const :def :defrecord :deftype :do :fn :fn-method :host-call :host-field :if :invoke :js :js-array :js-object :js-var :let :letfn :local :loop :map :new :no-op :ns :ns* :quote :recur :set :set! :the-var :throw :try :var :vector :with-meta
            })

(deftest check-case-coverage-test
  (fake-ana-api/reset-found)

  ;;let
  (is-tc-e (let [x 0
                   y x]
             y)
           t/Num)

  ;;case
  (is-tc-e (fn [x] (case x
                    0 "zero"
                    "non-zero"))
           [number -> string])

  ;;def
  (tc-e (def x 1))

  ;;fn
  (tc-e (fn [x] x))
  
  
  ;;const
  (is-tc-e 1 number)

  ;;if
  (tc-e (if 1 1 0))

  ;;letfn
  (tc-e (t/letfn> [foo :- [t/Num -> t/Num]
                 (foo [x] x)]
                  (foo 2)))

  ;;loop
  (is-tc-e (t/loop [a :- t/Num 1
               b :- (t/U nil t/Num) nil]
             (if b (str a)
                 (recur 1 1)))
           t/Str)

  ;;map
  (tc-e {:name "Bob" :job "unemployed"})

  ;;set
  (tc-e #{1 2 3})

  ;;quote
  (tc-e '(1 2 3))

  

  (print "MISSING NODES (fake ERROR): ")
  (doseq [op (sort (clojure.set/difference nodes @fake-ana-api/ops-found))]
    (print (str op " ")))
  (println))

(deftest HSequential-parse-test
  (is-tc-e [] (t/HSequential [t/Any *]))
  (is-tc-e '() (t/HSeq [t/Any *]))
  (is-tc-e #{:kw} (t/HSet [:kw]))
  ;; FIXME Value/Val parsing
  #_(is-tc-e {:a 1} (t/HMap :mandatory {:a (t/Value 1)}))
  #_(is-tc-e {:a 1} (t/HMap :mandatory {:a (t/Val 1)}))
  (is-tc-e {:a 1} (t/Rec [x] (t/U nil (t/HMap :mandatory {:a t/Num} :optional {:b x}))))
  )

