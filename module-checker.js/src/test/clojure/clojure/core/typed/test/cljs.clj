(ns clojure.core.typed.test.cljs
  (:require [clojure.core.typed.test.cljs-utils :refer :all]
            [clojure.test :refer :all]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.jvm.subtype :as sub]
            [cljs.core.typed :as t]
            [clojure.core.typed.util-cljs :as ucljs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]

            [clojure.core.typed.checker.base-env-common :refer [delay-and-cache-env]
             :as common]
            [clojure.core.typed.checker.var-env :as var-env]
            [clojure.core.typed.test.cljs-core :as core-test]
            [clojure.core.typed.analyzer-api-intercept :as fake-ana-api]))

(deftest parse-prims-cljs-test
  (is-cljs (= (prs/parse-cljs 'cljs.core.typed/JSNumber)
              (r/JSNumber-maker)))
  (is-cljs (= (prs/parse-cljs 'cljs.core.typed/CLJSInteger)
              (r/CLJSInteger-maker)))
  (is-cljs (= (prs/parse-cljs 'cljs.core.typed/JSBoolean)
              (r/JSBoolean-maker)))
  (is-cljs (= (prs/parse-cljs 'cljs.core.typed/JSObject)
              (r/JSObject-maker)))
  (is-cljs (= (prs/parse-cljs 'cljs.core.typed/JSString)
              (r/JSString-maker))))

(deftest parse-array-cljs-test
  (is-cljs (= (prs/parse-cljs '(Array cljs.core.typed/JSNumber))
              (r/ArrayCLJS-maker (prs/parse-cljs 'cljs.core.typed/JSNumber)
                                 (prs/parse-cljs 'cljs.core.typed/JSNumber)))))

(deftest unparse-prims-cljs-test
  (is-cljs (= 'cljs.core.typed/JSNumber
              (prs/unparse-type (prs/parse-cljs 'cljs.core.typed/JSNumber))))
  (is-cljs (= 'cljs.core.typed/JSBoolean
              (prs/unparse-type (prs/parse-cljs 'cljs.core.typed/JSBoolean))))
  (is-cljs (= 'cljs.core.typed/CLJSInteger
              (prs/unparse-type (prs/parse-cljs 'cljs.core.typed/CLJSInteger))))
  (is-cljs (= '(Array cljs.core.typed/JSNumber)
              (prs/unparse-type (prs/parse-cljs '(Array cljs.core.typed/JSNumber)))))
  (is-cljs (= '(Array2 cljs.core.typed/JSNumber cljs.core.typed/JSBoolean)
              (prs/unparse-type (prs/parse-cljs '(Array2 cljs.core.typed/JSNumber cljs.core.typed/JSBoolean))))))

(deftest subtype-prims-cljs-test
  (is-cljs (sub/subtype? (r/-val 1) (prs/parse-cljs 'cljs.core.typed/JSNumber))))

;FIXME
#_
(deftest throw-test
  (is-tc-e (throw (js/JSError. "foo"))
           t/Nothing))

(deftest ann-test
  (is-tc-e (do (t/ann foo t/JSNumber)
               (def foo 1)))
  (is-tc-err (do (t/ann foo t/JSNumber)
                 (def foo nil))))

(deftest check-ns-test
  (is-cljs (t/check-ns* 'cljs.core.typed.test.ann)))

(deftest parse-protocol-test 
  (is-cljs (prs/parse-cljs '(cljs.core/IMap cljs.core.typed/JSNumber cljs.core.typed/JSNumber))))

(deftest Protocol-of-test
  (is-cljs (c/Protocol-of 'cljs.core/IMap [(r/JSNumber-maker)
                                           (r/JSNumber-maker)])))

(deftest heterogeneous-ds-test
  (is-tc-e [1 2]
           '[t/JSNumber t/JSNumber])
  (is-tc-e [1 2]
           (IVector t/JSNumber))
  (is-tc-e {:a 1}
           '{:a t/JSNumber})
  (is-tc-e {1 1}
           (IMap t/JSNumber t/JSNumber))
  (is-tc-e #{1}
           (ISet t/JSNumber))
  (is-tc-e (let [a 1] #{1 a})
           (ISet t/JSNumber)))

(deftest js*-test
  (is-tc-e (+ 1 1)))

(deftest fn-test
  (is-tc-e (fn a [b] a))
  (is-tc-e (fn [a] a)
           (t/All [x] [x -> x])))

(deftest inst-test
  (is-tc-e (let [f (-> (fn [a] a)
                       (t/ann-form (t/All [x] [x -> x])))]
             ((t/inst f t/JSNumber) 1))
           t/JSNumber))

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
    (defn ^{:ann '[(t/U nil (ISeqable t/Any)) t/Any -> cljs.core.typed/CLJSInteger]}
      index-of [xs x]
      (let [len (count xs)]
        (t/loop>
         [i :- cljs.core.typed/CLJSInteger, 0]
         (if (< i len)
           (if (= (nth xs i) x)
             i
             (recur (inc i)))
           -1))))))

#_
(deftest simple-polymorphic-test
  (is-cljs (t/check-ns* 'cljs.core.typed.test.identity)))

(deftest value-supertype-test
  (is-tc-e 'a Symbol)
  (is-tc-e :a Keyword)
  (is-tc-e 1 t/CLJSInteger)
  (is-tc-e 1.1 t/JSNumber)
  (is-tc-e 1 t/JSNumber)
  (is-tc-e true t/JSBoolean)
  (is-tc-e "a" t/JSString))

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
  (is-tc-e [1 2 3] (t/Coll cljs.core.typed/CLJSInteger))
  (is-tc-e [1 2 3] (t/Seqable cljs.core.typed/CLJSInteger))  ;;not sure if it should be...
  (is-tc-e (seq [1 2 3]) (t/NonEmptyASeq cljs.core.typed/CLJSInteger)))

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
           t/JSNumber)

  ;;case
  (is-tc-e (fn [x] (case x
                    0 "zero"
                    "non-zero"))
           [t/JSNumber -> cljs.core.typed/JSString])

  ;;def
  (tc-e (def x 1))

  ;;fn
  (tc-e (fn [x] x))
  
  
  ;;const
  (is-tc-e 1 t/JSNumber)

  ;;if
  (tc-e (if 1 1 0))

  ;;letfn
  (tc-e (t/letfn> [foo :- [t/JSNumber -> t/JSNumber]
                 (foo [x] x)]
                  (foo 2)))

  ;;loop
  (is-tc-e (t/loop [a :- t/JSNumber 1
                    b :- (t/U nil t/JSNumber) nil]
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
  (is-tc-e {:a 1}
           (t/Rec [x] (t/U nil (t/HMap :mandatory {:a t/JSNumber} :optional {:b x}))))
  )

(deftest undefined-test
  (is-tc-err nil t/JSUndefined)
  (is-tc-err nil t/JSNull)
  (is (not (sub? nil cljs.core.typed/JSNull)))
  (is (not (sub? nil cljs.core.typed/JSUndefined)))
  (is (sub? cljs.core.typed/JSUndefined nil))
  (is (sub? cljs.core.typed/JSNull nil))
  (is-tc-e (t/fn [a :- t/JSUndefined] :- nil
             a))
  (is-tc-e (t/fn [a :- t/JSNull] :- nil
             a))
  (is-tc-err (t/fn [a :- t/JSNull] :-  t/JSUndefined
               a))
  (is-tc-err (t/fn [a :- t/JSUndefined] :-  t/JSNull
               a))
  (is-tc-err (when (undefined? nil)
               :kw)
             nil)
  (is-tc-e (t/fn [a :- t/JSNull]
             (when (undefined? a)
               :kw))
           [t/JSNull :-> nil])
  (is-tc-e (t/fn [a :- t/JSUndefined]
             (when (undefined? a)
               :kw))
           [t/JSUndefined :-> ':kw])
  (is-tc-e (do
             (t/ann ^:no-check a t/JSUndefined)
             (def a nil)
             a)
           nil)
  (is-tc-e (t/fn [a :- (t/U (cljs.core/IVector t/Any) t/JSUndefined)]
             (if a
               (t/ann-form a (cljs.core/IVector t/Any))
               (t/ann-form a t/JSUndefined))))
)

(deftest ratio-test
  (is-tc-e 1/2 t/JSNumber))

(deftest goog-imports
  (is-cljs (t/check-ns* 'cljs.core.typed.test.goog-import)))

(deftest jsobj-test
  (is-cljs (t/check-ns* 'cljs.core.typed.test.js-obj)))
