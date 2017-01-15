(ns clojure.core.typed.test.cljs-core
    (:require [cljs.core.typed :as t]
              [cljs.core :as core]
              [clojure.core.typed.test.cljs-utils :refer [is-tc-e tc-e]]
              [clojure.core.typed.profiling :as profile]
              [clojure.test :refer :all]))

;;; defining tests this way should help estimate the coverage
;;; cljs.core vars not included in (keys @core-tests) are totally untested

(def core-tests (atom {}))

(defmacro add-test [f & pairs]
  (assert (even? (count pairs)))
  `(swap! core-tests conj 
      ['~(symbol "cljs.core" (name f)) '~pairs]))

;;tests for single parameter functions
(defmacro add-test1 [f & pairs]
  (assert (even? (count pairs)))
  `(swap! core-tests conj
      [(with-meta '~(symbol "cljs.core" (name f)) 
        {:single-arg true})
       '~pairs]))

(defmacro run-core-tests []
  (cons 'do
    (for [[f cases] @core-tests
          [args expected] (partition 2 cases)]
      (if (:single-arg (meta f))
          `(is-tc-e (~f ~args) ~expected)
          `(is-tc-e (~f ~@args) ~expected)))))


;;; core vars test

(add-test1 seq
    [1 2 3] (t/NonEmptySeqable number)
    []      (t/Option (t/NonEmptyASeq t/Any)))

(add-test1 first 
    [8]           number
    (seq [1 2 3]) number
    nil           nil)

(add-test1 rest
    [1 2 3]       (core/ASeq number)
    (seq [1 2 3]) (core/ASeq number)
    []            (core/ASeq t/Any))

(add-test1 last
    [1 2 3]       number
    (seq [1 2 3]) number
    []            (t/Option t/Any))

(add-test1 butlast
    [1 2 3] (core/ASeq number)
    (seq [1 2 3]) (core/ASeq number)
    []  (core/ASeq t/Any))

(add-test1 second
    [1 2 3] number
    []      nil
    nil     nil
    (seq [1 2 3]) (t/Option number))


(add-test1 clj->js
    {:a 1} t/Any)

(add-test1 nil?
    nil   boolean
    "nil" boolean)

(add-test1 ifn?
    (fn [x] x) boolean
    "foo"      boolean)

(add-test1 identity
    8       number
    "hello" string
    [0]     (t/Vec number))

(add-test take
    [2 [1 2 3 4]] (core/ASeq int))

(add-test drop
    [2 [1 2 3 4]] (core/ASeq int))

(add-test1 number?
    8     boolean
    [1 2] boolean)

(add-test1 string?
    "hello" boolean)

(add-test1 seq?
    (seq [1 2 3]) boolean
    [1 2 3]       boolean)

(add-test apply
    [+ [2 3]]               number
    [str ["hello" "world"]] string)

(add-test conj
    [["foo"] "bar"] (core/IVector string)
    [(seq [3 4 5]) 1 2] (core/ASeq number)
    [(seq [[1] [2 3]]) [8] [9]] (core/ASeq (t/Vec number))
    ;[{:foo 5} [:bar 8]] (t/Map core/Keyword number)
    ;[{:foo "bar"} {:baz 1 2 3}] (t/Map core/Keyword t/Any)
    )

(add-test get
    [#{1 2 3} 3] (t/Option number)
    [{:a true :b false} :c false] boolean)

(add-test assoc
    [["foo" "bar"] 2 "baz"] (t/Vec string)
    [{[2 3] "foo"} [4 5] "bar"] (t/Map (t/Vec number) string))

(add-test dissoc
    [{:foo 8 :bar 9} :foo] (t/Map core/Keyword number))

(add-test1 fn?
    (fn [x y] y)  boolean
    cljs.core/map boolean)

(add-test1 peek
    [{:foo "bar" :baz "zot"} {:foo "bar"}] (t/Map core/Keyword string))

(add-test1 pop
    [1 2 3] (t/Vec number))

(add-test disj
    [#{1 2 3 4} 3 4] (t/Set number))

(add-test1 empty?
    []  boolean
    #{} boolean
    [1] boolean
    {:a 1} boolean)

(add-test1 coll?
    {:a 1} boolean
    #{1 2} boolean
    [2 3]  boolean
    "foo"  boolean
    2      boolean)

(add-test1 map?
    {:a 1} boolean
    {}     boolean
    [0 1]  boolean)

(add-test1 vector?
    []     boolean
    [1]    boolean
    {:a 1} boolean
    "foo"  boolean)

(add-test1 false?
    []    boolean
    false boolean
    true  boolean
    1     boolean)

(add-test1 true?
    []    boolean
    false boolean
    true  boolean
    1     boolean)

(add-test1 seq?
    []           boolean
    #{2 3}       boolean
    (seq [1 2])  boolean
    (seq #{2 3}) boolean)

(add-test1 boolean
    []     boolean
    "true" boolean
    :false boolean)

(add-test1 integer?
    2   boolean
    1.3 boolean
    "8" boolean)

(add-test contains?
    [[1 2 3] 2]    boolean
    [#{1 2} 8]     boolean
    [(seq []) "a"] boolean)

(add-test find
    [{:a 1 :b 2} :b] (t/Option (HVec [Keyword number]))
    [[:a :b] 0]      (t/Option (HVec [int Keyword])))

(add-test distinct?
    [:a :b]       boolean
    [[1] [1] [1]] boolean
    [:a "foo" []] boolean)

(add-test compare
    [[1 2] [2 3]] number
    ["foo" "bar"] number
    [2 1]         number)

(add-test sort
    [[2 4 2 1 5 3]] (t/Option (core/ASeq int))
    [[:a :c :b]]    (t/Option (core/ASeq Keyword))
    [(t/fn [x :- int y :- int] :- int (- x y))
     [6 1 7 3 2]]   (t/Option (core/ASeq int)))

(add-test1 shuffle
    [1 2 3 4] (t/Vec int)
    #{4 8 2}  (t/Vec int))

;FIXME reenable after porting to tools.analyzer.js. Some issue with hygienic renaming
#_(add-test reduce
    [(t/fn [x :- int y :- string] :- int 0)
     ["foo" "bar" "baz"]] int
    [(t/fn [x :- int y :- int] :- (core/Reduced int) (reduced 0))
     #{8 36 2}] int
    [(t/fn [x :- int y :- int] :- (core/Reduced int) (reduced 0))
     #{8 36 2} 0] int)

(add-test reduce-kv
    [(t/fn [a :- int k :- Keyword v :- int] :- int (+ a v))
     0 {:a 1 :b 2 :c 3}] int)

(add-test <
    [1 5]     boolean
    [1.8 0.8] boolean
    [1/2 3/2] boolean)

(add-test1 int
    1      int)

(add-test mod
    [8 2] t/AnyInteger
    [8.1 2.1] number)

(add-test rand
    []  number
    [8] number)

(add-test1 rand-int
    1    int
    1000 int)

(add-test nthnext
    [nil 8] nil
    [#{2 3 1} 1] (t/Option (t/NonEmptyASeq int))
    [[1 2 3]  9] (t/Option (t/NonEmptyASeq int)))

(comment add-test1 rseq
    [1 2 3] (t/Option (t/NonEmptyASeq int)))

(add-test1 reverse
    [1 2 3] (core/ASeq int)
    {:a 1 :b 2} (core/ASeq t/Any)
    #{1 2 3}  (core/ASeq int))

(comment add-test list
    [1 2 3] (t/PersistentList int)
    [:a 2]  (t/PersistentList t/Any))

(add-test cons
    [1 [2 3]]   (core/ASeq int)
    [[1] [[2]]] (core/ASeq (t/Vec int)))







(deftest cljs-core-fns-refined-test
    (run-core-tests))
