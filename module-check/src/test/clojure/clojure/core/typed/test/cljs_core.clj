(ns clojure.core.typed.test.cljs-core
	(:require [cljs.core.typed :as t]
              [cljs.core :as core]
              [clojure.core.typed.test.cljs-utils :refer [is-tc-e]]
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






(deftest cljs-core-fns-refined-test
	(run-core-tests))