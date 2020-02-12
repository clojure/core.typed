(ns cljs.core.typed.test.ympbyc.test-base-env
  (:require-macros [cljs.core.typed :refer [ann ann-jsnominal] :as ct])
  (:require [cljs.core.typed :refer [All U IFn Option I Any Seqable Vec HSequential NonEmptyASeq NonEmptySeqable Atom1 Set Coll Map] :as t]
            [cljs.core :refer [IVector ISeq ASeq List]]))

;;seq
(ann seq-vec (NonEmptySeqable number))
(def seq-vec (seq [1 2 3]))

(ann seq-empty (Option (NonEmptyASeq nil)))
(def seq-empty (seq []))

;;fst

(ann vec-fst number)
(def vec-fst (first [8]))

(ann seq-fst number)
(def seq-fst (first (seq [1 2 3])))

(ann fst-nil nil)
(def fst-nil (first nil))


;;rest

(ann vec-rest (ASeq number))
(def vec-rest (rest [1 2 3]))

(ann seq-rest (ASeq number))
(def seq-rest (rest (seq [1 2 3])))

(ann rest-empty (ASeq nil))
(def rest-empty (rest []))


;;last

(ann vec-last number)
(def vec-last (last [1 2 3]))

(ann seq-last number)
(def seq-last (last (seq [1 2 3])))

(ann last-nil (Option number))
(def last-nil (last []))


;;butlast

(ann vec-butlast (ASeq number))
(def vec-butlast (butlast [1 2 3]))

(ann seq-butlast (ASeq number))
(def vec-butlast (butlast (seq [1 2 3])))

(ann butlast-empty (ASeq nil))
(def butlast-empty (butlast []))


;;test if NonEmptySeqable is Seqable
(ann nonemp (All [x] [(NonEmptySeqable x) -> number]))
(defn foo [xs] 1)

(foo (seq [1 2 3]))



(ann second-vec number)
(def second-vec (second [1 2 3]))

(ann second-empty nil)
(def second-empty (second []))

(ann second-nil nil)
(def second-nil (second nil))

(ann second-seq (Option number))
(def second-seq (second (seq [1 2 3])))



(ann clj-to-jsjs Any)
(def clj-to-js (clj->js {:a 1}))

;BUG: Use of js-obj triggers "js-op missing" in check_cljs
;(ann js-to-clj Any)
;(def js-to-clj (js->clj (js-obj "a" 1 "b" 2)))

(ann cljs.core/nil? [Any -> boolean])

(ann nil-pred-t boolean)
(def nil-pred-t (nil? nil))

(ann nil-pred-f boolean)
(def nil-pred-f (nil? "nil"))



(ann ifn?-test-t boolean)
(def ifn?-test-t (ifn? (fn [x] x)))

(ann ifn?-test-f boolean)
(def ifn-test-f (ifn? "foo"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test vars in base-env-common


(ann id-test number)
(def id-test (identity 8))

(ann take-vec (ASeq int))
(def take-vec (take 2 [1 2 3 4]))


(ann drop-vec (ASeq int))
(def drop-vec (drop 2 [1 2 3 4]))

(ann get-set (Option int))
(def get-set (get #{1 2 3} 2))

(ann sym-test Symbol)
(def sym-test 'foo)


;BUG: subtyping fails
;(ann atom-test (Atom1 number))
;(def atom-test (atom 3))

(ann set-test (Set number))
(def set-test #{5})

(ann number?-t boolean)
(def number?-t (number? 8))

(ann number?-f boolean)
(def number?-f (number? [1 2]))

(ann string?-t boolean)
(def string?-t (string? "hello"))

(ann seq?-t boolean)
(def seq-t (seq? (seq [1 2 3])))

(ann seq?-f boolean)
(def seq-f (seq? [1 2 3]))

;BUG: Use of `list` invokes an error
;(ann cljs.core/-conj [Any Any -> (Coll Any)])
;(ann cljs.core.List.EMPTY (List Any)) ;;this fails somehow
;(ann list?-test boolean)
;(def list?-test (list? (list 1 2 3)))

(ann apply-test number)
(def apply-test (apply + [2 3]))

(ann apply-test-str string)
(def apply-test-str (apply str ["hello, " "world"]))


(ann conj-1 (IVector string))
(def conj-1 (conj ["foo"] "bar"))

(ann conj-2 (ASeq number))
(def conj-2 (conj (seq [3 4 5]) 1 2))

(ann conj-3 (ASeq (Vec number)))
(def conj-3 (conj (seq [[1] [2 3]]) [8] [9]))

;BUG: this throws assert failed
;(ann conj-4 (t/Map Keyword number))
;(def conj-4 (conj {:foo 5} [:bar 8]))
;(ann conj-5 (t/Map Keyword Any))
;(def conj-5 (conj {:foo "bar"} {:baz 123}))


(ann get-1 (Option number))
(def get-1 (get #{1 2 3} 3))

(ann get-2 boolean)
(def get-2 (get {:a true :b false} :c false))


(ann assoc-vec (Vec string))
(def assoc-vec (assoc ["foo" "bar"] 2 "baz"))

(ann assoc-map (t/Map (Vec number) string))
(def assoc-map (assoc {[2 3] "foo"} [4 5] "bar"))

(ann dissoc-1 (t/Map Keyword number))
(def dissoc-1 (dissoc {:foo 8 :bar 9} :foo))

(ann fn?-1 boolean)
(def fn?-1 (fn? (fn [x y] y)))

(ann fn?-2 boolean)
(def fn?-2 (fn? cljs.core/map))

(ann peek-1 (t/Map Keyword string))
(def peek-1 (peek [{:foo "bar" :baz "zot"} {:foo "bar"}]))


(ann pop-1 (Vec number))
(def pop-1 (pop [1 2 3]))

(ann disj-1 (Set number))
(def disj-1 (disj #{1 2 3 4} 3 4))


;;jsnominals
(ann-jsnominal js/Object
    [[]
     :fields {}
     :methods {keys (Array string)
               toString [-> string]}])

(ann-jsnominal js/Document
    [[]
     :fields {}
     :methods
     {getElementById [string -> (cljs.core.typed/Option js/HTMLElement)]
      querySelector  [string -> (cljs.core.typed/Option js/HTMLElement)]}
     :ancestors #{js/Object}])

;(ann js/document js/Document)

(ann  get-el [string -> (Option js/HTMLElement)])
(defn get-el [sel] (.querySelector js/document sel))

(ann  inner-html [js/HTMLElement -> string])
(defn inner-html [el] (.-innerHTML el))

(ann inner-html-result string)
(def inner-html-result
  (let [el (get-el "body")]
    (if el (inner-html el) "")))

;;inheritance

(ann document-is-object string)
(def document-is-object (.toString js/document))
