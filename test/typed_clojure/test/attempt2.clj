(ns typed-clojure.test.attempt2
  (:import (clojure.lang Keyword))
  (:use [typed-clojure.attempt2]
        [analyze.core :only [ast]])
  (:use [clojure.test]))

(defmacro sub? [s t]
  `(subtype? (parse '~s)
             (parse '~t)))

(deftest subtype-classes
  (is (sub? Long Long))
  (is (sub? Long Object))
  (is (not (sub? Long Integer)))
  (is (not (sub? Long Integer))))

(deftest subtype-singletons
  (is (not (sub? 1 2)))
  (is (sub? 1 1))
  (is (sub? 1 Long))
  (is (not (sub? Long 1)))
  (is (sub? :a :a))
  (is (not (sub? :a :b)))
  (is (sub? :a clojure.lang.Keyword))
  (is (not (sub? clojure.lang.Keyword :a)))
  (is (sub? (U :a :b) clojure.lang.Keyword)))

(deftest subtype-nil
  (is (sub? nil nil))
  (is (sub? (U nil) nil))
  (is (not (sub? nil 1))))

(deftest subtype-unions
  (is (sub? (U) 
            (U)))
  (is (sub? (U) 
            (U Object nil)))
  (is (sub? (U Long) 
            (U Long)))
  (is (sub? (U Long Integer) 
            (U Integer Long)))
  (is (sub? (U (U Class String) Long Integer)
            (U Integer (U String Class) Long)))
  (is (not (sub? (U Object) (U Long))))
  (is (not (sub? Object (U Long))))
  (is (sub? Long (U Object)))
  (is (sub? (U Float Integer Double) Object))
  )

(deftest subtype-funs
  (is (sub? [1 -> 2] 
            [1 -> 2]))
  (is (sub? [Long -> 1] 
            [1 -> Long]))
  (is (sub? [Object Long -> 1] 
            [Long Long -> Long])))

(deftest subtype-varargs
  (is (sub? [Number & Object * -> Boolean] 
            [Number & Number * -> Boolean]))
  (is (sub? [Object & Number * -> Boolean]
            [Number & Number * -> Boolean]))
  (is (sub? [Number & Number * -> Boolean]
            [Number & Number * -> Boolean]))
  (is (sub? [Number & Number * -> Boolean]
            [Number & Number * -> Object]))
  (is (sub? [Number & Number * -> Number]
            [Number & Number * -> Number]))
  (is (sub? [Number Number & Boolean * -> Number]
            [Number Number -> Number]))
  (is (not
        (sub? [Number & Number * -> Boolean]
              [Number Number Number -> Number])))
  (is (sub? [Number Number & Boolean * -> Number]
            [Number Number Boolean Boolean -> Number]))
  )

(defmacro subfrm [form expected]
  `(subtype? (type-key (tc-expr (ast ~form)))
             (parse '~expected)))

(deftest tc-expr-number
  (is (subfrm 1
              1))
  (is (subfrm 1
              Long))
  (is (subfrm 1.1
              1.1))
  (is (subfrm 1.1
              Double)))

(deftest tc-expr-constant
  (is (subfrm 1/2
              1/2))
  (is (subfrm 1/2
              clojure.lang.Ratio))
  (is (subfrm 'a
              'a))
  (is (subfrm 'a
              clojure.lang.Symbol))
  (is (subfrm \c
              \c))
  (is (subfrm \c
              Character))
         )

(deftest tc-expr-keyword
  (is (subfrm :a
              :a))
  (is (subfrm :a
              clojure.lang.Keyword)))

(deftest tc-expr-string
  (is (subfrm "a"
              "a"))
  (is (subfrm "a"
              String)))

(deftest tc-expr-if
  (is (subfrm (if 1 2 3)
              (U 2 3)))
  (is (subfrm (if 1 2 3)
              Long)))

(deftest tc-expr-do
  (is (subfrm (do)
              nil))
  (is (subfrm (do 1)
              1))
  (is (subfrm (do 2 1)
              1)))

(defmacro tc [form]
  `(tc-expr (ast ~form)))

(deftest tc-expr-def
  (is (subfrm (def a)
              clojure.lang.Var))
  (is (with-type-anns
        {b Long}
        (tc 
          (def b 1)))))

(deftest tc-expr-fn-meta-annot
  (is (subfrm 
        (fn [] 1)
        [-> 1]))
  (is (subfrm 
        (fn [^{+T Long} b]
          b)
        [Long -> Long])))

(deftest tc-expr-fn-rest-type
  (is (subfrm
        (fn [& ^{+T Long} a]
          a)
        [& Long * -> (U Object nil)])))
