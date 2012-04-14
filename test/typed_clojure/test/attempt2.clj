(ns typed-clojure.test.attempt2
  (:import (clojure.lang Keyword IPersistentVector Sequential IPersistentList))
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
  (is (not (sub? (U Object nil) 
                 (U))))
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
  (is (sub? 
        [Long Long Long Long -> (U Object nil)]
        [Long Long & Long * -> (U)]))
  )

(deftest subtype-vectors
  (is (sub? (Vectorof Number)
            clojure.lang.IPersistentVector))
  (is (sub? (Vectorof Number)
            clojure.lang.Sequential))
  (is (sub? (Vectorof Integer)
            (Vectorof Number)))
  (is (not (sub? (Vectorof Number)
                 (Vectorof Integer))))

  (is (sub? (Vector* Integer Float Double)
            (Vector* Number Number Number)))
  (is (not (sub? (Vector* Number Number Number)
                 (Vector* Integer Float Double))))

  (is (sub? (Vector* Integer Float Double)
            (Vectorof Number)))
  (is (sub? (Vector* Integer Float Double)
            clojure.lang.IPersistentVector))
  )

(deftest subtype-sequentials
  (is (sub? (Sequentialof Double)
            (Sequentialof Number)))
  (is (not (sub? (Sequentialof Number)
                 (Sequentialof Double))))
  (is (sub? (Sequentialof Double)
            clojure.lang.Sequential))
  (is (not (sub? (Sequentialof Double)
                 clojure.lang.IPersistentVector)))
  (is (not (sub? clojure.lang.Sequential
                 (Sequentialof Double))))

  (is (sub? (Vectorof Double)
            (Sequentialof Double)))
  (is (sub? (Vector* Double Double)
            (Sequential* Double Number)))
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
        [& Long * -> (Sequentialof Long)])))

(deftest tc-expr-var
  (is (with-type-anns
        {a [Long -> Long]}
        (subfrm
          (do (declare a)
            a)
          [Long -> Long]))))

(deftest tc-expr-invoke
  (is (with-type-anns
        {a [Long -> Long]}
        (subfrm
          (do (declare a)
            (a 1))
          Long)))
  (is (with-type-anns
        {a (Fun [Long -> Long]
                [Long Long -> Double])}
        (subfrm
          (do (declare a)
            (a 1))
          Long)))
  (is (with-type-anns
        {a (Fun [Long -> Long]
                [Long Long -> Double])}
        (subfrm
          (do (declare a)
            (a 1 1))
          Double)))
  (is (with-type-anns
        {a (Fun [Long -> Long]
                [Long Long & Long * -> Double])}
        (subfrm
          (do (declare a)
            (a 1 1 1 1 1 1))
          Double)))
         )

(deftest tc-expr-let
  (is (subfrm 
        (let [x 1]
          x)
        1))
  (is (subfrm 
        (let [x 2
              x 3]
          (let [x 1]
            x))
        1))
  (is (with-type-anns
        {a [Long -> Long]}
        (subfrm 
          (do (declare a)
            (let [x (a 1)
                  y (a x)]
              y))
          Long)))
      )
