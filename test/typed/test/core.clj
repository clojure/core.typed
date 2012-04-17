(ns typed.test.core
  (:import (clojure.lang Keyword IPersistentVector Sequential IPersistentList Var Ratio
                         Symbol IPersistentMap))
  (:require [typed.core :refer :all]
            [analyze.core :refer [ast]]
            [clojure.test :refer :all]))

; add base type anns
(binding [*add-type-ann-fn* (fn [sym type-syn]
                              (add-type-ann sym (parse type-syn)))]
  (require 'typed.base))

(defmacro sub? [s t]
  `(binding [*ns* (find-ns 'typed.test.core)]
     (subtype? (parse '~s)
               (parse '~t))))

(deftest subtype-unit
  (is (sub? Unit Unit)))

(deftest subtype-any
  (is (sub? [1 -> 1] Any))
  (is (sub? Long Any)))

(deftest subtype-object
  (is (sub? [1 -> 1] Object))
  (is (sub? long Object))
  (is (sub? float Object))
  (is (sub? Object Object)))

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
  (is (sub? :a Keyword))
  (is (not (sub? Keyword :a)))
  (is (sub? (U :a :b) Keyword)))

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
            [Long Long -> Long]))
  (is (sub? [Long -> Long]
            [1 -> Any]
            )))

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
        [Long Long Long Long -> Any]
        [Long Long & Long * -> Nothing]))
  (is (sub? 
        [Long Long Long Long -> Any]
        clojure.lang.IFn))
  (is (not (sub? 
             clojure.lang.IFn
             [Long Long Long Long -> Any])))
  )

(deftest subtype-vectors
  (is (sub? (Vectorof Number)
            IPersistentVector))
  (is (sub? (Vectorof Number)
            Sequential))
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
            IPersistentVector))
  )

(deftest subtype-sequentials
  (is (sub? (Sequentialof Double)
            (Sequentialof Number)))
  (is (not (sub? (Sequentialof Number)
                 (Sequentialof Double))))
  (is (sub? (Sequentialof Double)
            Sequential))
  (is (not (sub? (Sequentialof Double)
                 IPersistentVector)))
  (is (not (sub? Sequential
                 (Sequentialof Double))))

  (is (sub? (Vectorof Double)
            (Sequentialof Double)))
  (is (sub? (Vector* Double Double)
            (Sequential* Double Number)))
  )

(deftest subtype-maps
  (is (sub? (Mapof Unit Unit)
            IPersistentMap))
  (is (sub? (Mapof Keyword Double)
            IPersistentMap))
  (is (sub? (Map* Integer Object Double Integer Number Number)
            IPersistentMap))
  (is (sub? (Mapof Integer Double)
            (Mapof Number Number)))
  (is (not (sub? (Mapof Number Number)
                 (Mapof Integer Double))))
  (is (sub? (Map* Integer Double Double Integer Number Number)
            (Mapof Number Number)))
  (is (not (sub? (Map* Integer Object Double Integer Number Number)
                 (Mapof Number Number))))
  (is (sub? (Map*)
            (Mapof Unit Unit)))
         )

(deftest subtype-primitives
  (is (sub? void void))
  (is (sub? nil void))
  (is (sub? void nil))
  (is (sub? int int))
  (is (sub? double double))
  (is (sub? float float))
  (is (sub? boolean boolean))
  (is (sub? long long)))

(deftest subtype-primitive-boxing
  (is (sub? long Long))
  (is (sub? double Double)))

(defmacro subfrm [form expected]
  `(binding [*ns* (find-ns 'typed.test.core)]
     (subtype? (type-key (tc-expr (ast ~form)))
               (parse '~expected))))

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
              Ratio))
  (is (subfrm 'a
              'a))
  (is (subfrm 'a
              Symbol))
  (is (subfrm \c
              \c))
  (is (subfrm \c
              Character))
  (is (subfrm
        {:a :b}
        IPersistentMap))
         )

(deftest tc-expr-keyword
  (is (subfrm :a
              :a))
  (is (subfrm :a
              Keyword)))

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
  `(binding [*ns* (find-ns 'typed.test.core)]
     (tc-expr (ast ~form))))

(defmacro with-env [& body]
  `(binding [*ns* (find-ns 'typed.test.core)]
     (with-type-anns ~@body)))

(deftest tc-expr-def
  (is (subfrm (def a)
              Var))
  (is (with-env
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
  (is (with-env
        {a [Long -> Long]}
        (subfrm
          (do (declare a)
            a)
          [Long -> Long]))))

(deftest tc-expr-invoke
  (is (with-env
        {a [Long -> Long]}
        (subfrm
          (do (declare a)
            (a 1))
          Long)))
  (is (with-env
        {a (Fun [Long -> Long]
                [Long Long -> Double])}
        (subfrm
          (do (declare a)
            (a 1))
          Long)))
  (is (with-env
        {a (Fun [Long -> Long]
                [Long Long -> Double])}
        (subfrm
          (do (declare a)
            (a 1 1))
          Double)))
  (is (with-env
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
  (is (with-env
        {a [Long -> Long]}
        (subfrm 
          (do (declare a)
            (let [x (a 1)
                  y (a x)]
              y))
          Long)))
      )

(deftest tc-static-method
  (is (subfrm
        (+ 1 1.1)
        double))
  (is (subfrm
        (+ 1 1)
        long)))

(deftest tc-static-field
  (is (subfrm
        (. Integer TYPE)
        Class)))

(deftest tc-instance-method
  (is (subfrm
        (.getClass "a")
        (U nil Class))))

;; TODO find instance field to test
(deftest tc-instance-field)

(deftest tc-map
  (is (subfrm
        {(get {} 1) 1}
        (Map* nil 1))))
