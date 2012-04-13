(ns typed-clojure.test.attempt2
  (:import (clojure.lang Keyword))
  (:use [typed-clojure.attempt2])
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
  (is (sub? (U) (U)))
  (is (sub? (U) (U Object nil)))
  (is (sub? (U Long) (U Long)))
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
  (is (sub? [Number & Object -> Boolean] 
            [Number & Number -> Boolean]))
  (is (sub? [Object & Number -> Boolean]
            [Number & Number -> Boolean]))
  (is (sub? [Number & Number -> Boolean]
            [Number & Number -> Boolean]))
  (is (sub? [Number & Number -> Boolean]
            [Number & Number -> Object]))
  (is (sub? [Number & Number -> Number]
            [Number & Number -> Number]))
  (is (sub? [Number Number & Boolean -> Number]
            [Number Number -> Number]))
  (is (not
        (sub? [Number & Number -> Boolean]
              [Number Number Number -> Number])))
  (is (sub? [Number Number & Boolean -> Number]
            [Number Number Boolean Boolean -> Number]))
  )
