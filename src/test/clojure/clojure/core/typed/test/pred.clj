(ns clojure.core.typed.test.pred
  (:require 
    [clojure.test :refer :all]
    [clojure.core.typed :as t]))

(def Number? (t/pred Number))

(t/def-alias NumberAlias 
  Number)

(deftest class-pred-test
  (is (Number? 1))
  (is (not (Number? nil))))

(deftest hmap-pred-test
  (is ((every-pred
         (t/pred 
           (HMap)))
       {}
       {:a 'blah}))
  (is ((complement
          (t/pred 
            (HMap :mandatory {:a Number})))
        {}))
  (is ((t/pred 
         (HMap :mandatory {:a Number})) 
       {:a 1}))
  (is ((every-pred
         (t/pred 
           (HMap :optional {:a Number})))
       {:a 1}
       {}
       {:b 'a}))
  (is ((every-pred
         (t/pred 
           (HMap :absent-keys #{:a})))
       {:b 'a}))
  (is (not
        ((every-pred
           (t/pred 
             (HMap :absent-keys #{:a})))
         {:a 'a})))
  )

(deftest hvec-pred-test
  (is ((t/pred '[Number Number])
       [1 2]))
  (is ((every-pred
         (complement 
           (t/pred '[Number Number])))
       ['a 2]
       []
       [1]
       [1 2 3]))
  (is ((every-pred 
         (t/pred 
           '[Number Number Number *]))
       [1 2]
       [1 2 3]
       [1 2 3 4 5 6 6 7 4 2 1]))
  (is ((every-pred 
         (complement
           (t/pred 
             '[Number Number Number *])))
       []
       [1]
       [1 2 'a]
       [1 2 3 4 5 'a 6 7 4 2 1])))

(deftest rec-pred-test
  (is ((every-pred
         (t/pred (Rec [x] (U '[x] Number))))
       1
       '[1]
       '[[1]]
       '[[[[[2.2]]]]]))
  (is ((every-pred
         (t/pred (Rec [x] (U '{:a x} Number))))
       1
       '{:a 1}
       '{:a {:a 1}}
       '{:a {:a {:a {:a {:a 1}}}}}))
  (is ((every-pred
         (complement
           (t/pred (Rec [x] (U '[x] Number)))))
       '[1 1]
       '[[1] [1]])))

(deftest singleton-pred-test
  (is ((t/pred true)
       true))
  (is ((t/pred (Value true))
       true))
  (is ((t/pred false)
       false))
  (is ((t/pred (Value false))
       false))
  (is ((t/pred 'sym)
       'sym))
  (is ((t/pred (Value sym))
       'sym))
  (is ((t/pred ':sym)
       ':sym))
  (is ((t/pred (Value :sym))
       ':sym))
  (is ((t/pred nil)
       nil))
  (is ((t/pred (Value nil))
       nil))
  (is ((t/pred '1)
       1))
  (is ((every-pred
         (complement
           (t/pred '1)))
       1.0))
  (is ((t/pred (Value 1))
       1)))

(deftest countrange-pred-test
  (is ((every-pred
         (t/pred (CountRange 0)))
       nil
       []
       {}
       '()))
  (is ((every-pred
         (complement 
           (t/pred (CountRange 0))))
       ; only supports clojure collections
       (into-array [])
       )))

(deftest intersect-pred-test
  (is ((every-pred
         (t/pred (I Number Long)))
       1))
  (is ((every-pred
         (complement
           (t/pred (I Number Long))))
       1.1))
  (is ((every-pred
         (complement
           (t/pred (I Number Long))))
       1.1)))
       
(deftest union-pred-test
  (is ((every-pred
         (t/pred (U Number Long)))
       1
       1.1))
  (is ((every-pred
         (complement
           (t/pred (U Number Long))))
       'a))
  (is ((every-pred
         (complement
           (t/pred Nothing))
         (complement
           (t/pred (U))))
       'a)))

(deftest tfn-name-test
  (is ((every-pred
         (t/pred (clojure.core.typed/Option Number)))
       nil
       1
       1.1)))

(deftest iseq-pred-test
  (is ((every-pred
         (t/pred (t/Seq Number)))
       '(1 2)))
  (is ((every-pred
         (complement
           (t/pred (t/Seq Number))))
       [1 2])))

(deftest any-pred-test
  (is ((every-pred
         (t/pred Any))
       1 2 nil [1])))
