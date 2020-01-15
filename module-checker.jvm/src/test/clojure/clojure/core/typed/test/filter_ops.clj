(ns clojure.core.typed.test.filter-ops
  (:require 
    ; this loads the type system, must go first
    [clojure.core.typed.test.test-utils :refer :all]
    [clojure.core.typed :as t]
    [clojure.core.typed.checker.filter-ops :refer [-and -filter -not-filter -or]]
    [clojure.core.typed.checker.filter-rep :refer [-top make-AndFilter make-OrFilter -bot]]
    [clojure.core.typed.checker.type-rep :refer []]
    [clojure.core.typed.checker.jvm.parse-unparse :refer [parse-type]]
    [clojure.core.typed.checker.jvm.subtype :refer [subtype-type-filter?]]
            [clojure.test :refer :all]))

(deftest and-filter
  (is (= (-and -top)
         -top))
  (is (= (-and -top -top)
         -top))
  (is (= (-and -bot -bot)
         -bot))
  (is (= (-and -top -bot)
         -bot))
  (clj
    (let [f (-filter (parse-type `t/Num) 'x)]
      (is (= (-and f)
             f))
      (is (= (-and f -top)
             f))))
  (is-clj 
    (let [pf (-filter (parse-type `t/NonEmptyCount) 0)
          nf (-not-filter (parse-type nil) 0)]
      (= (-and pf nf)
         (make-AndFilter pf nf))))
  (is-clj
    (subtype-type-filter? (-and (-filter (parse-type `(t/U nil (t/NonEmptyVec t/Num))) 0)
                                (-filter (parse-type `(t/U nil t/EmptyCount)) 0))
                          (-filter (parse-type `nil) 0))))

(deftest or-filter
  (is (= (-or -bot -bot)
         -bot))
  (is (= (-or -top -top)
         -top))
  (is (= (-or -top -bot)
         -top))
  (is (= (-or -top (-filter (parse-type `nil) 0))
         -top))

  ;normalise to conjunctions of disjunctions
  (is (= (-or (-and (-filter (parse-type nil) 0)
                    (-filter (parse-type nil) 1))
              (-and (-filter (parse-type nil) 2)
                    (-filter (parse-type nil) 3)))
         (apply make-AndFilter
                (for [l [0 1]
                      r [2 3]]
                  (make-OrFilter (-filter (parse-type nil) l)
                                 (-filter (parse-type nil) r)))))))


;(clojure.core.typed.checker.filter-ops/-and                                                                                         
;  (! (U nil false) and__3941__auto____#0)
;  (is (U nil false) v2__#1)
;  (! (U nil false) and__3941__auto____#0)
;  (when
;    (is (U nil false) v1__#1)
;    (is (U clojure.core.typed/EmptyCount nil) v1__#0))
;  (is (U nil false) v1__#1)
;  (when
;    (! (U nil false) v2__#1)
;    (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))
;  (when
;    (is (U nil false) and__3941__auto____#0)
;    (! (U nil false) v1__#1))
;  (when
;    (! (U nil false) v1__#1)
;    (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))
;  (when
;    (is (U nil false) v2__#1)
;    (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
;
;(&
; (! (U nil false) and__3941__auto____#0)
; (when
;  (is (U nil false) v1__#1)
;  (is (U clojure.core.typed/EmptyCount nil) v1__#0))
; (is (U nil false) v1__#1)
; (when
;  (! (U nil false) v2__#1)
;  (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))
; (when
;  (is (U nil false) and__3941__auto____#0)
;  (! (U nil false) v1__#1))
; (when
;  (! (U nil false) v1__#1)
;  (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))
; (is (U nil false) v2__#1)
; (when
;  (is (U nil false) v2__#1)
;  (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
;
;(clojure.core.typed.checker.filter-ops/-or
; (&
;  (! (U nil false) v2__#1)
;  (! (U nil false) and__3941__auto____#0)
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0))
;  (is (U nil false) v1__#1)
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))
;  (when
;   (is (U nil false) and__3941__auto____#0)
;   (! (U nil false) v1__#1))
;  (when
;   (! (U nil false) v1__#1)
;   (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (&
;  (! nil v1__#0)
;  (! (U nil false) v1__#1)
;  (is clojure.core.typed/NonEmptyCount v1__#0)
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0))
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))
;  (is (U nil false) and__3941__auto____#0)
;  (when
;   (! (U nil false) and__3941__auto____#0)
;   (is (U nil false) v1__#1))
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0))))
;
;(&
; (| (! nil v1__#0) 
;    (! (U nil false) and__3941__auto____#0))
; (|
;  (when
;   (is (U nil false) and__3941__auto____#0)
;   (! (U nil false) v1__#1))
;  (when
;   (! (U nil false) and__3941__auto____#0)
;   (is (U nil false) v1__#1)))
; (|
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))
;  (when
;   (! (U nil false) v1__#1)
;   (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))))
; (|
;  (is (U nil false) and__3941__auto____#0)
;  (when
;   (! (U nil false) v1__#1)
;   (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))))
; (|
;  (! (U nil false) v2__#1)
;  (is clojure.core.typed/NonEmptyCount v1__#0))
; (|
;  (when
;   (is (U nil false) and__3941__auto____#0)
;   (! (U nil false) v1__#1))
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (|
;  (when
;   (! (U nil false) and__3941__auto____#0)
;   (is (U nil false) v1__#1))
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (| (! (U nil false) v1__#1) (! (U nil false) and__3941__auto____#0))
; (|
;  (! (U nil false) v2__#1)
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0)))
; (| (! nil v1__#0) (is (U nil false) v1__#1))
; (|
;  (when
;   (! (U nil false) and__3941__auto____#0)
;   (is (U nil false) v1__#1))
;  (when
;   (! (U nil false) v1__#1)
;   (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))))
; (|
;  (is (U nil false) and__3941__auto____#0)
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (|
;  (! nil v1__#0)
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0)))
; (|
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (| (! (U nil false) v2__#1) (is (U nil false) and__3941__auto____#0))
; (|
;  (! (U nil false) v2__#1)
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))))
; (|
;  (is clojure.core.typed/NonEmptyCount v1__#0)
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0)))
; (|
;  (is clojure.core.typed/NonEmptyCount v1__#0)
;  (is (U nil false) v1__#1))
; (|
;  (! nil v1__#0)
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))))
; (|
;  (when
;   (! (U nil false) v1__#1)
;   (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (|
;  (is clojure.core.typed/NonEmptyCount v1__#0)
;  (! (U nil false) and__3941__auto____#0))
; (|
;  (! (U nil false) v1__#1)
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0)))
; (|
;  (! nil v1__#0)
;  (when
;   (! (U nil false) v1__#1)
;   (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))))
; (|
;  (! (U nil false) v1__#1)
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))))
; (|
;  (! (U nil false) and__3941__auto____#0)
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0)))
; (|
;  (! (U nil false) v2__#1)
;  (when
;   (! (U nil false) and__3941__auto____#0)
;   (is (U nil false) v1__#1)))
; (|
;  (! nil v1__#0)
;  (when
;   (is (U nil false) and__3941__auto____#0)
;   (! (U nil false) v1__#1)))
; (when
;  (is (U nil false) v1__#1)
;  (is (U clojure.core.typed/EmptyCount nil) v1__#0))
; (|
;  (! (U nil false) v1__#1)
;  (when
;   (! (U nil false) v1__#1)
;   (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))))
; (|
;  (! (U nil false) v2__#1)
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (|
;  (! nil v1__#0)
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (|
;  (is clojure.core.typed/NonEmptyCount v1__#0)
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))))
; (|
;  (! (U nil false) v1__#1)
;  (when
;   (is (U nil false) and__3941__auto____#0)
;   (! (U nil false) v1__#1)))
; (|
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0))
;  (is (U nil false) v1__#1))
; (|
;  (! (U nil false) and__3941__auto____#0)
;  (when
;   (! (U nil false) and__3941__auto____#0)
;   (is (U nil false) v1__#1)))
; (| (is (U nil false) v1__#1) (is (U nil false) and__3941__auto____#0))
; (|
;  (is (U nil false) v1__#1)
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))))
; (|
;  (is clojure.core.typed/NonEmptyCount v1__#0)
;  (when
;   (! (U nil false) v1__#1)
;   (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))))
; (|
;  (! (U nil false) v1__#1)
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (|
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0))
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))))
; (when
;  (! (U nil false) v2__#1)
;  (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))
; (|
;  (is clojure.core.typed/NonEmptyCount v1__#0)
;  (when
;   (is (U nil false) and__3941__auto____#0)
;   (! (U nil false) v1__#1)))
; (|
;  (! (U nil false) and__3941__auto____#0)
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))))
; (|
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0))
;  (when
;   (! (U nil false) v1__#1)
;   (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))))
; (|
;  (is clojure.core.typed/NonEmptyCount v1__#0)
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (| (! nil v1__#0) (! (U nil false) v2__#1))
; (|
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0))
;  (when
;   (is (U nil false) and__3941__auto____#0)
;   (! (U nil false) v1__#1)))
; (|
;  (is (U nil false) v1__#1)
;  (when
;   (! (U nil false) and__3941__auto____#0)
;   (is (U nil false) v1__#1)))
; (|
;  (is (U nil false) and__3941__auto____#0)
;  (when
;   (is (U nil false) and__3941__auto____#0)
;   (! (U nil false) v1__#1)))
; (|
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))
;  (when
;   (is (U nil false) and__3941__auto____#0)
;   (! (U nil false) v1__#1)))
; (|
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0))
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (|
;  (is (U nil false) v1__#1)
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (when
;  (is (U nil false) v2__#1)
;  (is (U clojure.core.typed/EmptyCount nil) v2__#0))
; (| (! (U nil false) v2__#1) (! (U nil false) v1__#1))
; (|
;  (! (U nil false) and__3941__auto____#0)
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0))))
