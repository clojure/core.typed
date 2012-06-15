(ns typed.test.example
  (:use [clojure.test])
  (:require [typed.core :refer [subtype? Fn Any Nothing]]))

(deftest form-test
  (is (assoc {} :a 1)))

;; subtype
(deftest value-subtypes
  (is (subtype? 1 1))
  (is (not (subtype? 1 2))))

(deftest value-subtypes
  (is (subtype? 1 1))
  (is (not (subtype? 1 2))))

(deftest function-subtypes
  (is (subtype? (Fn [Number -> Number]) (Fn [Number -> Number]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtyping

(deftest subtype-any-nothing
  (is (subtype? (Fn [1 -> 1]) Any))
  (is (subtype? Nothing (Fn [1 -> 1])))
  (is (subtype? Nothing Any))
  (is (subtype? Nothing Object))
  (is (subtype? Nothing nil))
  (is (subtype? Nil Any))
  (is (subtype? Long Any)))

(assoc {}
       :a 1
       :b 2)
;=> {:a 1, :b 2}



(conj {:a 1}
      (when 1
        [:b 2])
      [:c 3])


(comment
(deftest subtype-object
  (is (subtype? (Fn [1 -> 1]) Object))
  (is (subtype? byte Object))
  (is (subtype? short Object))
  (is (subtype? int Object))
  (is (subtype? long Object))
  (is (subtype? float Object))
  (is (subtype? double Object))
  (is (subtype? char Object))
  (is (subtype? boolean Object))
  (is (not (subtype? nil Object)))
  (is (subtype? Object Object)))

(deftest subtype-fun
  (is (subtype? (Fn [-> nil]) clojure.lang.IFn))
  (is (subtype? (Fn [-> nil]) clojure.lang.AFn))
  (is (subtype? (Fn [-> nil]) clojure.lang.IObj)))

(deftest subtype-classes
  (is (subtype? Long Long))
  (is (subtype? Long Object)))

(deftest subtype-singletons
  (is (not (subtype? 1 2)))
  (is (subtype? 1 1))
  (is (subtype? 1 Long))
  (is (not (subtype? Long 1)))
  (is (subtype? :a :a))
  (is (not (subtype? :a :b)))
  (is (subtype? :a Keyword))
  (is (not (subtype? Keyword :a)))
  (is (subtype? (U :a :b) Keyword)))

(deftest subtype-nil
  (is (subtype? nil nil))
  (is (subtype? (U nil) nil))
  (is (not (subtype? nil Var)))
  (is (not (subtype? nil 1)))
  (is (subtype? nil ISeq))          ; nil implements first, rest, cons
  (is (not (subtype? nil Seqable))) ; nil does not implement clojure.lang.ISeq/seq
  (is (subtype? nil IMeta))
  (is (subtype? nil IObj))
  (is (subtype? nil Counted))
  (is (subtype? nil ILookup))
  (is (subtype? nil Associative)))

(deftest subtype-ISeq
  (is (subtype? nil ISeq))
  (is (not (subtype? Iterable ISeq)))
  (is (not (subtype? java.util.Map ISeq))))

(deftest subtype-Seqable
  (is (not (subtype? nil Seqable)))
  (is (subtype? Iterable Seqable))
  (is (subtype? java.util.Map Seqable)))

(deftest subtype-unions
         (is (subtype? (U)
                       (U)))
         (is (subtype? (U)
                       (U Object nil)))
         (is (not (subtype? (U Object nil) 
                            (U))))
         (is (subtype? (U Long) 
                       (U Long)))
         (is (subtype? (U Long Integer) 
                       (U Integer Long)))
         (is (subtype? (U (U Class String) Long Integer)
                       (U Integer (U String Class) Long)))
         (is (not (subtype? (U Object) (U Long))))
         (is (not (subtype? Object (U Long))))
         (is (subtype? Long (U Object)))
         (is (subtype? (U Float Integer Double) Object))
         )

(deftest subtype-funs
  (is (subtype? [1 -> 2] 
            [1 -> 2]))
  (is (subtype? [Long -> 1] 
            [1 -> Long]))
  (is (subtype? [Object Long -> 1] 
            [Long Long -> Long]))
  (is (subtype? [Long -> Long]
            [1 -> Any]
            )))

(deftest subtype-qual-keywords
  (is (subtype? ::a ::a))
  (is (subtype? t/Type t/Type))
  (is (subtype? ClassType t/Type))
  (is (not (subtype? t/Type Object))))

(deftest subtype-varargs
         (is (subtype? (Fn [Number & Object * -> Boolean]) 
                       (Fn [Number & Number * -> Boolean])))
         (is (subtype? (Fn [Object & Number * -> Boolean])
                       (Fn [Number & Number * -> Boolean])))
         (is (subtype? (Fn [Number & Number * -> Boolean])
                       (Fn [Number & Number * -> Boolean])))
         (is (subtype? (Fn [Number & Number * -> Boolean])
                       (Fn [Number & Number * -> Object])))_
         (is (subtype? (Fn [Number & Number * -> Number])
                       (Fn [Number & Number * -> Number])))
         (is (subtype? (Fn [Number Number & Boolean * -> Number])
                       (Fn [Number Number -> Number])))
         (is (not
               (subtype? (Fn [Number & Number * -> Boolean])
                         (Fn [Number Number Number -> Number]))))
         (is (subtype? (Fn [Number Number & Boolean * -> Number])
                       (Fn [Number Number Boolean Boolean -> Number])))
         (is (subtype? 
               (Fn [Long Long & Long * -> Long])
               (Fn [1 1 1 1 1 1 1 -> Any])))
         (is (subtype? 
               (Fn [Long Long Long Long -> Any])
               clojure.lang.IFn))
         (is (not (subtype? 
                    clojure.lang.IFn
                    (Fn [Long Long Long Long -> Any]))))
         )

(deftest subtype-vectors
         (is (subtype? (Vector Number)
                       IPersistentVector))
         (is (subtype? (Vector Number)
                       clojure.lang.Sequential))
         (is (subtype? (Vector Integer)
                       (Vector Number)))
         (is (not (subtype? (Vector Number)
                            (Vector Integer))))
         )

(deftest subtype-seqable
         (is (subtype? (Seq Double)
                       clojure.lang.ISeq))
         (is (subtype? (Seq Double)
                       (Seq Number)))
         (is (subtype? (Vector Double)
                       (Seq Number))))

;(deftest subtype-primitives
;         (is (subtype? void void))
;         (is (subtype? nil void))
;         (is (subtype? void nil))
;         (is (subtype? int int))
;         (is (subtype? double double))
;         (is (subtype? float float))
;         (is (subtype? boolean boolean))
;         (is (subtype? long long)))

;(deftest subtype-primitive-boxing
;         (is (subtype? long Long))
;         (is (subtype? Long long))
;         (is (subtype? double Double))
;         (is (subtype? Double double))
;         )

;(deftest subtype-primitive-numbers
;         (is (subtype? long Number))
;         (is (subtype? double Number))
;         (is (subtype? int Number))
;         (is (subtype? byte Number))
;         (is (subtype? short Number))
;         )

(deftest subtype-variables
         (is (subtype? (All [[x :variance :covariant]] x)
                       (All [[x :variance :covariant]] x)))
         (is (not (subtype? (All [[x :variance :covariant]] x)
                            (All [[y :variance :covariant]] y)))))

  )
