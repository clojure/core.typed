(ns clojure.core.typed.check-below
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.errors :as err]
            )
  )

;; check-below : (/\ (Results Type -> Result)
;;                   (Results Results -> Result)
;;                   (Type Results -> Type)
;;                   (Type Type -> Type))

;check that arg type tr1 is under expected
(defn check-below [tr1 expected]
  {:pre [((some-fn r/TCResult? r/Type?) tr1)
         ((some-fn r/TCResult? r/Type?) expected)]
   :post [((some-fn r/TCResult? r/Type?) %)]}
  (letfn [(filter-better? [{f1+ :then f1- :else :as f1}
                           {f2+ :then f2- :else :as f2}]
            {:pre [(fl/Filter? f1)
                   (fl/Filter? f2)]
             :post [(con/boolean? %)]}
            (cond
              (= f1 f2) true
              (and (fo/implied-atomic? f2+ f1+)
                   (fo/implied-atomic? f2- f1-)) true
              :else false))
          (object-better? [o1 o2]
            {:pre [(obj/RObject? o1)
                   (obj/RObject? o2)]
             :post [(con/boolean? %)]}
            (cond
              (= o1 o2) true
              ((some-fn obj/NoObject? obj/EmptyObject?) o2) true
              :else false))]
    ;tr1 = arg
    ;expected = dom
    ; Omitted some cases dealing with multiple return values
    (cond
      (and (r/TCResult? tr1)
           (r/TCResult? expected)
           (= (c/Un) (r/ret-t tr1))
           (fl/NoFilter? (r/ret-f expected))
           (obj/NoObject? (r/ret-o expected)))
      (let [ts2 (:t tr1)]
        (r/ret ts2))

      (and (r/TCResult? tr1)
           (= (c/Un) (r/ret-t tr1)))
      expected

      (and (r/TCResult? tr1)
           (r/TCResult? expected)
           (= (fo/-FS fl/-top fl/-top)
              (r/ret-f expected))
           (obj/EmptyObject? (r/ret-o expected)))
      (let [{t1 :t f1 :fl o1 :o} tr1
            {t2 :t} expected]
        (when-not (sub/subtype? t1 t2)
          (cu/expected-error t1 t2))
        expected)

      (and (r/TCResult? tr1)
           (r/TCResult? expected))
      (let [{t1 :t f1 :fl o1 :o} tr1
            {t2 :t f2 :fl o2 :o} expected]
        (cond
          (not (sub/subtype? t1 t2)) (cu/expected-error t1 t2)

          (and (not (filter-better? f1 f2))
               (object-better? o1 o2))
          (err/tc-delayed-error (str "Expected result with filter " f2 ", got filter"  f1))

          (and (filter-better? f1 f2)
               (not (object-better? o1 o2)))
          (err/tc-delayed-error (str "Expected result with object " o2 ", got object"  o1))

          (and (not (filter-better? f1 f2))
               (not (object-better? o1 o2)))
          (err/tc-delayed-error (str "Expected result with object " o2 ", got object"  o1 " and filter "
                                   f2 " got filter " f1)))
        expected)

      (and (r/TCResult? tr1)
           (r/Type? expected))
      (let [{t1 :t f :fl o :o} tr1
            t2 expected]
        (when-not (sub/subtype? t1 t2)
          (cu/expected-error t1 t2))
        (r/ret t2 f o))

      ;FIXME
      ;; erm.. ? What is (FilterSet: (list) (list))
      ;; TODO this case goes here, but not sure what it means 
      ;
      ;[((? r/Type? t1) (tc-result1: t2 (FilterSet: (list) (list)) (Empty:)))
      ; (unless (sub/subtype t1 t2)
      ;   (tc-error/expr "Expected ~a, but got ~a" t2 t1))
      ; t1]

      (and (r/Type? tr1)
           (r/TCResult? expected))
      (let [t1 tr1
            {t2 :t f :fl o :o} expected]
        (if (sub/subtype? t1 t2)
          (err/tc-delayed-error (str "Expected result with filter " f " and " o ", got " t1))
          (cu/expected-error t1 t2))
        t1)

      (and (r/Type? tr1)
           (r/Type? expected))
      (let [t1 tr1
            t2 expected]
        (when-not (sub/subtype? t1 t2)
          (cu/expected-error t1 t2))
        expected)

      :else (let [a tr1
                  b expected]
              (err/int-error (str "Unexpected input for check-below " a b))))))
