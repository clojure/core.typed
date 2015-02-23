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
            [clojure.core.typed.debug :as dbg]))

(defn simple-filter-better? [f1 f2]
  (or (fl/NoFilter? f2)
      (sub/subtype-filter? f1 f2)))

;; apply f1 to the current environment, and if the type filter
;; is boring enough it will reflect in the updated type environment
;(defn can-extract-in? [env f1 f2]
;  (cond
;    (fl/TypeFilter? f2) (let [good? (atom true)
;                              new-env (update/env+ env [f1] good?)]
;                          (boolean
;                            (when @good?
;                            )))

;; check-below : (/\ (Result Type -> Result)
;;                   (Result Result -> Result)
;;                   (Type Result -> Type)
;;                   (Type Type -> Type))

;check that arg type tr1 is under expected
(defn check-below [tr1 expected]
  {:pre [((some-fn r/TCResult? r/Type?) tr1)
         ((some-fn r/TCResult? r/Type?) expected)]
   :post [(cond
            (r/TCResult? tr1) (r/TCResult? %)
            (r/Type? tr1) (r/Type? %))]}
  (letfn [;; Try and use subtyping, otherwise 
          (filter-better? [{f1+ :then f1- :else :as f1}
                           {f2+ :then f2- :else :as f2}]
            {:pre [(fl/FilterSet? f1)
                   (fl/FilterSet? f2)]
             :post [(con/boolean? %)]}
            (cond
              (= f1 f2) true
              :else
              (let [f1-better? (simple-filter-better? f1+ f2+)
                    f2-better? (simple-filter-better? f1- f2-)] 
                (and f1-better? f2-better?))))
          (object-better? [o1 o2]
            {:pre [(obj/RObject? o1)
                   (obj/RObject? o2)]
             :post [(con/boolean? %)]}
            (cond
              (= o1 o2) true
              ((some-fn obj/NoObject? obj/EmptyObject?) o2) true
              :else false))
          (flow-better? [{flow1 :normal :as f1}
                         {flow2 :normal :as f2}]
            {:pre [((every-pred r/FlowSet?) f1 f2)]
             :post [(con/boolean? %)]}
            (cond
              (= flow1 flow2) true
              (fl/NoFilter? flow2) true
              (sub/subtype-filter? flow1 flow2) true
              :else false))
          (construct-ret [tr1 expected]
            {:pre [((every-pred r/TCResult?) tr1 expected)]
             :post [(r/TCResult? %)]}
            (r/ret (r/ret-t expected)
                   (let [exp-f (r/ret-f expected)
                         tr-f (r/ret-f tr1)]
                     (fo/-FS (if-not (fl/NoFilter? (:then exp-f))
                               (:then exp-f)
                               (:then tr-f))
                             (if-not (fl/NoFilter? (:else exp-f))
                               (:else exp-f)
                               (:else tr-f))))
                   (let [exp-o (r/ret-o expected)
                         tr-o (r/ret-o tr1)]
                     (if (obj/NoObject? exp-o)
                       tr-o
                       exp-o))
                   (let [exp-flow (r/ret-flow expected)
                         tr-flow (r/ret-flow tr1)]
                     (if (fl/NoFilter? (:normal exp-flow))
                       tr-flow
                       exp-flow))))]
    ;tr1 = arg
    ;expected = dom
    (cond
      (and (r/TCResult? tr1)
           (r/TCResult? expected))
      (let [{t1 :t f1 :fl o1 :o flow1 :flow} tr1
            {t2 :t f2 :fl o2 :o flow2 :flow} expected]
        (cond
          (not (sub/subtype? t1 t2)) (cu/expected-error t1 t2)

          :else
          (let [better-fs? (filter-better? f1 f2)
                ;_ (prn "better-fs?" better-fs? f1 f2)
                better-obj? (object-better? o1 o2)
                better-flow? (flow-better? flow1 flow2)
                ;_ (prn "better-flow?" better-flow? flow1 flow2)
                ]
            (cond
              (not better-flow?) (err/tc-delayed-error (str "Expected result with flow filter " (pr-str flow2) 
                                                            ", got flow filter "  (pr-str flow1)))
              (and (not better-fs?)
                   better-obj?)
              (err/tc-delayed-error (str "Expected result with filter " (pr-str f2) ", got filter "  (pr-str f1)))

              (and better-fs? 
                   (not better-obj?))
              (err/tc-delayed-error (str "Expected result with object " (pr-str o2) ", got object " (pr-str o1)))

              (and (not better-fs?)
                   (not better-obj?))
              (err/tc-delayed-error (str "Expected result with object " (pr-str o2) ", got object"  o1 " and filter "
                                         (pr-str f2) " got filter " (pr-str f1))))))
        (construct-ret tr1 expected))

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
          (err/tc-delayed-error (str "Expected result with filter " (pr-str f) " and object " (pr-str o) ", got trivial filter and empty object."))
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

(defn maybe-check-below
  [tr1 expected]
  {:pre [(r/TCResult? tr1)
         ((some-fn nil? r/TCResult?) expected)]
   :post [(r/TCResult? %)]}
  (or (when expected
        (check-below tr1 expected))
      tr1))

