(ns typed.test.core
  (:require [typed.core :refer :all, :as t]
            [analyze.core :refer [ast]]
            [clojure.test :refer :all])
  (:import (clojure.lang Keyword IPersistentVector Sequential IPersistentList Var Ratio
                         Symbol IPersistentMap ISeq Seqable Counted ILookup Associative
                         IMeta IObj IFn Symbol)
           (typed.core ClassType)))

(defmacro type= [s t]
  `(binding [*ns* (find-ns 'typed.test.core)]
     (= (parse '~s)
        (parse '~t))))

(defmacro prse [t]
  `(binding [*ns* (find-ns 'typed.test.core)]
     (parse '~t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Free Variables

(deftest free-vars-fun
  (is (= (-> (free-vars (parse '(All [x y] [x -> y])))
           set)
         #{(-tv 'x) (-tv 'y)})))

(deftest free-vars-bnd-vars
  (is (= (-> (free-vars (-tv 'x (parse '(All [y] 
                                             (Vectorof y)))))
           set)
         #{(-tv 'x (parse '(All [y] 
                                (Vectorof y)))) 
           (-tv 'y)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable rename

(deftest var-rename
  (is (= (rename (-tv 'x)
                 {(-tv 'x) 
                  (-tv 'y)})
         (-tv 'y)))
  (is (not= (rename (parse '(All [x] x))
                    {(-tv 'x) 
                     (-tv 'y)})
            (-tv 'y)))
  (is (= (rename (parse 'Object)
                 {(-tv 'x)
                  (-tv 'y)})
         (parse 'Object))))

(deftest res-conflict-test
  (is (let [x (-tv 'x)
            t (parse '(All [x] x))]
        (not= (resolve-conflicts t #{x})
              t)))
  (is (let [x (-tv 'x)
            t (parse '(All [y] y))]
        (= (resolve-conflicts t #{x})
           t)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable elimination

;; Promote

(deftest var-elim-promote-no-vars
  (is (= (promote Any #{}) 
         Any))
  (is (= (promote Nothing #{}) 
         Nothing))
  (is (= (promote Nil #{}) 
         Nil))
  (is (= (promote (parse 'Object) #{})
         (parse 'Object))))

(deftest var-elim-promote-vars
  (is (let [t1 (-tv 'x)]
        (= (promote t1 #{t1}) 
           Any)))
  (is (let [t1 (-tv 'x)
            t2 (-tv 'y)]
        (= (promote t1 #{t2})
           t1))))

(deftest var-elim-promote-vars-with-bnd
  (is (let [bnd (parse 'Double)
            t1 (-tv 'x bnd)]
        (= (promote t1 #{t1}) 
           bnd)))
  (is (let [bnd (parse 'Double)
            t1 (-tv 'x bnd)
            t2 (-tv 'y)]
        (= (promote t1 #{t2}) 
           t1))))

(deftest var-elim-promote-funs
  ;identity
  (is (= (promote (parse '(All [x] [x -> x])) #{})
         (parse '(All [x] [x -> x]))))
  ;x is free
  (is (let [fun (-fun [(make-tvar-binding (map->arity {:dom [(-tv 'x)]
                                                       :rest-type (-tv 'y)
                                                       :rng (-tv 'y)})
                                          [(-tv 'y)])])]
        (= (promote fun #{(-tv 'x)})
           (prse (All [y] [Nothing & y * -> y])))))
  ;x is free
  (is (let [fun (-fun [(make-tvar-binding (map->arity {:dom [(-tv 'y)]
                                                :rest-type (-tv 'x)
                                                :rng (-tv 'x)})
                                          [(-tv 'y)])])]
        (= (promote fun #{(-tv 'x)})
           (prse (All [y] [y & Nothing * -> Any])))))
         )

;; Demote

(deftest var-elim-demote-no-vars
  (is (= (demote Any #{}) Any))
  (is (= (demote Nothing #{}) Nothing))
  (is (= (demote Nil #{}) Nil))
  (is (= (demote (parse 'Object) #{}) (parse 'Object))))

(deftest var-elim-demote-vars
  (is (let [t1 (-tv 'x)]
        (= (demote t1 #{t1}) 
           Nothing)))
  (is (let [t1 (-tv 'x)
            t2 (-tv 'y)]
        (= (demote t1 #{t2}) 
           t1))))

(deftest var-elim-demote-vars-with-bnd
  (is (let [bnd (parse 'Double)
            t1 (-tv 'x bnd)]
        (= (demote t1 #{t1}) 
           Nothing)))
  (is (let [bnd (parse 'Double)
            t1 (-tv 'x bnd)
            t2 (-tv 'y)]
        (= (demote t1 #{t2}) 
           t1))))
         
(deftest var-elim-demote-funs
  ;identity
  (is (= (demote (parse '(All [x] [x -> x])) #{})
         (parse '(All [x] [x -> x]))))
  ;x is free
  (is (let [fun (-fun [(make-tvar-binding (map->arity {:dom [(-tv 'x)]
                                                       :rest-type (-tv 'y)
                                                       :rng (-tv 'y)})
                                          [(-tv 'y)])])]
        (= (demote fun #{(-tv 'x)})
           (prse (All [y] [Any & y * -> y])))))
  ;x is free
  (is (let [fun (-fun [(make-tvar-binding (map->arity {:dom [(-tv 'y)]
                                                       :rest-type (-tv 'x)
                                                       :rng (-tv 'x)})
                                          [(-tv 'y)])])]
        (= (demote fun #{(-tv 'x)})
           (prse (All [y] [y & Any * -> Nothing]))))))

;; Eliminating vars occuring in bounds of variables

(deftest var-elim-promote-vs-in-bnds
  ; Rule VU-Fun-2
  (is (let [y (-tv 'y)
            x (-tv 'x (->Vector (-tv 'y)))]
        (= (promote x
                    #{y})
           Any))))

(deftest var-elim-demote-vs-in-bnds
  ; Rule VD-Fun-2
  (is (let [y (-tv 'y)
            x (-tv 'x (->Vector (-tv 'y)))]
        (= (demote x
                   #{y})
           Nothing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint Generation

(deftest empty-constraint-set-test
  (is (let [x (-tv 'x)
            y (-tv 'y)]
        (= (empty-constraint-set #{x y})
           (->ConstraintSet
             {x (->SubConstraint Nothing Any)
              y (->SubConstraint Nothing Any)})))))

(deftest singleton-constraint-set-test
  (is (let [x (-tv 'x)
            y (-tv 'y)]
        (= (singleton-constraint-set x (->EqConstraint (ClassType-from Number)) #{y})
           (->ConstraintSet
             {x (->EqConstraint (ClassType-from Number))
              y (->SubConstraint Nothing Any)})))))

(deftest tvar-constraints-test
  (testing "x is at most a Number, must be a subtype of itself"
  (is (let [x (-tv 'x (ClassType-from Number))
            xs #{x}]
        (= (constraint-gen #{} xs x x)
           (->ConstraintSet
             {x (sub-constraint Nothing (ClassType-from Number))})))))

  (testing "x is at most a Number, y is anything, both bounded by Any"
  (is (let [x (-tv 'x)
            y (-tv 'y)
            xs #{x y}]
        (= (constraint-gen #{} xs x (ClassType-from Number))
           (->ConstraintSet
             {x (->SubConstraint Nothing (ClassType-from Number))
              y (->SubConstraint Nothing Any)})))))

  (testing "x is bounded by Number, can't be constrained to Any"
  (is (let [x (-tv 'x (ClassType-from Number))
            xs #{x}]
        (= (constraint-gen #{} xs x Any)
           (->ConstraintSet
             {x (->SubConstraint Nothing (ClassType-from Number))})))))
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculating Variances

(defmacro with-frees [xs tsyn]
  `(with-type-vars (into {} (map #(vector (:nme %) %) ~xs))
     (parse '~tsyn)))

(deftest variance-type-variables
  (testing "with covariance"
           (is (let [x (-tv 'x)
                     variance (with-covariance 
                                (calculate-variances x #{x}))]
                 (= {x covariant}
                    variance))))
  (testing "with contravariance"
           (is (let [x (-tv 'x)
                     variance (with-contravariance
                                (calculate-variances x #{x}))]
                 (= {x contravariant}
                    variance))))
  (testing "in a scope"
           (is (let [x (-tv 'x)
                     t (with-frees [x]
                                   (All [y] x))
                     variance (with-covariance
                                (calculate-variances t #{x}))]
                 (= {x covariant}
                    variance))))
  (testing "constant x, (x does not occur)"
           (is (let [x (-tv 'x)
                     t (with-frees [x]
                                   (All [x] x))
                     variance (with-covariance
                                (calculate-variances t #{x}))]
                 (= {x constant}
                    variance)))))

(deftest variance-function-fixed-domain
  (testing "function domain, with covariance"
    (is (let [x (-tv 'x)
              t (with-frees [x]
                            (All [y] [x -> y]))
              variance (with-covariance
                         (calculate-variances t #{x}))]
          (= {x contravariant}
             variance))))
  (testing "function domain, with contravariance"
    (is (let [x (-tv 'x)
              t (with-frees [x]
                            (All [y] [x -> y]))
              variance (with-contravariance
                         (calculate-variances t #{x}))]
          (= {x covariant}
             variance)))))

(deftest variance-function-rest-type
  (testing "function rest type, with covariance"
           (is (let [x (-tv 'x)
                     t (with-frees [x]
                                   (All [y] [& x * -> y]))
                     variance (with-covariance
                                (calculate-variances t #{x}))]
                 (= {x contravariant}
                    variance))))
  (testing "function rest type, with contravariance"
  (is (let [x (-tv 'x)
            t (with-frees [x]
                (All [y] [& x * -> y]))
            variance (with-contravariance
                       (calculate-variances t #{x}))]
        (= {x covariant}
           variance)))))

(deftest variance-function-range
  (testing "function range, with covariance"
    (is (let [x (-tv 'x)
              t (with-frees [x]
                            (All [y] [y -> x]))
              variance (with-covariance
                         (calculate-variances t #{x}))]
          (= {x covariant}
             variance))))
  (testing "function range, with contravariance"
    (is (let [x (-tv 'x)
              t (with-frees [x]
                            (All [y] [y -> x]))
              variance (with-contravariance
                         (calculate-variances t #{x}))]
          (= {x contravariant}
             variance)))))

(deftest invariant-tests
  (testing "variable on both sides of arrow"
    (is (let [x (-tv 'x)
              t (with-frees [x]
                            [x -> x])
              variance (with-covariance
                         (calculate-variances t #{x}))]
          (= {x invariant}
             variance)))))

(deftest variance-calc-complex-nesting
  (testing "variance in nested functions"
    (is (let [x (-tv 'x)
              y (-tv 'y)
              z (-tv 'z)
              t (with-frees [x y z]
                  [x y x & [z -> x] * -> [x -> y]])
              variance (with-covariance
                         (calculate-variances t #{x y z}))]
          (= {x contravariant
              y invariant
              z covariant}
             variance)))))

(deftest variance-calc-bnds
  (testing "variable in bound"
    (testing "covariant position is rigid"
      (is (let [x (-tv 'x)
                t (with-frees [x]
                    (All [(y <! (Vectorof x))]
                      y))
                variance (with-covariance
                           (calculate-variances t #{x}))]
            (= {x rigid}
               variance)))))
    (testing "contravariant position is rigid"
      (is (let [x (-tv 'x)
                t (with-frees [x]
                    (All [(y <! (Vectorof x))]
                      y))
                variance (with-contravariance
                           (calculate-variances t #{x}))]
            (= {x rigid}
               variance))))
    (testing "bound variable does not occur in body"
      (is (let [x (-tv 'x)
                t (with-frees [x]
                    (All [(y <! (Vectorof x))]
                      x))
                variance (with-contravariance
                           (calculate-variances t #{x}))]
            (= {x contravariant}
               variance)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polymorphic calls

(defmacro tc [form]
  `(binding [*ns* (find-ns 'typed.test.core)]
     (tc-expr (ast ~form))))

(defmacro with-env [& body]
  `(binding [*ns* (find-ns 'typed.test.core)]
     (with-type-anns ~@body)))

(deftest poly-manual-inst
  (is (= (->
           (with-env 
             {id (All [x] [x -> x])}
             (tc
               (do (declare id)
                 (with-type-args [Number]
                   (id 1)))))
           type-key)
         (ClassType-from 'Number))))

(deftest poly-call-test
  (testing "identity"
    (is (= (type-key
             (with-env 
               {id (All [x] [x -> x])}
               (tc
                 (do (declare id)
                   (id 1)))))
           (parse 1))))
  (is (thrown?
        Exception
        (with-env 
          {id (All [(x <! Number)] [x -> x])}
          (tc
            (do (declare id)
              (id \c)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equality

(deftest eq-tvar
  (is (= (-tv 'x)
         (-tv 'x)))
  (is (not (= (-tv 'y)
              (-tv 'x)))))

(deftest eq-union
  (is (type= (U nil Object)
             (U Object nil)))
  (is (not (type= (U Object)
                  (U Object nil))))
  (is (not (type= (U Object nil)
                  (U Object))))
  (is (type= Any Any))
  (is (not (type= (U (U 1 2) (U 3 4))
                  (U 1 2 3 4))))
  (is (type= (U (U 1 2) (U 3 4))
             (U (U 3 4) (U 1 2)))))
         
(deftest eq-fun
  (is (type= [-> nil]
             [-> nil]))
  (is (type= (Fun [-> nil]
                  [nil -> nil]
                  [nil nil -> nil])
             (Fun [nil nil -> nil]
                  [nil -> nil]
                  [-> nil])))
  (is (not (type= [-> nil]
                  [-> Object])))
  (is (type= [Integer & Nothing * -> Object]
             [Integer & Nothing * -> Object])))

(deftest eq-class-and-prim
  (is (type= Object Object))
  (is (not (type= Integer Object)))
  (is (not (type= int Integer)))
  (is (type= int int)))

(deftest eq-all
  (is (type= Object (All [x] Object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bounds

(deftest all-bounded
  (is (= (parse '(All [(x <! Number)]
                      x))
         (make-tvar-binding (-tv 'x (parse 'Number))
                            [(-tv 'x (parse 'Number))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtyping

(defmacro sub? [s t]
  `(binding [*ns* (find-ns 'typed.test.core)]
     (subtype? (parse '~s)
               (parse '~t))))

(deftest subtype-any-nothing
  (is (sub? [1 -> 1] Any))
  (is (sub? Nothing [1 -> 1]))
  (is (sub? Nothing Any))
  (is (sub? Nothing Object))
  (is (sub? Nothing nil))
  (is (sub? nil Any))
  (is (sub? Long Any)))

(deftest subtype-object
  (is (sub? [1 -> 1] Object))
  (is (sub? byte Object))
  (is (sub? short Object))
  (is (sub? int Object))
  (is (sub? long Object))
  (is (sub? float Object))
  (is (sub? double Object))
  (is (sub? char Object))
  (is (sub? boolean Object))
  (is (not (sub? nil Object)))
  (is (sub? Object Object)))

(deftest subtype-fun
  (is (sub? [-> nil] IFn))
  (is (sub? [-> nil] clojure.lang.AFn))
  (is (sub? [-> nil] IObj)))

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
  (is (not (sub? nil Var)))
  (is (not (sub? nil 1)))
  (is (sub? nil ISeq))          ; nil implements first, rest, cons
  (is (not (sub? nil Seqable))) ; nil does not implement clojure.lang.ISeq/seq
  (is (sub? nil IMeta))
  (is (sub? nil IObj))
  (is (sub? nil Counted))
  (is (sub? nil ILookup))
  (is (sub? nil Associative)))

(deftest subtype-ISeq
  (is (sub? nil ISeq))
  (is (not (sub? Iterable ISeq)))
  (is (not (sub? java.util.Map ISeq))))

(deftest subtype-Seqable
  (is (not (sub? nil Seqable)))
  (is (sub? Iterable Seqable))
  (is (sub? java.util.Map Seqable)))

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

(deftest subtype-qual-keywords
  (is (sub? ::a ::a))
  (is (sub? t/Type t/Type))
  (is (sub? ClassType t/Type))
  (is (not (sub? t/Type Object))))

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
        [Long Long & Long * -> Long]
        [1 1 1 1 1 1 1 -> Any]))
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

(deftest subtype-seqable
  (is (sub? (Seqof Double)
            ISeq))
  (is (sub? (Seqof Double)
            (Seqof Number)))
  (is (sub? (Sequentialof Double)
            (Seqof Number)))
  (is (sub? (Vectorof Double)
            (Seqof Number))))

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
  (is (sub? (Mapof [Nothing Nothing])
            IPersistentMap))
  (is (sub? (Mapof [Keyword Double])
            IPersistentMap))
  #_(is (sub? (Map* [Integer Object] [Double Integer] [Number Number])
            IPersistentMap))
  (is (sub? (Mapof [Integer Double])
            (Mapof [Number Number])))
  (is (not (sub? (Mapof [Number Number])
                 (Mapof [Integer Double]))))
  #_(is (sub? (Map* [Integer Double] [Double Integer] [Number Number])
            (Mapof [Number Number])))
  #_(is (not (sub? (Map* [Integer Object] [Double Integer] [Number Number])
                 (Mapof [Number Number]))))
  #_(is (sub? (Map*)
              (Mapof [Nothing Nothing])))
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
  (is (sub? Long long))
  (is (sub? double Double))
  (is (sub? Double double))
         )

(deftest subtype-primitive-numbers
  (is (sub? long Number))
  (is (sub? double Number))
  (is (sub? int Number))
  (is (sub? byte Number))
  (is (sub? short Number))
         )

(deftest subtype-protocols
  (is (sub? Symbol typed.core/IParseType))
  (is (sub? nil typed.core/IParseType))
  (is (sub? (Vectorof Double) typed.core/IParseType)))

(deftest subtype-variables
  (is (sub? (All [x] x)
            (All [x] x)))
  (is (not (sub? (All [x] x)
                 (All [y] y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All literal

(deftest all-parse-unparse
  (is (let [syn '(All [x] (U x clojure.lang.Symbol))]
        (subtype? (-> syn parse unparse parse)
                  (parse syn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Checking

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
  #_(is (subfrm
        {:a :b}
        (Map* [:a :b])))
  #_(is (subfrm
        {1 2 3 4 5 6 7 8}
        (Map* 1 2 5 6 7 8 3 4)))
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

(deftest tc-expr-def
  (is (subfrm (def a)
              Var))
  (is (with-env
        {b Long}
        (tc 
          (def b 1))))
  (is (with-env
        {double-num [Number -> Number]
         add-twice [Number -> Number]}
        (subfrm 
          (do
            (defn add-twice [n]
              (+ n n))
            (defn double-num [n]
              (add-twice n))
            (double-num 1))
        Number))))

(deftest tc-expr-fn-meta-annot
  (is (subfrm 
        (fn [] 1)
        [-> 1]))
  (is (subfrm 
        (fn [^{:- Long} b]
          b)
        [Long -> Long]))
  (is (let [f 
            ^{:-params [Seqable]}
            #(conj % 1)]
        (subfrm
          f
          [Seqable -> Seqable]))))

(deftest tc-expr-fn-rest-type
  (is (subfrm
        (fn [& ^{:- Long} a]
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

#_(deftest tc-map
  (is (subfrm
        {(get {} 1) 1}
        (Map* [Any 1]))))

(deftest tc-new
  (is (subfrm 
        (Exception. "a")
        Exception)))

(deftest tc-throw
  (is (subfrm
        (throw (Exception. "a"))
        Nothing)))

(deftest tc-case
  (is (subfrm
        (let [a 1]
          (case a
            1 :a
            2 :b))
        Keyword)))

(deftest tc-try
  (is (subfrm
        (try
          1
          (catch Exception e
            2)
          (finally 3))
        (U 1 2))))

(deftest tc-import
  (is (subfrm
        (import 'clojure.lang.IPersistentList)
        Class)))

(deftest tc-vector
  (is (subfrm
        [1 (+ 1 1)]
        (Vector* Number Number))))

(deftest tc-the-var
  (is (subfrm
        #'a
        Var)))

(comment
  (binding [*add-type-ann-fn* (fn [sym type-syn]
                              (add-type-ann sym (parse type-syn)))]
    (require :reload 'typed.base))

(tc-expr (ast (ns a (:require [b]))))
  )
