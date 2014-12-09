(ns clojure.core.typed.test.core
  (:refer-clojure :exclude [update])
  (:require 
    ; this loads the type system, must go first
    [clojure.core.typed.test.test-utils :refer :all]
            [clojure.test :refer :all]
            [clojure.core.typed.analyze-clj :as ana]
            [clojure.repl :refer [pst]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.core.typed.unsafe]
            [clojure.core.typed.init]
            [clojure.core.typed.utils :as u :refer [expr-type]]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.check :as chk]
            [clojure.core.typed.check.funapp :as funapp]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.update :as update :refer [env+ update]]
            [clojure.core.typed.tc-equiv :refer [tc-equiv]]
            [clojure.core.typed.collect-phase :as collect]
            [clojure.core.typed.collect-utils :as collect-u]
            [clojure.core.typed.inst :as inst]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.type-ctors :refer :all]
            [clojure.core.typed.type-rep :refer :all]
            [clojure.core.typed.filter-rep :refer :all]
            [clojure.core.typed.filter-ops :refer :all]
            [clojure.core.typed.object-rep :refer :all]
            [clojure.core.typed.path-rep :refer :all]
            [clojure.core.typed.parse-unparse :refer :all]
            [clojure.core.typed.constant-type :refer [constant-type]]
            [clojure.core.typed.lex-env :refer :all]
            [clojure.core.typed.promote-demote :refer :all]
            [clojure.core.typed.frees :refer :all]
            [clojure.core.typed.free-ops :refer :all]
            [clojure.core.typed.dvar-env :refer :all]
            [clojure.core.typed.cs-gen :refer :all]
            [clojure.core.typed.cs-rep :refer :all]
            [clojure.core.typed.subst :refer [subst-all] :as subst]
            [clojure.core.typed.test.rbt]
            [clojure.core.typed.test.person]
            [clojure.core.typed.internal]
            [clojure.tools.trace :refer [trace-vars untrace-vars
                                         trace-ns untrace-ns]])
; we want clojure.lang.Seqable to be scoped here. 
; The :refer :all of clojure.core.typed adds another Seqable which
; is less useful here.
  (:use [clojure.core.typed :as tc :exclude [Seqable loop fn defprotocol let dotimes
                                             for doseq def remove filter defn atom ref]])
  (:import (clojure.lang ISeq IPersistentVector Atom IPersistentMap
                         ExceptionInfo Var Seqable)))

;Aliases used in unit tests
(defmacro is-with-aliases [& body]
  `(is-clj (do (check-ns '~'clojure.core.typed.test.util-aliases)
               ~@body)))

(defmacro is-tc-e-with-aliases [& body]
  `(is-clj (do (check-ns '~'clojure.core.typed.test.util-aliases)
               (tc-e ~@body)
               true)))

;(check-ns 'clojure.core.typed.test.deftype)

(deftest add-scopes-test
  (is-clj (let [body (make-F 'a)]
        (= (add-scopes 0 body)
           body)))
  (is-clj (let [body (make-F 'a)]
        (= (add-scopes 1 body)
           (Scope-maker body))))
  (is-clj (let [body (make-F 'a)]
        (= (add-scopes 3 body)
           (-> body Scope-maker Scope-maker Scope-maker)))))

(deftest remove-scopes-test
  (is-clj (let [scope (Scope-maker (make-F 'a))]
        (= (remove-scopes 0 scope)
           scope)))
  (is-clj (let [body (make-F 'a)]
        (= (remove-scopes 1 (Scope-maker body))
           body))))

(deftest parse-type-test
  (is-clj (= (Poly-body* '(x) (parse-type '(clojure.core.typed/All [x] x)))
            (make-F 'x)))
  (is-clj (= (Poly-body* '(x y) (parse-type '(clojure.core.typed/All [x y] x)))
             (make-F 'x)))
  (is-clj (= (Poly-body* '(x y) (parse-type '(clojure.core.typed/All [x y] y)))
             (make-F 'y)))
  (is-clj (= (Poly-body* '(a b c d e f g h i) (parse-type '(clojure.core.typed/All [a b c d e f g h i] e)))
             (make-F 'e))))

(deftest parse-type-fn-test
  (is-clj (= (parse-type '[nil * -> nil])
         (make-FnIntersection (make-Function () -nil -nil))))
  (is-clj (= (parse-type '(clojure.core.typed/All [x ...] [nil ... x -> nil]))
         (PolyDots* '(x) [no-bounds]
                    (make-FnIntersection (make-Function () -nil nil (DottedPretype1-maker -nil 'x)))))))

(deftest poly-constructor-test
  (is-clj (= (Poly-body*
           '(x)
           (Poly* '(x) [no-bounds]
                  (make-F 'x)))
         (make-F 'x)))
  (is-clj (= (Poly-body*
           '(x)
           (Poly* '(x)
                  [(Bounds-maker -nil -false nil)]
                  (make-F 'x)))
         (make-F 'x)))
  (is-clj (= (parse-type '(clojure.core.typed/All [x x1 [y :< x] z] [x -> y]))
             (let [no-bounds-scoped (Bounds-maker
                                      (add-scopes 4 -any)
                                      (add-scopes 4 (Un))
                                      nil)]
               (Poly-maker 4
                           [no-bounds-scoped
                            no-bounds-scoped
                            (Bounds-maker 
                              (add-scopes 4 (B-maker 3))
                              (add-scopes 4 (Un))
                              nil)
                            no-bounds-scoped]
                           (add-scopes 4
                                       (make-FnIntersection
                                         (make-Function [(B-maker 3)] (B-maker 1)
                                                        nil nil))))))))








(deftest trans-dots-test
  (is-clj (= (inst/manual-inst (parse-type '(clojure.core.typed/All [x b ...]
                                                 [x ... b -> x]))
                               (map parse-type '(Integer Double Float)))
             (parse-type '[Integer Integer -> Integer])))
  (is-clj (= (inst/manual-inst (parse-type '(clojure.core.typed/All [x b ...]
                                                 [b ... b -> x]))
                               (map parse-type '(Integer Double Float)))
             (parse-type '[Double Float -> Integer])))
  ;map type
  (is-clj (= (inst/manual-inst (parse-type '(clojure.core.typed/All [c a b ...]
                                                 [[a b ... b -> c] (clojure.lang.Seqable a) (clojure.lang.Seqable b) ... b -> (clojure.lang.Seqable c)]))
                               (map parse-type '(Integer Double Float)))
             (parse-type '[[Double Float -> Integer] (clojure.lang.Seqable Double) (clojure.lang.Seqable Float) -> (clojure.lang.Seqable Integer)])))
  (is-clj (= (clj (inst/manual-inst (parse-type '(clojure.core.typed/All [x b ...]
                                                      ['[x b] ... b -> '['[x b] ... b]]))
                                    (map parse-type '(Integer Double Float))))
             (parse-type '['[Integer Double] '[Integer Float] 
                           -> '['[Integer Double] '[Integer Float]]])))
  ;TODO HSequential
  (is-clj (= (clj
               (inst/manual-inst (parse-type `(All [x# b# ~'...]
                                                   [x# ~'... b# :-> (HSequential [x# ~'... b#])]))
                                 (map parse-type `(Integer Double Float))))
             (parse-type `(IFn [Integer Integer :-> (HSequential [Integer Integer])]))))
  ; completeness check
  (is (check-ns 'clojure.core.typed.test.trans-dots))
  )


(deftest tc-invoke-fn-test
  (is-clj (subtype? (ety
                      ((clojure.core.typed/fn> [a :- Number, b :- Number] b)
                       1 2))
                    (parse-type `Number)))
  ; manual instantiation "seq"
  ;FIXME randomly fails. Try again when I/U are sorted sets.
  (is-clj (subtype? (ety
                      ((clojure.core.typed/fn> [a :- (clojure.lang.Seqable Number), b :- Number] 
                                               ((clojure.core.typed/inst seq Number) a))
                       [1 2 1.2] 1))
                    (parse-type `(Option (I (clojure.lang.ISeq java.lang.Number) (CountRange 1))))))
  ; inferred "seq"
  (is-clj (subtype? (ety
                      (clojure.core.typed/fn> [a :- (clojure.lang.Seqable Number), b :- Number] 
                                              1))
                    (make-FnIntersection
                      (make-Function
                        [(RClass-of Seqable [(RClass-of Number nil)]) (RClass-of Number nil)] 
                        (-val 1)
                        nil nil
                        :filter (-FS -top -bot)
                        :object -empty))))
  ; poly inferred "seq"
  ; FIXME pfn> NYI
  #_(is-clj (= (ety
           (clojure.core.typed/pfn> [c] 
                            [[a :- (clojure.lang.Seqable c)] 
                             [b :- Number]] 
                            1))
         (let [x (make-F 'x)]
           (Poly* [(:name x)]
                  [no-bounds]
                  (make-FnIntersection
                    (make-Function
                      [(RClass-of Seqable [x]) (RClass-of Number)] 
                      (-val 1)
                      nil nil
                      :filter (-FS -top -bot)
                      :object -empty))))))
  ;test invoke fn
  (is-clj (subtype? (ety
                      ((clojure.core.typed/fn> [a :- (clojure.lang.Seqable Number), b :- Number] 
                                               (seq a))
                       [1 2 1.2] 1))
                    (parse-type `(U nil (I (CountRange 1) (clojure.lang.ISeq Number))))))
  (is-clj (subtype? (ety
                      ((clojure.core.typed/fn> [a :- (clojure.lang.IPersistentMap clojure.core.typed/Any Number), b :- Number] 
                                               ((clojure.core.typed/inst get Number clojure.core.typed/Nothing) a b))
                       (zipmap [1] [2]) 1))
                    (parse-type `(U nil Number)))))

;FIXME
;(deftest get-special-test
;  (is-clj (subtype? 
;            (ety 
;              (clojure.core.typed/fn> [a :- (HMap :mandatory {:a Number})]
;                                      (get a :a)))
;            (parse-type
;              '(Fn ['{:a java.lang.Number} -> java.lang.Number 
;                    :filters {:then (& (is (HMap :mandatory {:a Number}) 0)
;                                       (is java.lang.Number 0 [(Key :a)]))
;                              :else (| (is (U nil false) 0 [(Key :a)])
;                                       (is (HMap :absent-keys #{:a}) 0) )} 
;                    :object {:path [(Key :a)], :id 0}])))))

(deftest truth-false-values-test
  (is-clj (= (tc-t (if nil 1 2))
         (ret (-val 2) (-FS -top -bot) (EmptyObject-maker))))
  (is-clj (= (tc-t (if false 1 2))
         (ret (-val 2) (-FS -top -bot) (EmptyObject-maker))))
  (is-clj (= (tc-t (if 1 1 2))
         (ret (-val 1) (-FS -top -bot) (EmptyObject-maker)))))

(deftest empty-fn-test
  (is-clj (= (tc-t (clojure.core/fn []))
         (ret (make-FnIntersection
                (Function-maker [] (make-Result -nil
                                            (-FS -bot -top)
                                            (EmptyObject-maker))
                            nil nil nil))
              (-FS -top -bot)
              (EmptyObject-maker))))
  (is-clj (= (tc-t (fn [] 1))
         (ret (make-FnIntersection
                (Function-maker [] (make-Result (-val 1)
                                              (-FS -top -bot)
                                              (EmptyObject-maker))
                              nil nil nil))
              (-FS -top -bot)
              (EmptyObject-maker))))
  (is-clj (= (tc-t (let []))
         (ret -nil (-FS -bot -top) (EmptyObject-maker)))))

(deftest path-test
  (is-clj (= (tc-t (fn [a] (let [a 1] a)))
         (ret (make-FnIntersection
                (Function-maker [-any]
                              (make-Result (-val 1)
                                           (-FS -top -top)
                                           -empty)
                              nil nil nil))
              (-FS -top -bot) -empty)))
  (is-clj (= (tc-t (let [a nil] a))
         (ret -nil (-FS -top -top) -empty))))

(deftest equiv-test
  ; 1 arity :else filter is always bot
  (is-clj (= (tc-t (= 1))
         (ret (Un -true -false) (-FS -top -bot) -empty)))
  (is-clj (= (tc-t (= 1 1))
         (tc-t (= 1 1 1 1 1 1 1 1 1 1))
         (ret (Un -true -false) (-FS -top -top) -empty)))
  (is-clj (= (tc-t (= 'a 'b))
         (tc-t (= 1 2))
         (tc-t (= :a :b))
         (tc-t (= :a 1 'a))
         (ret (Un -true -false) (-FS -top -top) -empty)))
  (is-clj (= (tc-t (= :Val (-> {:a :Val} :a)))
         (ret (Un -true -false) (-FS -top -top) -empty))))

(deftest name-to-param-index-test
  ;a => 0
  (is-clj 
    (= (tc-t 
         (clojure.core.typed/fn> [a :- (U (HMap :mandatory {:op (Value :if)})
                                          (HMap :mandatory {:op (Value :var)}))] 
                                 (:op a)))
       (clj
         (ret 
           (parse-type 
             `(IFn [(U '{:op (Value :var)} '{:op (Value :if)}) :-> (U ':var ':if) 
                    :filters {:then (~'is (U (Value :var) (Value :if)) 0 [(~'Key :op)]), 
                              :else (~'| (~'is (HMap :absent-keys #{:op}) 0) 
                                         (~'is (U nil false) 0 [(~'Key :op)]))} 
                    :object {:path [(~'Key :op)], :id 0}]))
           (-FS -top -bot)
           -empty)))))

(deftest refine-test
  (is-clj (= (tc-t 
           (clojure.core.typed/fn> [a :- (U (HMap :mandatory {:op (Value :if)})
                                            (HMap :mandatory {:op (Value :var)}))]
                           (when (= (:op a) :if) 
                             a)))
         (ret (make-FnIntersection
                (Function-maker
                    [(Un (make-HMap :mandatory {(-val :op) (-val :if)})
                         (make-HMap :mandatory {(-val :op) (-val :var)}))]
                    (make-Result (Un -nil (make-HMap :mandatory {(-val :op) (-val :if)}))
                                 (-FS (-and (-filter (-val :if) 0 [(-kpe :op)])
                                            (-not-filter (Un -false -nil) 0)
                                            (-filter (make-HMap :mandatory {(-val :op) (-val :if)}) 0))
                                           ; what are these filters doing here?
                                      (-or (-and (-filter (-val :if) 0 [(-kpe :op)])
                                                 (-filter (Un -false -nil) 0))
                                           (-not-filter (-val :if) 0 [(-kpe :op)])))
                                 -empty)
                    nil nil nil))
              (-FS -top -bot)
              -empty))))


#_(deftest dotted-infer-test
  (is-cf (map number? [1])))

(deftest check-invoke
  ; wrap in thunk to prevent evaluation (analyzer currently evaluates forms)
  (is (err/top-level-error-thrown? (cf (fn [] (symbol "a" 'b)))))
  (is (both-subtype? (ety (symbol "a" "a"))
                     (clj (RClass-of clojure.lang.Symbol)))))

(deftest check-do-test
  (is-clj (= (ety (do 1 2))
         (-val 2))))

(deftest tc-var-test
  (is-clj (subtype? (ret-t (tc-t seq?))
                    (parse-type `(Pred (Seq Any))))))

(deftest heterogeneous-ds-test
  (is-clj 
    (not (subtype? (parse-type `(HMap :mandatory {:a (Value 1)}))
                   (RClass-of ISeq [-any]))))
  (is-clj 
    (not (subtype? (parse-type `(HVec [(Value 1) (Value 2)]))
                   (RClass-of ISeq [-any]))))
  (is-clj
    (subtype? (parse-type `(HSeq [(Value 1) (Value 2)]))
              (RClass-of ISeq [-any])))
  (is-clj 
    (subtype? (parse-type `(~'List* (Value 1) (Value 2)))
              (RClass-of ISeq [-any])))
  (is-clj (= (tc-t [1 2])
             (ret (-hvec [(-val 1) (-val 2)]) (-true-filter) -empty)))
  (is-clj (= (tc-t '(1 2))
         (ret (HeterogeneousList-maker [(-val 1) (-val 2)]) (-true-filter) -empty)))
  (is-clj (= (tc-t {:a 1})
         (ret (-complete-hmap {(-val :a) (-val 1)}) (-true-filter) -empty)))
  (is-clj (= (tc-t {})
         (ret (-complete-hmap {}) (-true-filter) -empty)))
  (is-clj (= (tc-t [])
         (ret (-hvec []) (-true-filter) -empty)))
  (is-clj (= (tc-t '())
         (ret (HeterogeneousList-maker []) (-true-filter) -empty)))
  (is-cf '(a b) (List* clojure.lang.Symbol clojure.lang.Symbol)))

(deftest implied-atomic?-test
  (is-clj (implied-atomic? (-not-filter -false 'a)(-not-filter (Un -nil -false) 'a))))

(deftest combine-props-test
  (is-clj (= (map set (update/combine-props [(ImpFilter-maker (-not-filter -false 'a)
                                               (-filter -true 'b))]
                                 [(-not-filter (Un -nil -false) 'a)]
                                 (atom true)))
         [#{} #{(-not-filter (Un -nil -false) 'a)
                (-filter -true 'b)}])))

(deftest env+-test
  ;test basic TypeFilter
  ;update a from clojure.core.typed/Any to (Value :a)
  (is-clj (let [props [(-filter (-val :a) 'a)]
            flag (atom true)]
        (and (= (let [env {'a -any}
                      lenv (-PropEnv env props)]
                  (env+ lenv [] flag))
                (-PropEnv {'a (-val :a)} props))
             @flag)))
  ;test positive KeyPE
  ;update a from (U (HMap :mandatory {:op :if}) (HMap :mandatory {:op :var})) => (HMap :mandatory {:op :if})
  (is-clj (let [props [(-filter (-val :if) 'a [(-kpe :op)])]
            flag (atom true)]
        (and (= (let [env {'a (Un (make-HMap :mandatory {(-val :op) (-val :if)})
                                  (make-HMap :mandatory {(-val :op) (-val :var)}))}
                      lenv (-PropEnv env props)]
                  (env+ lenv [] flag))
                (-PropEnv {'a (make-HMap :mandatory {(-val :op) (-val :if)})} props))
             @flag)))
  ;test negative KeyPE
  (is-clj (let [props [(-not-filter (-val :if) 'a [(-kpe :op)])]
            flag (atom true)]
        (and (= (let [env {'a (Un (make-HMap :mandatory {(-val :op) (-val :if)})
                                  (make-HMap :mandatory {(-val :op) (-val :var)}))}
                      lenv (-PropEnv env props)]
                  (env+ lenv [] flag))
                (-PropEnv {'a (make-HMap :mandatory {(-val :op) (-val :var)})} props))
             @flag)))
  ;test impfilter
  (is-clj (let [{:keys [l props]}
            (env+ (-PropEnv {'a (Un -false -true) 'b (Un -nil -true)}
                             [(ImpFilter-maker (-not-filter -false 'a)
                                           (-filter -true 'b))])
                  [(-not-filter (Un -nil -false) 'a)]
                  (atom true))]
        (and (= l {'a -true, 'b -true})
             (= (set props)
                #{(-not-filter (Un -nil -false) 'a)
                  (-filter -true 'b)}))))
  ; more complex impfilter
  (is-with-aliases (= (env+ (-PropEnv {'and1 (Un -false -true)
                                       'tmap (Name-maker 'clojure.core.typed.test.util-aliases/UnionName)}
                                      [(ImpFilter-maker (-filter (Un -nil -false) 'and1)
                                                    (-not-filter (-val :MapStruct1)
                                                                 'tmap
                                                                 [(-kpe :type)]))
                                       (ImpFilter-maker (-not-filter (Un -nil -false) 'and1)
                                                    (-filter (-val :MapStruct1)
                                                             'tmap
                                                             [(-kpe :type)]))])
                            [(-filter (Un -nil -false) 'and1)]
                            (atom true))))
  ; refine a subtype
  (is-clj (= (:l (env+ (-PropEnv {'and1 (RClass-of Seqable [-any])} [])
                       [(-filter (RClass-of IPersistentVector [-any]) 'and1)]
                       (atom true)))
             {'and1 (RClass-of IPersistentVector [-any])}))
  ; bottom preserved
  (is-clj (let [a (atom true)]
        (env+ (-PropEnv {'foo -any} []) [-bot] a)
        (false? @a))))

;FIXME all these tests relate to CTYP-24
(deftest destructuring-special-ops
  ;FIXME for destructuring rest args
;  (is-clj (= (tc-t (let [a '(a b)]
;                 (seq? a)))
;         (ret -true (-true-filter) -empty)))
  (is-clj (= (-> 
           (tc-t (let [a {:a 1}]
                   (if (seq? a)
                     (apply hash-map a)
                     a)))
           ret-t)
         (-complete-hmap {(-val :a) (-val 1)})))
  (is-clj (= (tc-t (clojure.core.typed/fn> [{a :a} :- (HMap :mandatory {:a (Value 1)})]
                               a))
         (ret (make-FnIntersection 
                (Function-maker [(make-HMap :mandatory {(-val :a) (-val 1)})]
                              (make-Result (-val 1) 
                                           (-FS -top -top)  ; have to throw out filters whos id's go out of scope
                                           ;(-path [(-kpe :a)] 0) ; requires 'equivalence' filters
                                           -empty)
                              nil nil nil))
              (-FS -top -bot)
              -empty)))
  ;FIXME inferred filters are bit messy, but should be (-FS -bot (! Seq 0))
  #_(is-with-aliases (= (-> (tc-t (clojure.core.typed/fn> [a :- clojure.core.typed.test.util-aliases/UnionName]
                                   (seq? a)))
           ret-t)
         (make-FnIntersection
           (Function-maker [(Name-maker 'clojure.core.typed.test.util-aliases/UnionName)]
                         (make-Result -false 
                                      ;FIXME why isn't this (-FS -bot (-not-filter (RClass-of ISeq [-any]) 0)) ?
                                      (-FS -bot -top)
                                      -empty)
                         nil nil nil))))
  (is-clj (= (tc-t (let [{a :a} {:a 1}]
                 a))
         (ret (-val 1) 
              (-FS -top -top) ; a goes out of scope, throw out filters
              -empty)))
  ;FIXME should be (-FS -bot (! ISeq 0))
  #_(is-clj (= (tc-t (clojure.core.typed/fn> [a :- (HMap :mandatory {:a (Value 1)})]
                               (seq? a)))
         (ret (make-FnIntersection
                (Function-maker [(make-HMap :mandatory {(-val :a) (-val 1)})]
                              (make-Result -false (-false-filter) -empty)
                              nil nil nil))
              (-FS -top -bot)
              -empty)))
  ;roughly the macroexpansion of map destructuring
  ;FIXME
  #_(is-clj (= (tc-t (clojure.core.typed/fn> 
                 [map-param :- clojure.core.typed.test.rbt-types/badRight]
                 (when (and (= :Black (-> map-param :tree))
                            (= :Red (-> map-param :left :tree))
                            (= :Red (-> map-param :left :right :tree)))
                   (let [map1 map-param
                         map1
                         (if (clojure.core/seq? map1)
                           (clojure.core/apply clojure.core/hash-map map1)
                           map1)

                         mapr (clojure.core/get map1 :right)
                         mapr
                         (if (clojure.core/seq? mapr)
                           (clojure.core/apply clojure.core/hash-map mapr)
                           mapr)

                         maprl (clojure.core/get mapr :left)
                         ;_ (print-env "maprl")
                         maprl
                         (if (clojure.core/seq? maprl)
                           (clojure.core/apply clojure.core/hash-map maprl)
                           maprl)]
                     maprl))))))
  ;destructuring a variable of union type
  (is-clj (= (->
           (tc-t (clojure.core.typed/fn> [{a :a} :- (U (HMap :mandatory {:a (Value 1)})
                                                       (HMap :mandatory {:b (Value 2)}))]
                                         a))
           ret-t)
         (make-FnIntersection 
           (make-Function [(Un (make-HMap :mandatory {(-val :a) (-val 1)})
                               (make-HMap :mandatory {(-val :b) (-val 2)}))]
                          (Un (-val 1) -any))))))

(deftest Name-resolve-test
  (is-with-aliases (= (tc-t (clojure.core.typed/fn> [tmap :- clojure.core.typed.test.util-aliases/MyName]
                                                    ;call to (apply hash-map tmap) should be eliminated
                                                    (let [{e :a} tmap]
                                                      e)))
                      (ret (make-FnIntersection 
                             (Function-maker [(Name-maker 'clojure.core.typed.test.util-aliases/MyName)]
                                         (make-Result (-val 1) (-FS -top -top) -empty)
                                         nil nil nil))
                           (-FS -top -bot) -empty)))
  (is-with-aliases (= (tc-t (clojure.core.typed/fn> [tmap :- clojure.core.typed.test.util-aliases/MapName]
                                                    (let [{e :a} tmap]
                                                      (assoc e :c :b))))
                      (ret (make-FnIntersection (Function-maker [(Name-maker 'clojure.core.typed.test.util-aliases/MapName)]
                                                            (make-Result (make-HMap :mandatory {(-val :a) (-val 1)
                                                                                 (-val :c) (-val :b)})
                                                                         (-FS -top -bot) -empty)
                                                            nil nil nil))
                           (-FS -top -bot) -empty)))
  ; Name representing union of two maps, both with :type key
  (is-with-aliases (subtype? 
                     (-> (tc-t (clojure.core.typed/fn> [tmap :- clojure.core.typed.test.util-aliases/UnionName]
                                                       (:type tmap)))
                         ret-t)
                     (parse-type 
                       `[clojure.core.typed.test.util-aliases/UnionName :-> (U (Value :MapStruct2)
                                                                               (Value :MapStruct1))])))
  ; using = to derive paths
  (is-with-aliases (subtype? 
                     (-> (tc-t (clojure.core.typed/fn> [tmap :- clojure.core.typed.test.util-aliases/UnionName]
                                                       (= :MapStruct1 (:type tmap))))
                         ret-t)
                     (make-FnIntersection 
                       (make-Function 
                         [(Name-maker 'clojure.core.typed.test.util-aliases/UnionName)]
                         (Un -false -true)
                         nil nil
                         :filter (let [t (-val :MapStruct1)
                                       path [(-kpe :type)]]
                                   (-FS (-and 
                                          (-filter (make-HMap :mandatory {(-val :type) (-val :MapStruct1)
                                                           (-val :a) (Name-maker 'clojure.core.typed.test.util-aliases/MyName)})
                                                   0)
                                          (-filter (-val :MapStruct1) 0 path)
                                          (-filter t 0 path))
                                        (-not-filter t 0 path)))))))
  ; using filters derived by =
  (is-with-aliases (subtype? (-> (tc-t (clojure.core.typed/fn> [tmap :- clojure.core.typed.test.util-aliases/UnionName]
                                                               (if (= :MapStruct1 (:type tmap))
                                                                 (:a tmap)
                                                                 (:b tmap))))
                                 ret-t)
                             (parse-type 
                               `[clojure.core.typed.test.util-aliases/UnionName :-> clojure.core.typed.test.util-aliases/MyName])))
  ; following paths with test of conjuncts
  ;FIXME
  #_(is-clj (= (tc-t (clojure.core.typed/fn> [tmap :- clojure.core.typed.test.util-aliases/UnionName]
                                         ; (and (= :MapStruct1 (-> tmap :type))
                               ;      (= 1 1))
                               (if (clojure.core.typed/print-filterset "final filters"
                                    (let [and1 (clojure.core.typed/print-filterset "first and1"
                                                 (= :MapStruct1 (-> tmap :type)))]
                                      (clojure.core.typed/print-env "first conjunct")
                                      (clojure.core.typed/print-filterset "second and1"
                                        (if (clojure.core.typed/print-filterset "second test"
                                              and1)
                                          (do (clojure.core.typed/print-env "second conjunct")
                                            (clojure.core.typed/print-filterset "third and1"
                                              (= 1 1)))
                                          (do (clojure.core.typed/print-env "fail conjunct")
                                            (clojure.core.typed/print-filterset "fail and1"
                                              and1))))))
                                 (do (clojure.core.typed/print-env "follow then")
                                   (assoc tmap :c :d))
                                 1)))
         (ret (make-FnIntersection (Function-maker [(Name-maker 'clojure.core.typed.test.util-aliases/UnionName)]
                              (let [t (Un (-val 1)
                                          (make-HMap :mandatory {(-val :type) (-val :MapStruct1)
                                                               (-val :c) (-val :d)
                                                               (-val :a) (Name-maker 'clojure.core.typed.test.util-aliases/MyName)}))]
                                (make-Result t (-FS -top -bot) -empty))
                              nil nil nil))
              (-FS -top -bot) -empty))))

;(tc-t (clojure.core.typed/fn> [[a :- Number]
;                       [b :- Number]]
;                      (if (= a 1)
;                        a
;                        )))
;
;(-> (tc-t (clojure.core.typed/fn> [[tmap :- clojure.core.typed.test.core/UnionName]]
;                          (= :MapStruct1 (-> tmap :type))
;                          (= 1 (-> tmap :a :a))))
;  :t :types first :rng :fl unparse-filter-set pprint)

;(->
;  (tc-t (clojure.core.typed/fn> [[a :- Number]
;                         [b :- Number]]
;;           (-FS (-and (-filter (-val 1) 'a)
;;                      (-filter (-val 2) 'b))
;;                (-or (-not-filter (-val 1) 'a)
;;                     (-not-filter (-val 2) 'b)
;;                     (-and (-not-filter (-val 1) 'a)
;;                           (-not-filter (-val 2) 'b))))
;          (clojure.core.typed/tc-pr-filters "final filters"
;            (let [and1 (clojure.core.typed/tc-pr-filters "first and1"
;                         (= a 1))]
;              (clojure.core.typed/tc-pr-env "first conjunct")
;;              (-FS (-and (-not-filter (Un -false -nil) and1)
;;                         (-filter (-val 2) 'b))
;;                   (-or (-filter (Un -false -nil) and1)
;;                        (-not-filter (-val 2) 'b)))
;              (clojure.core.typed/tc-pr-filters "second and1"
;                (if (clojure.core.typed/tc-pr-filters "second test"
;                      and1)
;                  (do (clojure.core.typed/tc-pr-env "second conjunct")
;                    (clojure.core.typed/tc-pr-filters "third and1"
;                      (= b 2)))
;                  (do (clojure.core.typed/tc-pr-env "fail conjunct")
;                    (clojure.core.typed/tc-pr-filters "fail and1"
;                      and1))))))))
;  :t :types first :rng :fl unparse-filter-set pprint)

(deftest update-test
  (is-clj (= (update (Un (make-HMap :mandatory {(-val :type) (-val :Map1)})
                         (make-HMap :mandatory {(-val :type) (-val :Map2)}))
                     (-filter (-val :Map1) 'tmap [(-kpe :type)]))
             (make-HMap :mandatory {(-val :type) (-val :Map1)})))
  ;test that update resolves Names properly
  (is-with-aliases (= (update (Name-maker 'clojure.core.typed.test.util-aliases/MapStruct2)
                              (-filter (-val :MapStruct1) 'tmap [(-kpe :type)]))
                      (Un)))
  ;test that update resolves Names properly
  ; here we refine the type of tmap with the equivalent of following the then branch 
  ; with test (= :MapStruct1 (:type tmap))
  (is-with-aliases (= (update (Name-maker 'clojure.core.typed.test.util-aliases/UnionName)
                              (-filter (-val :MapStruct1) 'tmap [(-kpe :type)]))
                      (make-HMap :mandatory {(-val :type) (-val :MapStruct1) 
                              (-val :a) (Name-maker 'clojure.core.typed.test.util-aliases/MyName)})))
  (is-with-aliases (= (update (Name-maker 'clojure.core.typed.test.util-aliases/UnionName)
                              (-not-filter (-val :MapStruct1) 'tmap [(-kpe :type)]))
                      (make-HMap :mandatory {(-val :type) (-val :MapStruct2) 
                              (-val :b) (Name-maker 'clojure.core.typed.test.util-aliases/MyName)})))
  (is-clj (= (update (Un -true -false) (-filter (Un -false -nil) 'a nil)) 
             -false)))

#_(def-alias SomeMap (U (HMap :mandatory {:a (Value :b)})
                      (HMap :mandatory {:b (Value :c)})))

(deftest assoc-test
  (is-clj (= (tc-t (assoc {} :a :b))
         (ret (-complete-hmap {(-val :a) (-val :b)})
              (-FS -top -bot)
              -empty)))
  ;see `invoke-special` for assoc for TODO
  ;FIXME
  #_(is-clj (= (-> (tc-t (-> (fn [m]
                         (assoc m :c 1))
                     (clojure.core.typed/ann-form [clojure.core.typed.test.core/SomeMap -> (U '{:a ':b :c '1}
                                                                         '{:b ':c :c '1})])))
           ret-t :types first :rng)
         (make-Result (Un (make-HMap :mandatory {(-val :a) (-val :b)
                                  (-val :c) (-val 1)})
                          (make-HMap :mandatory {(-val :b) (-val :c)
                                  (-val :c) (-val 1)}))
                      (-FS -top -bot)
                      -empty))))
         


(deftest check-get-keyword-invoke-test
  ;truth valued key
  (is-clj (= (tc-t (let [a {:a 1}]
                     (:a a)))
             (ret (-val 1) (-FS -top -top) -empty)))
  ;false valued key, a bit conservative in filters for now
  (is-clj (= (tc-t (let [a {:a nil}]
                     (:a a)))
             (ret -nil (-FS -top -top) -empty)))
  ;multiple levels
  (is-clj (= (tc-t (let [a {:c {:a :b}}]
                     (-> a :c :a)))
             (ret (-val :b) (-FS -top -top) -empty)))
  (is-clj (= (tc-t (clojure.core/get {:a 1} :a))
             (tc-t (clojure.lang.RT/get {:a 1} :a))
             ;FIXME
             #_(tc-t ({:a 1} :a))
             (tc-t (:a {:a 1}))
             (ret (-val 1)
                  (-FS -top -top)
                  -empty)))
  ;keyword-invoke with default
  (is-tc-e (:a (ann-form {} (HMap :optional {:a Number}))
               'a)
           (U Number Symbol)))

(defn print-cset [cs]
  (into {} (doall
             (for [ms (:maps cs)
                   [k v] (:fixed ms)]
               [k
                [(str (unparse-type (:S v))
                      " << "
                      (:X v)
                      " << "
                      (unparse-type (:T v)))]]))))

(deftest promote-demote-test
  (is-clj (= (promote-var (make-F 'x) '#{x})
         -any))
  (is-clj (= (demote-var (make-F 'x) '#{x})
         (Bottom)))
  (is-clj (= (promote-var (RClass-of clojure.lang.ISeq [(make-F 'x)]) '#{x})
         (RClass-of clojure.lang.ISeq [-any])))
  (is-clj (= (demote-var (RClass-of clojure.lang.ISeq [(make-F 'x)]) '#{x})
             (RClass-of clojure.lang.ISeq [(Bottom)]))))

(deftest variances-test
  (is-clj (= (fv-variances (make-F 'x))
         '{x :covariant}))
  (is-clj (= (fv-variances -any)
         '{}))
  (is-clj (= (fv-variances 
           (make-Function [] (RClass-of Atom [(make-F 'a) (make-F 'a)])))
         '{a :invariant}))
  (is-clj (= (fv-variances 
           (make-Function [] (RClass-of Atom [-any (make-F 'a)])))
         '{a :covariant}))
  (is-clj (= (fv-variances 
           (make-Function [] (RClass-of Atom [(make-F 'a) -any])))
         '{a :contravariant})))


(deftest fv-test
  (is-clj (= (fv (make-F 'x))
         '#{x})))

(deftest fi-test
  (is-clj (empty? (fi (make-F 'x)))))

(deftest cs-gen-test
  (is-clj (= (cs-gen #{} ;V
                     (zipmap '[x y] (repeat no-bounds)) ;X
                     {} ;Y
                     (-val 1) ;S
                     (make-F 'x)) ;T
             (->cset [(make-cset-entry {'x (->c (-val 1) 'x -any no-bounds)
                                        'y (->c (Un) 'y -any no-bounds)})])))
  ;intersections correctly inferred
  (is-clj (= (cs-gen '#{} {'x no-bounds} '{} 
                     (-hvec [(RClass-of Number)])
                     (In (RClass-of Seqable [(make-F 'x)]) (make-CountRange 1)))
             (->cset [(make-cset-entry {'x (->c (RClass-of Number) 'x -any no-bounds)})])))
;correct RClass ancestor inference
  (is-clj (= (cs-gen #{} {'x no-bounds} {} 
                     (RClass-of IPersistentVector [(RClass-of Number)])
                     (RClass-of Seqable [(make-F 'x)]))
             (->cset [(make-cset-entry {'x (->c (RClass-of Number) 'x -any no-bounds)})]))))

(deftest subst-gen-test
  (let [cs (clj (cs-gen #{} ;V
                        (zipmap '[x y] (repeat no-bounds)) ;X
                        {} ;Y
                        (-val 1) ;S
                        (make-F 'x)))]
    (is-clj (= (subst-gen cs #{} (make-F 'x))
           {'x (->t-subst (-val 1) no-bounds)
            'y (->t-subst (Un) no-bounds)}))))

(deftest infer-test
  (is-clj (clj
        (= (infer (zipmap '[x y] (repeat no-bounds)) ;tv env
                  {}
                  [(-val 1) (-val 2)] ;actual
                  [(make-F 'x) (make-F 'y)] ;expected
                  (make-F 'x))))) ;result
  (is-clj (clj 
        (= (infer {'x no-bounds} ;tv env
                  {}
                  [(RClass-of IPersistentVector [(Un (-val 1) (-val 2) (-val 3))])] ;actual
                  [(RClass-of Seqable [(make-F 'x)])] ;expected
                  (RClass-of clojure.lang.ASeq [(make-F 'x)]))))) ;result
  (is-clj (clj
        (= (infer {'x no-bounds} ;tv env
                  {}
                  [(-hvec [(-val 1) (-val 2) (-val 3)])] ;actual
                  [(RClass-of Seqable [(make-F 'x)])] ;expected
                  (RClass-of clojure.lang.ASeq [(make-F 'x)])))))) ;result

(deftest arith-test
  (is-clj (subtype? (:t (tc-t (+)))
                (RClass-of Number)))
  (is-clj (subtype? (:t (tc-t (+ 1 2)))
                (RClass-of Number)))
  ;wrap in thunks to prevent evaluation
  (is (err/top-level-error-thrown? (cf (fn [] (+ 1 2 "a")))))
  (is (err/top-level-error-thrown? (cf (fn [] (-)))))
  (is (err/top-level-error-thrown? (cf (fn [] (/))))))

(deftest tc-constructor-test
  (is-clj (= (tc-t (Exception. "a"))
         (ret (RClass-of Exception)
              (-FS -top -bot)
              (EmptyObject-maker)))))

(deftest tc-throw-test
  (is-clj (subtype? (:t (tc-t (fn [] (throw (Exception. "a")))))
                (make-FnIntersection
                  (make-Function [] (Un))))))

(deftest first-seq-test
  (is-clj (clj (subtype? (ret-t (tc-t (first [1 1 1])))
                     (Un -nil (RClass-of Number)))))
  (is-clj (subtype? (In (RClass-of clojure.lang.PersistentList [-any])
                    (make-CountRange 1))
                (In (RClass-of Seqable [-any])
                    (make-CountRange 1))))
  (is-clj (subtype? (ret-t (tc-t (let [l [1 2 3]]
                               (if (seq l)
                                 (first l)
                                 (throw (Exception. "Error"))))))
                (RClass-of Number)))
  (is-clj (= (tc-t (first [1]))
         (ret (-val 1))))
  (is-clj (= (tc-t (first []))
         (ret -nil)))
  (is-clj (subtype? (ret-t (tc-t (first [1 2 3])))
                (RClass-of Number))))

(deftest intersection-maker-test
  (is-clj (= (In -nil (-val 1))
         (Un)))
  (is-clj (clj 
        (= (In (RClass-of Seqable [-any])
               -nil)
           (Un)))))
;FIXME
;  (is-clj (= (In (RClass-of Number)
;             (RClass-of Symbol))
;         (Un)))
;  (is-clj (= (In (RClass-of clojure.lang.APersistentMap [-any -any])
;             (RClass-of clojure.lang.Sorted))
;         (make-Intersection 
;           #{(RClass-of clojure.lang.APersistentMap [-any -any])
;             (RClass-of clojure.lang.Sorted)}))))
;



(deftest names-expansion-test
  (is (do
        (tc-e (defalias clojure.core.typed.test.core/MyAlias
                (U nil (HMap :mandatory {:a Number}))))
        (clj
          (subtype? (Name-maker 'clojure.core.typed.test.core/MyAlias)
                    -any)))))

(deftest ccfind-test
  (is-clj (clj
        (subtype? (-> (tc-t (clojure.core.typed/fn> [a :- (clojure.lang.IPersistentMap Long String)]
                                                    (find a 1)))
                      :t :types first :rng :t)
                  (Un (-hvec [(RClass-of Long) (RClass-of String)])
                      -nil)))))

(deftest map-infer-test
  (is-clj (subtype? (ret-t (tc-t (map + [1 2])))
                (RClass-of Seqable [(RClass-of Number)])))
  (is-clj (subtype? (ret-t (tc-t (map + [1 2] [1 2] [4 5] [6 7] [4 4] [3 4])))
                (RClass-of Seqable [(RClass-of Number)])))
  (is (err/top-level-error-thrown? (cf (map + [1 2] [1 2] [4 5] [6 7] [4 4] {3 4}))))
  (is (err/top-level-error-thrown? (cf (map + [1 2] [1 2] [4 5] [6 7] [4 4] #{'a 4})))))

(deftest ann-form-test
  (is-clj (= (ret-t (tc-t 
                      (clojure.core.typed/ann-form (atom 1)
                                                   (clojure.lang.Atom Number Number))))
             (parse-type '(clojure.lang.Atom Number Number)))))

(deftest atom-ops-test
  (is-clj (subtype? (ret-t (tc-t
                         (reset!
                           (clojure.core.typed/ann-form (atom 1) 
                                                (clojure.lang.Atom Number Number))
                           10.1)))
                (RClass-of Number))))

(deftest apply-test
  ;conservative while not tracking keys "not" in a hmap
  (is-clj (clj
        (subtype? (ret-t (tc-t (apply merge [{:a 1}])))
                  (Un -nil (RClass-of IPersistentMap [-any -any]))))))

(deftest destructuring-test
  ;Vector destructuring with :as
  (is-clj (subtype? (ret-t (tc-t (let [[a b :as c] (clojure.core.typed/ann-form [1 2] (clojure.core.typed/Vec Number))] 
                                   [a b c])))
                    (-hvec [(Un -nil (RClass-of Number))
                            (Un -nil (RClass-of Number))
                            (RClass-of Seqable [(RClass-of Number)])])))
  (is-clj (= (ret-t (tc-t (let [[a b :as c] [1 2]] 
                        [a b c])))
         (-hvec [(-val 1)
                 (-val 2)
                 (-hvec [(-val 1) (-val 2)])])))
  ;Map destructuring of vector
  ;FIXME needs implementing, but gives a decent error msg
  #_(is-clj (= (ret-t (tc-t (let [{a 0 b 1 :as c} [1 2]] 
                        [a b c])))
         (-hvec [(-val 1)
                 (-val 2)
                 (-hvec [(-val 1) (-val 2)])]))))

(deftest vararg-subtyping-test
  (is-clj (subtype? (parse-type '[nil * -> nil])
                (parse-type '[nil -> nil])))
  (is-cf (clojure.core.typed/ann-form (clojure.core.typed/inst merge clojure.core.typed/Any clojure.core.typed/Any) [nil -> nil])))

(deftest poly-filter-test
  (is-clj (both-subtype? 
            (ret-t (tc-t (let [a (clojure.core.typed/ann-form [1] (clojure.core.typed/Coll clojure.core.typed/AnyInteger))]
                           (if (seq a)
                             (first a)
                             'a))))
            (parse-type `(U AnyInteger (Value ~'a))))))

(deftest type-fn-test 
  (is-clj (clj
            (= (with-bounded-frees {(make-F 'm) (-bounds (parse-type `(TFn [[~'x :variance :covariant]] Any))
                                                         (parse-type `(TFn [[~'x :variance :covariant]] Nothing)) )}
                 (funapp/check-funapp
                   (ana/ast-for-form ''a) ;dummy
                   [(ana/ast-for-form 1)];dummy
                   (ret (parse-type 
                          `(All [~'x]
                                  [~'x :-> (~'m ~'x)])))
                   [(ret -nil)]
                   nil))
               (ret (TApp-maker (make-F 'm) [-nil]))))))

;TODO how to handle casts. CTYP-12
;Also need tc-t to bind *delayed-errors*
#_(deftest prims-test
  (is-clj (= (ret-t (tc-t (Math/sqrt 1)))
         (parse-type 'double))))

(deftest hmap-subtype
  (is-cf {} (clojure.lang.APersistentMap clojure.core.typed/Any clojure.core.typed/Any)))

;; `do` is special at the top level, tc-ignore should expand out to `do`
(tc-ignore
 (defprotocol some-proto (some-proto-method [_]))
 some-proto-method)

(deftest array-test
  (is-clj (= (Class/forName "[I") 
         (class (into-array> int [1]))))
  (is-clj (clj (= (Class/forName "[Ljava.lang.Object;") 
              (class (into-array> (clojure.core.typed/U nil int) [1])))))
  (is-clj (clj (= (Class/forName "[Ljava.lang.Number;") 
              (class (into-array> (clojure.core.typed/U nil Number) [1])))))
  (is-clj (clj (= (Class/forName "[Ljava.lang.Object;") 
              (class (into-array> (clojure.core.typed/U clojure.lang.Symbol Number) [1])))))
  (is-clj (= (Class/forName "[Ljava.lang.Object;") 
         (class (into-array> Object (clojure.core.typed/U clojure.lang.Symbol Number) [1]))))
  )

(deftest class-pathelem-test
  (is-clj (= (-> (tc-t #(class %))
           ret-t :types first :rng Result-object*)
         (-path [(ClassPE-maker)] 0)))
  (is-clj (subtype? (-> (tc-t 
                      #(= Number (class %)))
                  ret-t)
                (FnIntersection-maker
                  [(make-Function
                     [-any]
                     (RClass-of 'boolean)
                     nil nil
                     :filter (-FS (-and (-filter (-val Number) 0 [(ClassPE-maker)])
                                        ;the important filter, updates first argument to be Number if predicate is true
                                        (-filter (RClass-of Number) 0))
                                  (-not-filter (-val Number) 0 [(ClassPE-maker)])))]))))

;TODO ^--
;(-> (tc-t #(let [a (class %)]
;             (if a
;               true
;               false)))
;  ret-t unparse-type)

(deftest map-literal-test
  (is-cf {:bar :b}
         '{:bar ':b})
  ;correctly generalise
  (is-cf {(clojure.core.typed/ann-form :bar clojure.lang.Keyword) :b}
         (clojure.lang.IPersistentMap clojure.lang.Keyword ':b)))

(deftest isa-test
  (is-cf (isa? 1 1))
  (is-cf (isa? {:parents {} :ancestors {} :descendants {}} 1 1))
  (is-cf #(isa? (class %) Number)))

(deftest array-primitive-hint-test
  (is-cf (let [^ints a (clojure.core.typed/into-array> int [(int 1)])]
           (alength a))))


(deftest flow-assert-test
  (is-clj (subtype?
            (-> (tc-t (fn [a]
                        {:pre [(integer? a)]}
                        a))
                ret-t)
            (parse-type `[Any :-> AnyInteger])))
  (is-clj (subtype? 
            (-> (tc-t (let [a (read-string "1")
                            _ (assert (integer? a))]
                        (+ 10 a)))
                ret-t)
            (parse-type `AnyInteger)))
  ;postconditions
  (is-clj (subtype?
            (-> (tc-t (fn [a]
                        {:post [(vector? %)]}
                        a))
                ret-t)
            (parse-type `[Any :-> (Vec Any)]))))

(deftest complete-hmap-test
  (is-clj (subtype? (-complete-hmap {})
                    (parse-type `(clojure.lang.APersistentMap Nothing Nothing))))
  (is-clj (not
            (subtype? (make-HMap :mandatory {})
                      (parse-type `(clojure.lang.APersistentMap Nothing Nothing)))))
  (is-clj (subtype? (-> (tc-t {}) ret-t)
                    (parse-type `(clojure.lang.APersistentMap Nothing Nothing)))))

(deftest dotted-on-left-test
  (is-tc-e (memoize (fn []))))

(deftest string-as-seqable-test
  (is-clj (subtype? 
            (RClass-of String)
            (RClass-of Seqable [-any])))
  (is-clj (subtype? 
            (-val "a")
            (RClass-of Seqable [-any])))
  (is-tc-e (seq "a"))
  (is-tc-e (first "a") Character)
  (is-tc-e (first (ann-form "a" String)) (Option Character)))

(deftest string-as-indexed-test
  (is (sub? String (clojure.lang.Indexed clojure.core.typed/Any))))

(deftest recursive-cf-test
  (is (thrown? Exception
               (cf (clojure.core.typed/cf 1 Number)
                   clojure.core.typed/Any))))

(deftest intersection-simplify-test
  (is-tc-e (let [a (ann-form [] (U (Extends [Number] :without [(clojure.lang.IPersistentVector Number)])
                                   (clojure.lang.IPersistentVector Number)))]
             (when (vector? a)
               a))
           (U nil (clojure.lang.IPersistentVector Number))))

(deftest kw-args-test
  (is (check-ns 'clojure.core.typed.test.kw-args)))

(deftest get-APersistentMap-test
  (is-tc-e (get (ann-form {} (clojure.lang.APersistentMap Num Num)) :a)
           (U nil Num)))

(deftest enum-field-non-nilable-test
  (is-cf (java.util.concurrent.TimeUnit/NANOSECONDS)
         java.util.concurrent.TimeUnit))

;;;; Checking deftype implementation of protocol methods

(deftest new-instance-method-return-test
  (is (check-ns 'clojure.core.typed.test.protocol))
  (is (err/top-level-error-thrown?
        (check-ns 'clojure.core.typed.test.protocol-fail))))
;;;;

;FIXME CTYP-71
;(deftest let-filter-unscoping-test
;  (is-cf (fn [a]
;            (and (< 1 2) a))
;         [(U nil Number) -> clojure.core.typed/Any :filters {:then (is Number 0)}]))

(deftest map-literal-containing-funapp-test
  (is-tc-e {:bar (identity 1)}))

(deftest doseq>-test
  (is-tc-e (doseq> [a :- (U AnyInteger nil), [1 nil 2 3]
                    :when a]
             (inc a)))
  ;wrap in thunk to prevent evaluation
  (is-tc-err
    (fn []
      (doseq> [a :- (U AnyInteger nil), [1 nil 2 3]]
        (inc a)))))

(deftest for>-test
  (is-tc-e
    (for> :- Number
          [a :- (U nil Number), [1 nil 2 3]
           b :- Number, [1 2 3]
           :when a]
          (+ a b))
    (Seq Number))
  (is-tc-err
    (for> :- Number
          [a :- (U Symbol nil Number), [1 nil 2 3]
           b :- Number, [1 2 3]]
          (+ a b))))

(deftest dotimes>-test
  (is-tc-e (dotimes> [i 100] (inc i)) nil))

(deftest records-test
  (is (check-ns 'clojure.core.typed.test.records))
  (is (check-ns 'clojure.core.typed.test.records2)))

(deftest string-methods-test
  (is-tc-e (.toUpperCase "a") 
           String))

(deftest common-destructuring-test
  (is (check-ns 'clojure.core.typed.test.destructure)))

(deftest loop-errors-test
  (is (caught-top-level-errors #{1}
        (cf (loop [a 1] a))))
  (is (caught-top-level-errors #{2}
        (cf (clojure.core.typed/loop> [a :- String, 1] a)))))

(deftest map-indexed-test
  (is-tc-e (map-indexed (inst vector Any AnyInteger Long) 
                        [1 2])
           (Seqable '[AnyInteger Long])))

(deftest letfn>-test
  (is-tc-e (letfn> [a :- [Number -> Number]
                    (a [b] (inc b))]
             (a 1))
           Number)
  ; annotation needed
  (is-tc-err (letfn> [(a [b] (inc b))]
               (a 1)))
  ;interdependent functions
  (is-tc-e (letfn> [a :- [Number -> Number]
                    (a [c] (b c))

                    b :- [Number -> Number]
                    (b [d] (do a d))]
             (a 1))
           Number))

;FIXME convert datatypes+records to RClasses
(deftest protocol-untyped-ancestor-test
  (is (check-ns 'clojure.core.typed.test.protocol-untyped-extend)))

(deftest kw-args-fail-test
  (is (caught-top-level-errors #{1}
        (check-ns 'clojure.core.typed.test.kw-args-undeclared-fail))))

(deftest filter-combine-test
  (is (check-ns 'clojure.core.typed.test.filter-combine)))

(deftest or-filter-simplify-test
  ;(| (is T  a)
  ;   (is T' a))
  ; simplifies to
  ;(is (U T T') a)
  (is-clj 
    (= (-or (-filter (RClass-of clojure.lang.Symbol) 'id)
            (-filter (RClass-of String) 'id))
       (-filter (Un (RClass-of clojure.lang.Symbol)
                    (RClass-of String))
                'id)))

  ;(| (is T  a pth)
  ;   (is T' a pth))
  ; simplifies to
  ;(is (U T T') a pth)
  (is-clj 
    (= (-or (-filter (RClass-of clojure.lang.Symbol) 'id [(-kpe :a)])
            (-filter (RClass-of String) 'id [(-kpe :a)]))
       (-filter (Un (RClass-of clojure.lang.Symbol)
                    (RClass-of String))
                'id [(-kpe :a)])))
  
  ;(& (is T a pth)
  ;   (when (is T a pth)
  ;     (is T' 'b)))
  ;  simplifies to 
  ;(& (is T a pth)
  ;   (is T' 'b))
  ;  FIXME
;  (is-clj 
;    (= (-and (-filter (RClass-of clojure.lang.Symbol) 'id [(-kpe :a)])
;             (-imp (-filter (RClass-of clojure.lang.Symbol) 'id [(-kpe :a)])
;               (-filter (RClass-of String) 'id2 [(-kpe :a) (-kpe :b)])))
;       (-and (-filter (RClass-of clojure.lang.Symbol) 'id [(-kpe :a)])
;             (-filter (RClass-of String) 'id2 [(-kpe :a) (-kpe :b)]))))
  )

(deftest or-filter-update-test
  (is-clj (clj
        (= (update -any
                   (-or (-filter (RClass-of clojure.lang.Symbol) 'id)
                        (-filter (RClass-of String) 'id)))
           (Un (RClass-of clojure.lang.Symbol)
               (RClass-of String))))))

(deftest path-update-test
  (is-clj 
    (both-subtype? (clj (update (Un -nil (make-HMap :mandatory {(-val :foo) (RClass-of Number)}))
                                (-filter (Un -false -nil) 'id [(-kpe :foo)])))
                   -nil))
  (is-clj 
    (both-subtype? (update (Un -nil (make-HMap :mandatory {(-val :foo) (RClass-of Number)}))
                           (-not-filter (Un -false -nil) 'id [(-kpe :foo)]))
                   (make-HMap :mandatory {(-val :foo) (RClass-of Number)})))
  ; if (:foo a) is nil, either a has a :foo entry with nil, or no :foo entry
  (is-clj (both-subtype? (update (make-HMap)
                                 (-filter -nil 'id [(-kpe :foo)]))
                         (make-HMap :optional {(-val :foo) -nil}))))

(deftest multimethod-test
  (is (check-ns 'clojure.core.typed.test.mm)))

(deftest instance-field-test
  (is-cf (.ns ^clojure.lang.Var #'clojure.core/map))
  (is (err/top-level-error-thrown?
        (cf (fn [] (.ns ^clojure.lang.Var 'a))))))

(deftest HMap-syntax-test
  (is-clj (= (parse-type `(HMap :absent-keys #{:op}))
             (make-HMap :absent-keys #{(-val :op)} :complete? false))))

(deftest map-filter-test
  (is-tc-e 
    (ann-form (fn [a] (:op a))
              [(U '{:op ':if} '{:op ':case})
               -> (U ':if ':case)
               :filters {:then (is (U ':case ':if) 0 [(Key :op)])
                         :else (| (is (HMap :absent-keys #{:op}) 0)
                                  (is (U false nil) 0 [(Key :op)]))}
               :object {:id 0
                        :path [(Key :op)]}]))
  ; {:then (is :if 0 [:op])
  ;  :else (| (! :if 0 [:op])
  ;           (is (HMap :absent-keys #{:op}) 0))}
  (is-tc-e #(= :if (:op %))
           [(U '{:op ':if} '{:op ':case})
            -> Boolean
            :filters {:then (& (is '{:op (Value :if)} 0)
                               (is ':if 0 [(Key :op)]))
                      :else (! ':if 0 [(Key :op)])}])
  (is-tc-e (fn [a :- (U '{:op ':if} '{:op ':case})
                b :- (U '{:op ':if} '{:op ':case})]
             (if (= :if (:op a))
               (= :case (:op b))
               false)))
  (is-tc-e (fn [a b] 
             (let [and__3941__auto__ (clojure.core/symbol? a)] 
               (if (print-filterset "test" and__3941__auto__)
                 (clojure.core/number? b) 
                 and__3941__auto__)))))

(deftest warn-on-unannotated-vars-test
  (is (check-ns 'clojure.core.typed.test.warn-on-unannotated-var)))

(deftest number-ops-test
  (is-tc-e (min (Integer. 3) 10) Number))

(deftest ctor-infer-test
  (is-tc-e (java.io.File. "a"))
  (is-tc-e (let [a (or "a" "b")]
             (java.io.File. a)))
  (is-tc-err
    (fn [& {:keys [path] :or {path "foo"}}]
      (print-env "a")
      (java.io.File. path))
    [& :optional {:path String} -> java.io.File]))

;(fn> [a :- (U (Extends Number :without [(IPerVec clojure.core.typed/Any)])
;              (Extends (IPV clojure.core.typed/Any) :without [Number])
;              String)]
;  (cond
;    (number? a) ...
;    (vector? a) ...
;    :else ...))
;
;(Extends [(IPersistentCollection a)
;          (Seqable a)
;          (IPersistentVector a)
;          (Reversible a)
;          [Number -> a]
;          (IPersistentStack a)
;          (ILookup Number a)
;          (Associative Number a)
;          (Indexed a)]
;         :without [(IPersistentMap clojure.core.typed/Any clojure.core.typed/Any)])

(deftest extends-test
  ; without extends: never returns a (IPV Number) because we can have
  ; a type (I (IPM clojure.core.typed/Any clojure.core.typed/Any) (IPV clojure.core.typed/Any))
  (is-tc-err
    (fn [a]
      (if (vector? a)
        a
        nil))
    [(U (Vec Num) (Map Any Any)) -> (U nil (Vec Num))])
  ; can use assertions to prove non-overlapping interfaces
  (is-tc-e
    (fn [a]
      {:pre [(or (and (vector? a)
                      (not (map? a)))
                 (and (map? a)
                      (not (vector? a))))]}
      (if (vector? a)
        a
        nil))
    [(U (Vec Num) (Map Any Any)) -> (U nil (Vec Num))])
  ; or use static types
  (is-tc-e 
    (fn [a]
      (if (vector? a)
        a
        nil))
    [(U (Extends [(clojure.lang.IPersistentVector Number)]
                 :without [(clojure.lang.IPersistentMap Any Any)])
        (Extends [(clojure.lang.IPersistentMap Any Any)]
                 :without [(clojure.lang.IPersistentVector Any)]))
     -> (U nil (clojure.lang.IPersistentVector Number))])
  ; technically it's ok to implement Number and IPM
  (is-tc-e 
    (fn [a]
      {:pre [(number? a)]}
      (print-env "a")
      (+ 1 a))
    [(clojure.lang.IPersistentMap Any Any) -> Number]))


(deftest set!-test
  (is (check-ns 'clojure.core.typed.test.set-bang)))

(deftest flow-unreachable-test
  ; this will always throw a runtime exception, which is ok.
  (is-tc-e 
    (fn [a] 
      {:pre [(symbol? a)]}
      (print-env "a") 
      (ann-form a Sym))
    [Long -> Sym]))

; FIXME this is wrong, should not just be nil
#_(deftest array-first-test
    (is-cf (let [a (clojure.core.typed/into-array> Long [1 2])]
             (first a))))

(deftest every?-update-test
  (is-tc-e (let [a (ann-form [] (U nil (Coll Any)))]
             (assert (every? number? a))
             a)
           (U nil (Coll Number))))

(deftest keys-vals-update-test
  (is-clj (both-subtype? 
            (update (RClass-of IPersistentMap [-any -any])
                    (-filter (RClass-of Seqable [(RClass-of Number)])
                             'a [(KeysPE-maker)]))
            (RClass-of IPersistentMap [(RClass-of Number) -any])))
  ; test with = instead of subtype to catch erroneous downcast to (IPersistentMap clojure.core.typed/Nothing clojure.core.typed/Any)
  (is-clj (both-subtype?
            (-> 
              (tc-t (let [m (clojure.core.typed/ann-form {} (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any))]
                      (assert (every? number? (keys m)))
                      m))
              ret-t)
            (parse-type `(clojure.lang.IPersistentMap Number Any))))
  (is-clj (both-subtype? 
            (-> (tc-t (let [m (clojure.core.typed/ann-form {} (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any))]
                        (assert (every? number? (keys m)))
                        (assert (every? number? (vals m)))
                        m))
                ret-t)
            (parse-type `(clojure.lang.IPersistentMap Number Number))))
  (is-cf (fn [m]
            {:pre [(every? number? (vals m))]}
            m)
          [(clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any) -> (clojure.lang.IPersistentMap clojure.core.typed/Any Number)])
  (is-cf (fn [m]
            {:pre [(every? symbol? (keys m))
                   (every? number? (vals m))]}
            m)
          [(clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any) -> (clojure.lang.IPersistentMap clojure.lang.Symbol Number)]))

; a sanity test for intersection cache collisions
(deftest intersect-cache-test
  (is-clj (=
       (Poly-body*
         ['foo1 'foo2]
         (Poly* '[x y] 
                [no-bounds no-bounds]
                (In (make-F 'x) (make-F 'y))))
       (In (make-F 'foo1) (make-F 'foo2)))))


;CTYP-27
;(deftest nth-inline-test
;  (is (cf (fn [s] (clojure.lang.RT/nth s 0 nil))
;          (clojure.core.typed/All [x] (Fn #_[nil -> nil]
;                       #_[(I (clojure.lang.Seqable x) (ExactCount 0)) -> nil]
;                       [(I (clojure.lang.Seqable x) (CountRange 1)) -> x]
;                       #_[(U nil  (clojure.lang.Seqable x)) -> (U nil  x)])))))

(deftest invoke-tfn-test
  (is-clj (inst/manual-inst (parse-type `(All [[~'x :< (TFn [[~'x :variance :covariant]] Any)]]
                                                (~'x Any)))
                            [(parse-type `(TFn [[~'x :variance :covariant]] Number))]))
  (is-clj (inst/manual-inst (parse-type `(All [[~'x :< (TFn [[~'x :variance :covariant]] Number)]]
                                              (~'x Any)))
                            [(parse-type `(TFn [[~'x :variance :covariant]] Number))]))
  (is-clj (inst/manual-inst (parse-type `(All [x#
                                                 [y# :< x#]] 
                                                Any))
                            [(parse-type `Any)
                             (parse-type `Any)])))

#_(fully-resolve-type (parse-type '((clojure.core.typed/All [a] (TFn [[x :variance :covariant :< a]] a)) Number)))


#_(deftest filter-seq-test
  ;  TODO possible extension for filter
;  (is (cf (filter :a (clojure.core.typed/ann-form [] (clojure.lang.Seqable '{:b Number})))
;          (clojure.lang.Seqable '{:b Number :a clojure.core.typed/Any})))
  (is-tc-err (core/filter (inst identity (U nil Number)) [1 nil])
             (Seqable Number))
  (is-tc-e ((inst core/filter (U nil Number) nil) (inst identity (U nil Number)) [1 nil])
           (Seqable Number))
  (is-tc-e ((inst core/filter (U nil Number) nil) identity [1 nil])
           (Seqable Number))

  (is-tc-e (filter-identity :in (U nil Number) [1 nil])
           (Seqable Number))
  (is (= (filter-identity :in (U nil Number) [1 nil])
         [1]))

  (is-tc-e (filter-identity :in (U false nil Number) [1 nil])
           (Seqable Number))
  (is (= (filter-identity :in (U false nil Number) [1 nil])
         [1]))

  (is-tc-err (filter-identity :in Number [1 nil])
             (Seqable Number))
  (is (= (filter-identity :in Number [1 nil])
         [1]))

  (is-tc-e (let [filter (ann-form core/filter
                                  (clojure.core.typed/All [x y]
                                       [[x -> clojure.core.typed/Any :filters {:then (is y 0)}] 
                                        (U nil (Seqable x)) -> (Seq y)]))]
             (filter number? [1 nil]))
           (Seqable Number)))

#_(deftest remove-nil-test
  (is-tc-e (remove-nil :in Number [1 2 3])
           (Seqable Number))
  (is (= (remove-nil :in Number [1 2 3])
         [1 2 3]))

  (is-tc-e (remove-nil :in (U nil Number) [1 2 3])
           (Seqable Number))
  (is-tc-e (remove-nil :in (U false nil Number) [1 2 3])
           (Seqable (U false Number))))

#_(deftest remove-false-test
  (is-tc-e (remove-false :in Number [1 2 3])
           (Seqable Number))
  (is (= (remove-false :in Number [1 2 3])
         [1 2 3]))

  (is-tc-e (remove-false :in (U false Number) [1 2 3])
           (Seqable Number))
  (is-tc-e (remove-false :in (U false nil Number) [1 2 3])
           (Seqable (U nil Number)))
  (is-tc-err ((inst core/remove (U nil Number) nil) nil? [1 2 3 nil]) (Seq Number)))

; keeping if we decide to use more expressive type for conj
;(deftest extensible-conj-test
;  (is (cf ((clojure.core.typed/inst conj Number (TFn [[x :variance :covariant]] x)
;                 (TFn [[x :variance :covariant]] (clojure.lang.PersistentHashSet x)))
;            (clojure.core.typed/ann-form #{} (clojure.lang.PersistentHashSet Number)) 1)
;          (clojure.lang.PersistentHashSet Number)))
;  ;FIXME
;#_(is (cf (conj
;            (clojure.core.typed/ann-form #{} (clojure.lang.PersistentHashSet Number)) 1)
;          (clojure.lang.PersistentHashSet Number))))
;(deftest subtype-ipcoll-test
;  (is-clj (sub? (clojure.lang.IPersistentCollection 
;              (clojure.lang.IMapEntry clojure.core.typed/Any clojure.core.typed/Any)
;              (TFn [[x :variance :covariant
;                     :< (U nil (clojure.lang.IMapEntry clojure.core.typed/Any clojure.core.typed/Any))]]
;                   x)
;              (clojure.core.typed/All [a1 b1]
;                   (TFn [[x :variance :covariant
;                          :< (U nil (clojure.lang.IMapEntry a1 b1))]]
;                        (clojure.lang.APersistentMap a1 b1))))
;            (clojure.core.typed/Coll clojure.core.typed/Any)))
;  (is-clj (sub? (TFn [[x :variance :covariant]]
;                   x)
;            (TFn [[x :variance :covariant]]
;                 clojure.core.typed/Any)))
;  (is-clj (sub? (TFn [[x :variance :covariant
;                     :< (U nil (clojure.lang.IMapEntry clojure.core.typed/Any clojure.core.typed/Any))]]
;                   x)
;            (TFn [[x :variance :covariant]]
;                 clojure.core.typed/Any)))
;  (is-clj (sub? (clojure.core.typed/All [a1 b1]
;                 (TFn [[x :variance :covariant
;                        :< (U nil (clojure.lang.IMapEntry a1 b1))]]
;                      (clojure.lang.APersistentMap a1 b1)))
;            (Rec [c]
;                 (TFn [[x :variance :covariant]] 
;                      (clojure.lang.IPersistentCollection 
;                        clojure.core.typed/Any 
;                        (TFn [[x :variance :covariant]] clojure.core.typed/Any) 
;                        c))))))


(deftest intersection-csgen-test
  (is-clj (clj (cs-gen #{} {'a no-bounds} {}
                   (In (RClass-of Seqable [(RClass-of Number)])
                       (make-CountRange 1))
                   (In (RClass-of Seqable [(make-F 'a)])
                       (make-CountRange 1))))))

(deftest iterable-as-seqable-test
  (is-cf (first (clojure.core.typed/ann-form [] (Iterable clojure.core.typed/Any)))))

; See CTYP-29 for discussion. f in (map f coll) needs to be only a single arity
; to help inference.
(deftest map-over-multiarity-fn-test
  (is-cf (map (clojure.core.typed/ann-form + [Number -> Number]) 
              (clojure.core.typed/ann-form [] (clojure.lang.Seqable Number))))
  (is-cf (map inc [(or (first (range)) 0) 1])
         (clojure.lang.Seqable clojure.core.typed/AnyInteger))
  (is-cf (fn [x] (map (clojure.core.typed/ann-form inc [Number -> Number]) [x 1]))
         [Number -> (clojure.lang.Seqable Number)])
  (is-clj (subtype? (ret-t (tc-t [(or (first (range)) 2) 1]))
                    (RClass-of Seqable [(RClass-of Number)])))
  (is-cf (fn [x] 
           (map (clojure.core.typed/ann-form inc [Number -> Number]) 
                [x 2 3])) 
         [Number -> (clojure.core.typed/Seq Number)]))

(deftest unannotated-datatype-test
  (is (check-ns 'clojure.core.typed.test.unannotated-datatype)))

;(deftest intersect-RClass-ancestors-test
;  (is-clj (= (In (RClass-of IPersistentSet [-any])
;             (RClass-of Seqable [(RClass-of Number)]))
;         (RClass-of IPersistentSet [-any]))))

(deftest munged-datatype-fields-test
  (is (check-ns 'clojure.core.typed.test.munge-record-field)))

;(defmacro when-async-dep [& body]
;  (when (try (require 'clojure.core.async)
;             true
;             (catch Exception e false))
;    `(do ~@body)))
;
;(when-async-dep
;(deftest Extends-subtype-test
;  (is-clj (check-ns 'clojure.core.typed.test.async))
;  (is-clj (sub? (Extends [(clojure.lang.Seqable Long)
;                      (clojure.lang.IPersistentVector Long)])
;            (Extends [(clojure.lang.Seqable Number)
;                      (clojure.lang.IPersistentVector Number)])))
;
;  (is-clj (sub? (clojure.core.typed.async/Chan clojure.core.typed.test.async/Query)
;            (clojure.core.typed.async/Chan clojure.core.typed/Any)))
;
;  (is-clj (sub? (clojure.core.async.impl.protocols/ReadPort clojure.core.typed.test.async/Query)
;            (clojure.core.async.impl.protocols/ReadPort clojure.core.typed/Any)))
;
;  (is-clj (sub? (clojure.core.async.impl.protocols/Channel clojure.core.typed.test.async/Query)
;            (clojure.core.async.impl.protocols/Channel clojure.core.typed/Any)))
;
;  (is-clj (sub? (Extends [(clojure.core.async.impl.protocols/Channel clojure.core.typed.test.async/Query)
;                      (clojure.core.async.impl.protocols/WritePort clojure.core.typed.test.async/Query)
;                      (clojure.core.async.impl.protocols/ReadPort clojure.core.typed.test.async/Query)])
;            (Extends [(clojure.core.async.impl.protocols/Channel clojure.core.typed/Any)
;                      (clojure.core.async.impl.protocols/WritePort clojure.core.typed/Any)
;                      (clojure.core.async.impl.protocols/ReadPort clojure.core.typed/Any)])))
;  (is-clj (sub? (Extends [(clojure.core.async.impl.protocols/Channel clojure.core.typed.test.async/Query)
;                      (clojure.core.async.impl.protocols/WritePort clojure.core.typed.test.async/Query)
;                      (clojure.core.async.impl.protocols/ReadPort clojure.core.typed.test.async/Query)])
;            (Extends [(clojure.core.async.impl.protocols/ReadPort clojure.core.typed/Any)]))))
;  )
;
;(when-async-dep
;(deftest Extends-cs-gen-test
;  (is-clj (check-ns 'clojure.core.typed.test.async))
;  (is-clj (cs-gen #{} {'x no-bounds} {} 
;          (parse-type '(clojure.lang.LazySeq clojure.core.typed/Any))
;          (with-bounded-frees {(make-F 'x) no-bounds}
;            (parse-type '(clojure.lang.LazySeq x)))))
;
;  (is-clj (cs-gen #{} {'x no-bounds} {} 
;              (parse-type '(clojure.core.async.impl.protocols/ReadPort clojure.core.typed/Any))
;              (with-bounded-frees {(make-F 'x) no-bounds}
;                (parse-type '(clojure.core.async.impl.protocols/ReadPort x)))))
;  (is-clj (cf ((clojure.core.typed/inst identity  (Extends [(clojure.core.async.impl.protocols/ReadPort clojure.core.typed/Any)]))
;           (clojure.core.typed/ann-form (clojure.core.typed.async/chan> clojure.core.typed/Any) 
;                              (Extends [(clojure.core.async.impl.protocols/ReadPort clojure.core.typed/Any)])))
;          (Extends [(clojure.core.async.impl.protocols/ReadPort clojure.core.typed/Any)])))
;  (is-clj (cf (identity
;           (clojure.core.typed/ann-form (clojure.core.typed.async/chan> clojure.core.typed/Any) 
;                                        (Extends [(clojure.core.async.impl.protocols/ReadPort clojure.core.typed/Any)])))
;          (Extends [(clojure.core.async.impl.protocols/ReadPort clojure.core.typed/Any)])))
;  (is-clj (cf (identity
;           (clojure.core.typed/ann-form (clojure.core.typed.async/chan> clojure.core.typed/Any) 
;                     (clojure.core.async.impl.protocols/ReadPort clojure.core.typed/Any)))
;          (clojure.core.async.impl.protocols/ReadPort clojure.core.typed/Any)))
;  (is-clj (cf (seq (clojure.core.typed/ann-form [] (clojure.core.typed/Coll (clojure.core.typed.async/Chan clojure.core.typed/Any))))
;          (U nil (clojure.core.typed/Coll (clojure.core.typed.async/Chan clojure.core.typed/Any)))))
;  (is-clj (cf (seq (clojure.core.typed/ann-form [] (clojure.core.typed/Coll (clojure.core.typed/Seqable clojure.core.typed/Any))))
;          (U nil (clojure.core.typed/Coll (clojure.core.typed/Seqable clojure.core.typed/Any)))))
;  (is-clj (subtype? (parse-type '(clojure.core.typed.async/Chan clojure.core.typed/Any))
;                -any))
;
;  (is-clj (overlap (parse-type '(clojure.core.typed.async/Chan clojure.core.typed/Any))
;               -any)))
;  )
;
;(when-async-dep
;(deftest Extends-inference-test
;  (is-clj (cf [1] (Extends [(clojure.lang.IPersistentVector Number)])))
;  (is-clj (cf (let [x (clojure.core.typed/ann-form [1] (Extends [(clojure.lang.Seqable Number) 
;                                                             (clojure.lang.IPersistentVector Number)]))]
;            ((clojure.core.typed/inst first Number) x))
;          (U nil Number)))
;  (is-clj (cf (let [x (clojure.core.typed/ann-form [1] (Extends [(clojure.lang.Seqable Number) 
;                                                             (clojure.lang.IPersistentVector Number)]))]
;            (first x))))
;  (is-clj (cf (clojure.core.typed/letfn> [af :- (clojure.core.typed/All [a]
;                                                 [(Extends [(clojure.lang.Seqable a)
;                                                            (clojure.lang.IPersistentVector a)])
;                                                  -> a])
;                                      (af [x] {:post [%]} (first x))]
;            (af [1])))))
;  )
;
;(when-async-dep
;(deftest core-async-test
;  (is-clj (check-ns 'clojure.core.typed.test.async-go))
;  (is-clj (check-ns 'clojure.core.typed.test.async-alts)))
;  )

;(cf (seq (ann-form (seq []) (Seqable clojure.core.typed/Any))))
;(cf (seq (ann-form (seq []) (ISeq clojure.core.typed/Any))))

(deftest mm-warn-on-unannotated-vars-test
  (is (check-ns 'clojure.core.typed.test.mm-warn-on-unannotated)))

(deftest HMap-parse-fail-test
  (is (err/tc-error-thrown? (clj (parse-type `(HMap :mandatory {:a Any} :absent-keys #{:a}))))))

(deftest HMap-absent-complete-test
  (is-clj (not (sub? (HMap :mandatory {:a clojure.core.typed/Any}) (HMap :absent-keys #{:a}))))
  (is-clj (not (sub? (HMap :complete? false) (HMap :complete? true))))
  (is-clj (sub? (HMap :complete? false) (HMap :complete? false)))
  (is-clj (sub? (HMap :complete? true) (HMap :complete? false)))
  (is-clj (sub? (HMap :complete? true) (HMap :complete? true)))
  (is-clj (sub? (HMap :absent-keys #{:a :b}) (HMap :absent-keys #{:a})))
  (is-clj (not (sub? (HMap :absent-keys #{}) (HMap :absent-keys #{:a}))))
  (is-clj (not (sub? (HMap :absent-keys #{:a}) (HMap :absent-keys #{:a :b})))))

(deftest extend-record-to-protocol-test
  (is (check-ns 'clojure.core.typed.test.extend-record)))

(deftest hmap-smart-infer-test
  (is-cf {:a #(+ % 1)} (HMap :optional {:a [Number -> Number]})))

(deftest fnil-test
  (is-cf ((fnil + 0) 2))
  (is-cf ((fnil + 0) nil))
  ; can Typed Racket do better here?
  (is-cf ((fnil (clojure.core.typed/ann-form + [Number * -> Number])
                0)
          2.2)))

;(cf (every? (fn [a] a) [1]))

;
(deftest csgen-combine-test
  (is-cf (map inc [0 1.1])
         (clojure.lang.Seqable Number))
  (is-cf (map (clojure.core.typed/inst vector clojure.core.typed/Any Number Number) [1] [2])
         (clojure.lang.Seqable '[Number Number])))

;FIXME uncomment after core.typed internals are being checked
;(deftest subtype-explosion-test
;  (is (sub? nil clojure.core.typed.type-rep/TCType)))

(deftest var-as-function-test
  (is-cf #'+ [Number * -> Number])
  (is-cf (#'+ 1 2))
  (is (sub? (clojure.core.typed/Var1 [-> nil]) [-> nil])))

(deftest future-test
  (is-cf @(future 'a) clojure.lang.Symbol)
  (is-cf (future 'a) java.util.concurrent.Future))

(deftest ignore-macro-def-test
  (is-cf (defmacro foobar [])))

(deftest typed-deps-fail-gracefully-test
  (is-tc-err (clojure.core.typed/typed-deps no.exist.fail)))

(deftest def-expected-test
  (is-cf (do
           (clojure.core.typed/ann foo1 clojure.core.typed/Any)
           (clojure.core.typed/ann-form (def foo1 1) clojure.core.typed/Any)))
  (is-cf (do
           (clojure.core.typed/ann foo2 clojure.core.typed/Any)
           (clojure.core.typed/ann-form (def foo2) clojure.core.typed/Any))))

(deftest CTYP-42
  (is (check-ns 'clojure.core.typed.test.succeed.CTYP-42-record-extend-protocol)))

(deftest atom>-test
  (is-tc-e (atom> (Vec Any) [])
           (Atom1 (Vec Any))))

(deftest CTYP-48
  (is-tc-e (fn [a] (:a a))
           [Nothing -> Any]))

(deftest CTYP-49 
  (is (check-ns 'clojure.core.typed.test.succeed.CTYP49-unreachable)))

#_(deftest CTYP-47-Fn-as-IFn
  (is-cf (fn [] #())
          [-> clojure.lang.IFn]))

(deftest plain-defprotocol-test
  (is (err/top-level-error-thrown? 
        (check-ns 'clojure.core.typed.test.fail.plain-defprotocol)))
  (is (err/top-level-error-thrown? 
        (check-ns 'clojure.core.typed.test.fail.CTYP-45))))

(deftest HMap-absent-key-update-test
  ;ensure absent keys are preserved when passed through occurrence typing's `update`
  (is-tc-e
    (let [a :- (HMap :mandatory {:a Num}
                     :optional {:b Num,
                                :c Num})
          {:a 1}]
      (when (:b a)
        (ann-form a (HMap :mandatory {:a Num}
                          :optional {:b Num,
                                     :c Num}))))))

(deftest non-empty-map-test
  (is-tc-e (map inc [1 2 3])
           (NonEmptySeq Num)))

;CTYP-53
(deftest hmap-cast-test
  (is (both-subtype?
        (ety
          (fn
            [m :- (HMap)]
            (assert (:foo m))
            m))
        (parse-clj `['{} :-> 
                     '{}
                     :filters {:then (~'! (U nil false) 0)
                               :else (~'is (U nil false) 0)}
                     :object {:id 0}])))
  (is (both-subtype? 
        (ety
          (fn
            [m :- (HMap)]
            :- (HMap :mandatory {:foo (clojure.core.typed/Vec clojure.core.typed/Any)})
            (assert (vector? (:foo m)))
            m))
        (parse-clj `[(HMap) :-> 
                     (HMap :mandatory {:foo (Vec Any)})
                     :filters {:then (~'! (U nil false) 0)
                               :else (~'is (U nil false) 0)}
                     :object {:id 0}])))
  (is (both-subtype? 
        (ety 
          (fn
            [m :- (HMap :mandatory {:bar Any})]
            (assert (nil? (:foo m)))
            m))
        (parse-clj `[(HMap :mandatory {:bar Any}) :-> 
                     (HMap :mandatory {:bar Any}
                           :optional {:foo nil})
                     :filters {:then (~'! (U nil false) 0)
                               :else (~'is (U nil false) 0)}
                     :object {:id 0}])))
  (is
    (both-subtype?
      (ety
        (fn
          [m :- '{}]
          (assert (not (vector? (:foo m))))
          m))
      (parse-clj `[(HMap) :-> 
                   ; not sure if this should simplify to (HMap)
                   (HMap :optional {:foo Any})
                   :filters {:then (~'! (U nil false) 0)
                             :else (~'is (U nil false) 0)}
                   :object {:id 0}])))
  (is 
    (clj
      (let [t1 (clj (update (parse-type `(HMap))
                            (parse-filter `(~'is (Vec Any) ~'m [(~'Key :foo)]))))
            t2 (clj (parse-type `(HMap :mandatory {:foo (Vec Any)})))]
        (both-subtype? t1 t2)))))

;CTYP-60
(deftest absent-keys-test
  (is (not (sub?-q `(HMap :mandatory {:a String}
                          :complete? true)
                   `(HMap :absent-keys #{:a}))))
  (is-tc-err {:a "a"} (HMap :absent-keys #{:a})))

;CTYP-61
(deftest hmap-assoc-test
  (is-tc-e (assoc (ann-form {} (HMap :optional {:a Any})) 
                  :b "v")
           (HMap :mandatory {:b (Value "v")} 
                 :optional {:a Any}))
  (is-tc-e (assoc (ann-form {} (HMap :optional {:a Any})) 
                  :a "v")
           (HMap :mandatory {:a (Value "v")})))

(deftest CTYP-37-defprotocol-better-error
  (is (check-ns 'clojure.core.typed.test.CTYP-37)))

(deftest invoke-dissoc-test
  ; complete dissocs
  (equal-types (dissoc {} :a)
               (HMap :mandatory {} :complete? true))
  
  (equal-types (dissoc {:a 6} :a)
               (HMap :mandatory {} :complete? true))
  
  (equal-types (dissoc {:a 6 :b 7} :a)
               (HMap :mandatory {:b '7} :complete? true))
  
  (equal-types (dissoc {:a 6 :b 7} :a :b)
               (HMap :mandatory {} :complete? true))
  
  (equal-types (dissoc {:a 6 :b 7 :c 8} :a :b)
               (HMap :mandatory {:c (Value 8)} :complete? true))
  
  ; incomplete dissocs
  (equal-types (dissoc (clojure.core.typed/ann-form {} (HMap)) :a)
               (HMap :absent-keys #{:a}))
  
  (equal-types (dissoc (clojure.core.typed/ann-form {} (HMap :optional {:a String})) :a)
               (HMap :absent-keys #{:a}))
  
  (equal-types (dissoc (clojure.core.typed/ann-form {} (HMap :optional {:a String})) :b)
               (HMap :optional {:a String} :absent-keys #{:b}))
  
  (equal-types (dissoc (clojure.core.typed/ann-form {:b 7} (HMap :mandatory {:b '7} :optional {:a String})) :a)
               (HMap :mandatory {:b (Value 7)} :absent-keys #{:a}))
  
  (equal-types (dissoc (clojure.core.typed/ann-form {:b 7} (HMap :optional {:a String :c String})) :a :b)
               (HMap :optional {:c String} :absent-keys #{:a :b})))

(deftest invoke-assoc-test
  
  ; HMaps
  (equal-types (assoc {} :a 5)
               (HMap :mandatory {:a '5} :complete? true))
  
  (equal-types (assoc nil :a 5)
               (HMap :mandatory {:a '5} :complete? true))
  
  (equal-types (assoc (clojure.core.typed/ann-form nil (U nil (HMap))) :a 5)
               (HMap :mandatory {:a '5}))
  
  (equal-types (assoc (clojure.core.typed/ann-form nil (U nil (HMap :complete? true))) :a 5)
               (HMap :mandatory {:a '5} :complete? true))
  
  (equal-types (assoc (clojure.core.typed/ann-form {} (HMap)) :a 5)
               (HMap :mandatory {:a '5}))
  
  (equal-types (assoc (clojure.core.typed/ann-form {} (HMap :optional {:a clojure.core.typed/Any})) :b "v")
               (HMap :mandatory {:b (Value "v")} :optional {:a clojure.core.typed/Any}))
  
  (equal-types (assoc (clojure.core.typed/ann-form {} (HMap :optional {:a clojure.core.typed/Any})) :a "v")
               (HMap :mandatory {:a (Value "v")}))

  ;CTYP-79 resolve types properly in assoc
  (is (check-ns 'clojure.core.typed.test.hmap-resolve-assoc))
  
  ; HVecs
  (equal-types-noparse (assoc [] 0 1)
                       (-hvec [(-val 1)]
                              :filters [(-true-filter)]
                              :objects [-empty]))
  
  (equal-types-noparse (assoc [3] 1 2)
                       (-hvec [(-val 3) (-val 2)]
                              :filters [(-FS -top -top) ; embedded literals dont get any
                                                        ; filter information (yet)?
                                        (-true-filter)]
                              :objects [-empty -empty]))
  
  (equal-types-noparse (assoc [0] 0 1)
                       (-hvec [(-val 1)]
                              :filters [(-true-filter)]
                              :objects [-empty]))
  
  (equal-types-noparse (assoc [0] 0 (if (clojure.core.typed/ann-form 1 clojure.core.typed/Any) 1 2))
                       (-hvec [(Un (-val 1) (-val 2))]
                              :filters [(-true-filter)]
                              :objects [-empty]))
  
  ; Basic types
  (equal-types (assoc {} 'a 5)
               (clojure.lang.IPersistentMap 'a '5))
  
  (equal-types (assoc {:b 6} 'a 5)
               (clojure.lang.IPersistentMap (U 'a ':b) (U '5 '6)))
  
  (equal-types (assoc (clojure.core.typed/ann-form nil
                                                   (U nil (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any)))
                 :a 5)
               (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any))
  
  (equal-types (assoc (clojure.core.typed/ann-form {} (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any)) :a 5)
               (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any))
  
  (equal-types (assoc (clojure.core.typed/ann-form [] (clojure.lang.IPersistentVector clojure.core.typed/Any)) 0 2)
               (clojure.lang.IPersistentVector clojure.core.typed/Any))
  
  ;; TODO: assocs on records
  
  )

(deftest CTYP-62-equiv-test
  (is (tc-equiv := 
                (ret (-val "a"))
                (ret -any))
      (ret (Un -false -true)))
  (is (= (eret (= "a" (clojure.core.typed/ann-form 1 clojure.core.typed/Any)))
         (ret (Un -false -true))))
  (is (= (eret (= "a" (clojure.core.typed/ann-form 1 clojure.core.typed/Any)))
         (ret (Un -false -true)))))

(deftest invoke-merge-test
  
  ; basic literal case
  (equal-types (merge {:a 5 :b 6} {:c 7 :d 8} {:e 9})
               (HMap :mandatory {:a '5 :b '6 :c '7 :d '8 :e '9} :complete? true))
  
  ; right hand optionals
  (equal-types (merge {:a 5 :b 6}
                      (clojure.core.typed/ann-form {} (HMap :optional {:c String} :complete? true)))
               (HMap :mandatory {:a '5 :b '6}
                     :optional {:c String}
                     :complete? true))
  
  ; left hand optionals
  (equal-types (merge (clojure.core.typed/ann-form {} (HMap :optional {:a Number} :complete? true))
                      (clojure.core.typed/ann-form {} (HMap :optional {:b String} :complete? true)))
               (HMap :optional {:a Number :b String} :complete? true))
  
  ; nil first argument
  (equal-types (merge nil
                      (clojure.core.typed/ann-form {} (HMap :optional {:a Number} :complete? true))
                      (clojure.core.typed/ann-form {} (HMap :optional {:b String} :complete? true)))
               (HMap :optional {:a Number :b String} :complete? true))
  
  ; nils in other arguments
  (equal-types (merge nil
                      nil
                      (clojure.core.typed/ann-form {} (HMap :optional {:a Number} :complete? true))
                      nil
                      nil
                      (clojure.core.typed/ann-form {} (HMap :optional {:b String} :complete? true))
                      nil)
               (HMap :optional {:a Number :b String} :complete? true))
  
  ; all nils
  (equal-types (merge nil)
               nil)
  (equal-types (merge nil nil nil)
               nil)
  
  ; (Option HMap) first argument
  (equal-types (merge (clojure.core.typed/ann-form {:c 5} (U nil (HMap :mandatory {:c '5} :complete? true)))
                      (clojure.core.typed/ann-form {} (HMap :optional {:a Number} :complete? true))
                      (clojure.core.typed/ann-form {} (HMap :optional {:b String} :complete? true)))
               (HMap :optional {:a Number :b String :c '5} :complete? true))
  
  ; (Option HMap) arguments
  (equal-types (merge (clojure.core.typed/ann-form {} (U nil (HMap :complete? true)))
                      (clojure.core.typed/ann-form {} (U nil (HMap :optional {:a Number} :complete? true)))
                      (clojure.core.typed/ann-form {} (U nil (HMap :optional {:b String} :complete? true))))
               (U nil (HMap :optional {:a Number :b String} :complete? true)))
  
  ; this merge doesn't actually give us any information about :b because
  ; the second map might not have a :b key, and the first map is partial.
  (equal-types (merge (clojure.core.typed/ann-form {} (HMap :optional {:a Number} :complete? false))
                      (clojure.core.typed/ann-form {} (HMap :optional {:b String} :complete? true)))
               (HMap :optional {:a Number}))
  
  ; but this does
  (equal-types (merge (clojure.core.typed/ann-form {} (HMap :optional {:a Number} :complete? false))
                      (clojure.core.typed/ann-form {:b "s"} (HMap :mandatory {:b String} :complete? true)))
               (HMap :mandatory {:b String} :optional {:a Number}))
  
  ; multiple optionals
  (equal-types (merge (clojure.core.typed/ann-form {} (HMap :optional {:a Number} :complete? true))
                      (clojure.core.typed/ann-form {} (HMap :optional {:b String} :complete? true))
                      (clojure.core.typed/ann-form {} (HMap :optional {:c Long} :complete? true)))
               (HMap :optional {:a Number :b String :c Long} :complete? true))
  
  ;; incomplete right handsides
  
  ; non-covering right hand side
  (equal-types (merge {:a 5 :b 6}
                      (clojure.core.typed/ann-form {} '{}))
               '{:a clojure.core.typed/Any :b clojure.core.typed/Any})
  
  (equal-types (merge (clojure.core.typed/ann-form {:a 6} '{:a Number})
                      (clojure.core.typed/ann-form {:b "s"} '{:b String}))
               '{:a clojure.core.typed/Any :b String})
  
  ; incomplete covering mandatory
  (equal-types (merge {:a 5}
                      (clojure.core.typed/ann-form {:a 10} '{:a '10}))
               '{:a '10})
  
  ; incomplete covering optional
  (equal-types (merge {:a 5}
                      (clojure.core.typed/ann-form {} (HMap :optional {:a (Value 10)})))
               '{:a (U (Value 5) (Value 10))})
  
  
  ; both incomplete optionals
  (equal-types (merge (clojure.core.typed/ann-form {} (HMap :optional {:a '5}))
                      (clojure.core.typed/ann-form {} (HMap :optional {:a '10})))
               (HMap :optional {:a (U '5 '10)}))
  
  ; (Option HMap) first argument incomplete
  (equal-types (merge (clojure.core.typed/ann-form {:a 5} (U nil '{:a '5}))
                      (clojure.core.typed/ann-form {:b 8} (HMap :mandatory {:b Number} :complete? true)))
               (U (HMap :mandatory {:b Number} :complete? true)
                  '{:a '5 :b Number}))
  
  ;; nil (HMap :absent-keys #{:a}) -> (HMap :absent-keys #{:a})
  ;; '{:a 5} '{} -> '{:a 5}
  ;; nil '{:a Number} -> '{:a Number}
  ;; '{:a 5} '{:a Number} -> '{:a Number}
  ; clojure.core.typed/All together: (U '{:a Number} (HMap :absent-keys #{:a})) or (HMap :optional {:a Number})
  (equal-types (merge (clojure.core.typed/ann-form {:a 5} (U nil '{:a '5}))
                      (clojure.core.typed/ann-form {} (HMap :optional {:a Number} :complete? false)))
               (HMap :optional {:a Number}))
  
  ; Basic maps
  (equal-types (merge (clojure.core.typed/ann-form {} (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any)) {:a 5})
               (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any))
  
  (equal-types (merge {} {'a 5})
               (clojure.lang.IPersistentMap 'a '5))
  
  (equal-types (merge {:b 6} {'a 5})
               (clojure.lang.IPersistentMap (U 'a ':b) (U '5 '6)))

;;  TODO not handling presence of non keyword keys yet
;;   (equal-types (merge {'a 5} {:b 6})
;;                (clojure.lang.IPersistentMap (U 'a ':b) (U '5 '6)))
  
  )

(deftest invoke-conj-test
  
  ; need to manually build hvec to match filters/objects
  (equal-types-noparse (conj nil nil)
                       (-hvec [-nil]
                              :filters [(-false-filter)]
                              :objects [-empty]))
  
  (equal-types-noparse (conj [1] 2 3)
                       (-hvec [(-val 1) (-val 2) (-val 3)]
                              :filters [(-FS -top -top)
                                        (-true-filter)
                                        (-true-filter)]
                              :objects [-empty -empty -empty]))
  
  (equal-types-noparse (conj [1]
                             (clojure.core.typed/ann-form nil (U nil '2))
                             3)
                       (-hvec [(-val 1) (Un -nil (-val 2)) (-val 3)]
                              :filters [(-FS -top -top) ; embedded literals dont get any
                                                        ; filter information (yet)?
                                        (-FS -top -top)
                                        (-true-filter)]
                              :objects [-empty -empty -empty]))
  
  (equal-types (conj (clojure.core.typed/ann-form nil (U nil '['1]))
                     (clojure.core.typed/ann-form nil (U nil '2)))
               (U '[(U nil '2)]
                '['1 (U nil '2)]))
  
  (equal-types (conj {:a 1} [:b 2])
               (HMap :mandatory {:a '1 :b '2} :complete? true))
  
  (equal-types (conj {:a 1}
                     (clojure.core.typed/ann-form nil (U nil '[':b '2])))
               (U (HMap :mandatory {:a '1} :complete? true)
                  (HMap :mandatory {:a '1 :b '2} :complete? true)))
  
  (equal-types (conj (clojure.core.typed/ann-form nil (U nil (HMap :mandatory {:a '1} :complete? true)))
                     (clojure.core.typed/ann-form nil (U nil '[':b '2])))
               (U '[(U nil '[':b '2])]
                  (HMap :mandatory {:a '1} :complete? true)
                  (HMap :mandatory {:a '1 :b '2} :complete? true)))
  
  (equal-types (conj #{5} 6 7)
               (clojure.lang.IPersistentSet (U '5 '6 '7)))
  
  )

(deftest unannotated-record-test
  (is (err/top-level-error-thrown?
        (check-ns 'clojure.core.typed.test.fail.unannotated-record))))

(deftest datatype-method-recur-test
  (is (check-ns 'clojure.core.typed.test.datatype-recur)))

(deftest record-annotated-as-datatype-test
  ; record annotated as datatype
  (is (err/top-level-error-thrown?
        (check-ns 'clojure.core.typed.test.fail.record-as-datatype)))
  ; datatype annotated as record
  (is (err/top-level-error-thrown?
        (check-ns 'clojure.core.typed.test.fail.datatype-as-record))))

(deftest recursive-ann-test
  (is (check-ns 'clojure.core.typed.test.recursive)))

(deftest comparable-inline-test
  (is-tc-e (fn [v x] (compare v x)) (IFn [Comparable Any -> Num])))

(deftest CTYP-71-simplify-test
                                              ; must be resolvable to trigger the bug
  (is-tc-e (clojure.core.typed/fn [a :- (U nil (clojure.core.typed/Nilable java.util.Date))] 
                                 (when a (clojure.core.typed/ann-form a java.util.Date)))))

;(clj (compact [(-filter (parse-type 'Number) 0)
;               (-not-filter (Un -false -nil) 0)]
;              false))

;TODO uncomment
; See CTYP-150
#_(deftest CTYP-84-hlist-ancestor-test
  (is-tc-e (seq '(1)) 
           (NonEmptySeq Num)))

(deftest CTYP-78-finally-expected-test
  (is (check-ns 'clojure.core.typed.test.finally)))

(deftest CTYP-77-invoke-nonliteral-kw-test
  (is (check-ns 'clojure.core.typed.test.non-literal-val-fn)))

(deftest CTYP-74-malformed-TApp-test
  (is-clj (err/tc-error-thrown? (parse-type `([Any ~'-> Any])))))

(deftest CTYP-73-reduced-test
  (is-tc-e (reduced 1) (clojure.lang.Reduced Num))
  (is-tc-e @(reduced 1) Num)
  (is-tc-e (reduce (ann-form
                     (fn [a b] (if (= a b) 1 (reduced 1)))
                     [Number Number -> (U (clojure.lang.Reduced Number) Number)])
                 1 [1 2 3])
           Num))

(deftest Assoc-test
  (is-tc-e {:a 1} (Assoc '{} ':a Number))
  (is-tc-e {:a 1} (Assoc (U '{:a Number} '{:a Double}) ':a Long))
  (is-tc-e (fn [a] (assoc a 1 2))
           (All [[x :> (Map Nothing Nothing) 
                  :< (Map Num Num)]]
              [x -> (Map Num Num)]))
  (is-tc-e (fn [a] (assoc a :a 1)) 
           (All [[x :> (Map Nothing Nothing) :< (Map Any Any)]] 
              [x -> (Assoc x ':a Num)]))
  (is-tc-e (let [f :- (All [[x :< (Map Any Any)]] 
                           [x -> (Assoc x ':a Number)])
                     (fn [a] (assoc a :a 1))]
             (f {:b 1}))
           '{:b Num :a Num})
  (is-tc-e (fn [a] (assoc a :a 1)) 
           (All [[x :< (Map Any Any)]] 
             [x -> (Assoc x ':a Number)]))
  (is-tc-e (let [add-a :- (All [[x :< (Map Any Any)]]
                               [x -> (Assoc x ':a Number)])
                 #(assoc % :a 1)
                 _ :- '{:a Num}, (add-a {})
                 _ :- Num, (-> (add-a {}) :a)]))
  (is-tc-e {:a 1 :b 2}
           (U (Assoc '{} ':a Num)
              (Assoc '{} ':b Num)
              (Assoc '{} ':a Num ':b Num))))

;(clj
;  (parse-filter 
;    '(&
;      (when
;        (! (U nil false) b69880)
;        (& (is clojure.core.typed/NonEmptyCount b) (! nil b)))
;      (! (U nil false) b69880)
;      (is clojure.core.typed/NonEmptyCount a)
;      (when
;        (is (U nil false) a69879)
;        (is (U clojure.core.typed/EmptyCount nil) a))
;      (! nil a)
;      (! (U nil false) a69879)
;      (when
;        (is (U nil false) b69880)
;        (is (U clojure.core.typed/EmptyCount nil) b)))))
;
;(clj
;  (parse-filter 
;    '(&
;      (when (! (U nil false) b69880)
;        (& (is clojure.core.typed/NonEmptyCount b) (! nil b)))
;      (! (U nil false) b69880))))
;
;(clj
;  (compact
;    [(parse-filter '(when (! (U nil false) b69880)
;                      (& (is clojure.core.typed/NonEmptyCount b) (! nil b))))
;     (parse-filter '(! (U nil false) b69880))]
;    false))

(deftest hvec-ops
  (is-tc-e (first [1 'a]) 
           Number)
  (is-tc-e {:a 1} 
           (Map Keyword Number))
  (is-tc-e {:a 1} 
           (I (Map Keyword Number)
                        (ExactCount 1)))
  (is-tc-e {:a 1} 
           (NonEmptySeqable '[Keyword Number]))
  (is-tc-e ((inst first '[Keyword Number]) {:a 1})
           '[Keyword Number])
  (is-tc-e (let [first (ann-form first
                                (clojure.core.typed/All [x]
                                  [(NonEmptySeqable x) -> x]))]
             (first {:a 1}))
           '[Keyword Number])
  (is-tc-e (first (ann-form {:a 1} (NonEmptySeqable '[Keyword Number])))
           '[Keyword Number])
  (is-tc-e (first (ann-form {:a 1} (I (ExactCount 1) (Map Keyword Number))))
           '[Keyword Number])
  (is-tc-e (first (ann-form {:a 1} (I (ExactCount 1) (clojure.lang.APersistentMap Keyword Number))))
           '[Keyword Number])
  (is-tc-e (first {:a 1})
           '[Keyword Number])
  (is-cf (-> {:a 1} first second) Number)
  )

(deftest variable-hvec-test
  (is (sub?-q `'[Num Num Num] `'[Num Num ~'*]))
  (is (sub?-q `'[Num Num Num] `'[Num Num ~'*]))
  (is (sub? '[Number Number Integer *] '[Number Number *]))
  (is (not (sub? '[Number Number clojure.lang.Symbol *] '[Number Number *])))
  (is-tc-e [1 2 3] '[Num Num *])
  (is-tc-e [1 2 3] '[Num Num Num Num *])
  (is-tc-e (first [1 'a 2]) Num)
  (is-tc-e (second [1 2 3]) Num))

(deftest CTYP-85-abo-test
  (is-tc-e (fn [] (fn [b] b))
           [-> [Any -> Any]]))

(deftest array-reflection-test
  (is-tc-e (fn make-process [script]
             {:post [%]}
             (let [^Runtime r (Runtime/getRuntime)
                   _ (assert r)
                   ^"[Ljava.lang.String;" arr (into-array> String ["echo 'hello'"])]
               (.exec r arr)))
           [String -> java.lang.Process]))

(deftest expected-IPersistentMap-test
  (is-tc-e {:a #(+ %1 %2)}
           (Map Any [Num Num -> Num])))

;(reset-caches)

;(chk/abstract-result
;  (ret (-hvec [-any] :filters [(-FS (-filter (parse-clj 'Number) 'a) -top)] :objects [(->Path nil 'a)])
;       (-FS (-filter (parse-clj 'Number) 'a) -top))
;  ['a])
;
;(ety (fn [a] [a]))
;
;(impl/with-clojure-impl
;  (update -any (-filter (-val clojure.core.typed.test.mm.FooRec) 'arg [(->ClassPE)])))


;TODO support (some #{...} coll)
;TODO (apply == (non-empty-seq))
;TODO tests for inferring upper/lower bounds

(deftest defn>-test
  (is-tc-e (defn> add-two :- Int [a :- Int]
             (+ a 2))
           (Var1 [Int -> Int]))
  (is-tc-e (defn> add-three 
             (:- Int 
              [a :- Int]
              (+ a 3)))
           #_(Var1 [Int -> Int])))

(deftest def>-test
  (is (check-ns 'clojure.core.typed.test.def-arrow))
  (is-tc-e (def> a :- Num 1)
           (Var1 Num)))

(deftest nested-keyword-update-test
  ; ordinary IPersistentMap does not get updated
  (is-tc-e (fn []
             (let [a :- (Map Any Any) {}]
               (if (number? (-> a :a :b))
                 a
                 (assert nil))))
           [-> (Map Any Any)])
  ; HMaps can gain "one level" of known entries.
  (is-tc-e (fn []
             (let [a :- '{} {}]
               (if (number? (-> a :a :b))
                 a
                 (assert nil))))
           [-> (HMap :optional {:a Any})])
  ; update a (HMap) with (is clojure.core.typed/Any a [(Key :a) (Key :b)])
  ; returns a (HMap :optional {:a clojure.core.typed/Any})
  ; Only one level is updated, we can't say any more about the inner
  ; :b key.
  (is-clj  (let [t (parse-clj '(HMap))
                 path [(-kpe :a) (-kpe :b)]
                 lo+ (-filter (parse-clj 'Number) 'a path)
                 lo- (-not-filter (parse-clj 'Number) 'a path)
                 expected+ (parse-clj `(HMap :optional {:a Any}))
                 expected- (parse-clj `(HMap :optional {:a Any}))]
             (and (both-subtype? (update t lo+) expected+)
                  (both-subtype? (update t lo-) expected+))))
  ; negative absent keys. The absent entry :a is not a Number (KeyPE does not support defaults), so we
  ; just return the original type
  (is-clj (let [t (parse-type `(HMap :absent-keys #{:a}))]
            (= t
               (update t (-not-filter (RClass-of Number) 'a [(-kpe :a) (-kpe :b)])))))

  ; When we update a (HMap) that has no information about an :a key, sometimes we can prove
  ; the updated type always has an :a key.
  ;
  ; Here we restrict to a '{:a Number} because the path is a Number, which is never nil. We assume
  ; nil is the not-found type.
  (is-clj (let [t (parse-type `(HMap))]
            (both-subtype? (parse-type `(HMap :mandatory {:a Num}))
                           (update t (-filter (RClass-of Number) 'a [(-kpe :a)])))))

  ; We restrict (HMap) to (HMap :optional {:a clojure.core.typed/Any}), which is slightly less accurate, because
  ; we can't prove that the HMap :a entry is never nil. 
  (is-clj (let [t (parse-type '(HMap))]
            (both-subtype? (parse-type `(HMap :optional {:a Any}))
                           (update t (-not-filter (RClass-of Number) 'a [(-kpe :a)]))))))


(deftest poly-inst-scoping-test
  (is-tc-e (fn [a] (inst identity foo))
           (All [foo] [Any -> Any]))
  (is-tc-e
    (fn [f coll]
      (fn
        [x :- a
         y :- (Seqable b)]))
    (All [a b] [Any Any -> Any]))
  ;zero args is fine for dotted vars
  (is-tc-e (fn [a] (inst a))
           [(All [b ...] [b ... b -> Any]) -> Any]))

(deftest infer-bounds-test
  (is (= (infer-bounds -any nil)
         (infer-bounds -any -nothing)
         (infer-bounds nil nil)
         (-bounds -any -nothing)))
  (is-clj (let [t (parse-type `(Seq Num))]
            (= (infer-bounds t nil)
               (-bounds t -nothing)))))

(deftest consistent-variance-test
  (is-clj (let [t (parse-type `(TFn [[x# :variance :covariant]] x#))]
            (TypeFn-body* (TypeFn-fresh-symbols* t) t)
            true))
  (is-clj (let [t (parse-type `(TFn [[x# :variance :contravariant]] Any))]
            (TypeFn-body* (TypeFn-fresh-symbols* t) t)
            true)))

(deftest hvec-abstract-test
  (is-tc-e (fn [a b] [(class a) (class b)])
           [Any Any
            -> (HVec [(U nil Class) (U nil Class)]
                     :objects [{:path [Class], :id 0} {:path [Class], :id 1}])]))

; just a sanity check so keyword arguments don't accidentally break
(deftest check-ns-kw-args-test
  (is (check-ns 'clojure.core.typed.test.interop :collect-only true)))

;(sub? (clojure.core.typed/All [x] (TFn [[a :variance :covariant]] clojure.core.typed/Any))
;      (Rec [m] (TFn [[a :variance :covariant]] m)))

(deftest nested-tfn-test
  (is (check-ns 'clojure.core.typed.test.nested-tfn-operator)))

(deftest parse-forbidden-rec-test
  (is-clj (throws-tc-error?
            (parse-type `(Rec [x#] x#))))
  (is-clj (throws-tc-error?
            (parse-type `(Rec [x#] (I x# Number)))))
  (is-clj (throws-tc-error?
            (parse-type `(Rec [x#] (U x# Number)))))
  (is-clj (throws-tc-error?
            (parse-type `(Rec [x#] (U (I x# Number) Double))))))

(deftest parse-value-test
  (is-clj (throws-tc-error?
            (parse-type `(Value))))
  (is-clj (throws-tc-error?
            (parse-type `(Value 1 2 3))))
  (is-clj (throws-tc-error?
            (parse-type `a)))
  (is-clj (throws-tc-error?
            (parse-type ':a)))
  (is-clj (throws-tc-error?
            (parse-type '1))))

(deftest parse-TFn-bad-args-test
  (is-clj (throws-tc-error?
            (parse-type `(TFn [[x# :variance :covariant :argh]] Any)))))

(deftest parse-HMap-bad-args-test
  (is-clj (throws-tc-error?
            (parse-type `(HMap :foo))))
  (is-clj (throws-tc-error?
            (parse-type `(HMap :foo :foo))))
  (is-clj (throws-tc-error?
            (parse-type `(HMap :mandatory {} :mandatory {}))))
  (is-clj (throws-tc-error?
            (parse-type `(HMap ~'mandatory {})))))

(deftest hmap-intersection-test
  (is-tc-e {:a 1} 
           (I '{} '{:a Num}))
  (is-tc-e {:a 1 :b 2} 
           (I '{:b Num} '{:a Num}))
  (is-tc-e {:foo 3 :bar "hi"} 
           (I '{:foo Int} 
              '{:bar Str}))
  (is-tc-e {:a 1 :b 2} 
           (I '{:b Num} 
              '{:a Num}))
  (is-tc-e-with-aliases 
    {:a 1 :b 2} (I clojure.core.typed.test.util-aliases/HMapAlias1 
                   clojure.core.typed.test.util-aliases/HMapAlias2))
  (is-tc-e-with-aliases
    {:foo 3 :bar "hi"}
    (I clojure.core.typed.test.util-aliases/HMapAliasInt1 
       clojure.core.typed.test.util-aliases/HMapAliasStr2)))

(deftest rclass-invariant-test
  (is-clj
    (subtype? 
      (RClass-of 'clojure.lang.ChunkBuffer
                 [(RClass-of 'java.lang.Number)])
      (RClass-of 'clojure.lang.ChunkBuffer
                 [(Name-maker 'java.lang.Number)]))))

(deftest protocol-method-ann-test
  (is-clj (let [x1 (gensym 'x1)
                x2 (gensym 'x2)
                names [x1 x2]
                bnds [no-bounds no-bounds]
                mt (with-bounded-frees (zipmap (map make-F names)
                                               bnds)
                     (parse-type `(All [m1#]
                                    [Any ~x1 m1# ~'-> ~x2])))]
            (both-subtype? (collect-u/protocol-method-var-ann
                             mt names bnds)
                           (parse-type 
                             `(All [x1# x2# m1#]
                                [Any x1# m1# ~'-> x2#]))))))

(deftest deftype-poly-ancestor-test
  (is (check-ns 'clojure.core.typed.test.protocol-scoping)))

(deftest map-predicate-test
  (is-tc-e (fn [a] (number? (:k a)))
           (Pred (HMap :mandatory {:k Number})))
  (is-tc-err (fn [a] (integer? (:k a)))
             (Pred (HMap :mandatory {:k Number})))
  (is-tc-err (fn [a] (number? (:wrong-key a)))
             (Pred (HMap :mandatory {:k Number})))
  (is 
    (sub?-q
      `(IFn [Any ~'-> Boolean 
             :filters {:then (~'is Number 0 [(~'Key :k)]), 
                       :else (~'! Number 0 [(~'Key :k)])}])
      `(Pred (HMap :mandatory {:k Number}))))

  (is 
    (not
      (sub?-q
        `(IFn [Any ~'-> Boolean 
               :filters {:then (~'is Long 0 [(~'Key :k)]), 
                         :else (~'! Long 0 [(~'Key :k)])}])
        `(Pred (HMap :mandatory {:k Number})))))

  (is-clj 
    (sub/subtype-type-filter?
      (parse-filter `(~'is Number 0 [(~'Key :k)]))
      (parse-filter `(~'is (HMap :mandatory {:k Number}) 0))))

  (is-clj 
    (sub/subtype-not-type-filter?
      (parse-filter `(~'! Number 0 [(~'Key :k)]))
      (parse-filter `(~'! (HMap :mandatory {:k Number}) 0)))))

(deftest function-as-ifn-test
  (is (sub? [-> nil] clojure.lang.IFn))
  (is (sub? [-> nil] Callable))
  (is (sub? [-> nil] Runnable)))

(deftest swap!-special-test
  (is (check-ns 'clojure.core.typed.test.swap-bang)))

(deftest seqable-map-test
  (is-tc-e (map (fn [[a b] :- '[Number Number]]
                  (+ a b))
                {1 2 3 4 5 6})))

(deftest mapentry-first-test
  (is-tc-e (first (first {1 2}))
           Num))

(deftest CTYP-101-mapentry-test
  (is (check-ns 'clojure.core.typed.test.CTYP-101-mapentry)))

(deftest demunged-protocol-method-test
  (is (check-ns 'clojure.core.typed.test.protocol-munge)))

(deftest csgen-hmap-test
  ; (HMap :mandatory {:a Number :b Number} :complete? true) :!< (HMap :mandatory {:a x} :complete? true)
  (is-tc-err
    (letfn>
      [take-map :- (All [x] [(HMap :mandatory {:a x} :complete? true) -> x])
       (take-map [a] (:a a))]
      (take-map {:a 1 :b 2})))
  ; (HMap :mandatory {:a Number}) :!< (HMap :mandatory {:a x} :complete? true)
  (is-tc-err
    (letfn>
      [take-map :- (All [x] [(HMap :mandatory {:a x} :complete? true) -> x])
       (take-map [a] (:a a))]
      (take-map (ann-form 
                  {:a 1}
                  '{:a Number})))))

(deftest subtype-hmap-optional-test
  (is (sub?-q
        `(HMap :mandatory {:a Number})
        `(U (HMap :mandatory {:a Number})
            (HMap :absent-keys [:a]))))
  (is (sub?-q
        `(HMap :mandatory {:a Number})
        `(HMap :optional {:a Number})))
  (is (not
        (sub?-q
          `(HMap :complete? true :mandatory {:a Number :b Any})
          `(HMap :complete? true :mandatory {:a Number}))))
  (is (sub?-q
        `(HMap :complete? true :optional {:a Number :b Any})
        `(HMap :complete? true :optional {:a Number})))
  (is (not
        (sub?-q
          `(HMap :optional {:a Number})
          `(HMap :mandatory {:a Number}))))
  (is (not
        (sub?-q
          `(HMap :optional {:b Number})
          `(HMap :optional {:a Number}))))
  (is (not
        (sub?-q
          `(HMap :optional {:a Any})
          `(HMap :optional {:a Number}))))
  (is (not
        (sub?-q
          `(HMap :mandatory {:a Number})
          `(ExactCount 1))))
  (is (sub?-q
        `(HMap :complete? true :mandatory {:a Number})
        `(ExactCount 1)))
  (is (not
        (sub?-q
          `(HMap :complete? true 
                 :mandatory {:foo Any}
                 :optional {:a Number})
          `(ExactCount 1))))
  (is (sub?-q
        `(HMap :complete? true
               :mandatory {:foo Number})
        `(Map Any Num)))
  (is (not
        (sub?-q
          `(HMap :complete? true
                 :mandatory {:foo Number}
                 :optional {:bar Any})
          `(Map Any Num))))

  (is (sub?-q 
        `(U (HMap :mandatory {:foo Number}
                 :complete? true)
           (HMap :complete? true))
        `(HMap :optional {:foo Number})))
  (is (sub?-q
        `(U (HMap :mandatory {:c Number}
                  :optional {:b Number :a Number}
                  :complete? true)
            (HMap :optional {:b Number :c Number}
                  :complete? true))
        `(HMap :optional {:a Number :b Number :c Number})))
  (is (sub?-q
        `(U (HMap :mandatory {:c (Value 5)} 
                  :complete? true) 
            (HMap :complete? true))
        `(HMap :optional {:c (Value 5)} :complete? true)))
  (is (sub?-q
        `(U (HMap :mandatory {:c (Value 5)} 
                 :optional {:b Str :a Num} 
                 :complete? true) 
           (HMap :mandatory {} 
                 :optional {:b Str :a Num} 
                 :complete? true))
        `(HMap :optional {:a Num :b Str :c (Value 5)} 
               :complete? true)))
  (is (sub?-q
        `(HMap :optional {:b Str :a Num})
        `(HMap :optional {:a Num})))
  (is (not
        (sub?-q
          `(HMap :optional {:a Number})
          `(HMap :optional {:b String :a Number}))))
  )

(deftest hmap-expecteds-infer-test
  (is-tc-e {:a (fn [a] (+ a 1))}
           (HMap :mandatory {:a [Number -> Number]}))
  (is-tc-e {:a (fn [a] (+ a 1))}
           (HMap :optional {:a [Number -> Number]}))
  (is-tc-e {:a (fn [a] (+ a 1))}
           (Map Any [Number -> Number]))
  )

(deftest hmap-optional-get-test
  (is-tc-err (let [m :- (HMap :optional {:a Number})
                   {:a 1}]
               (get m :a))
             Num
             ;:ret (ret (Un (-name `Num) -nil))
             )
  (is-tc-e (get (ann-form
                  {:a 1}
                  (HMap :optional {:a Number}))
                :a)
           (U nil Number)))

(deftest datatype-variance-test
  (is (check-ns 'clojure.core.typed.test.variance-test)))

(deftest rec-type-test
  (is-clj (sub?-q
            `(HMap :mandatory {:a [Any :-> Any]} :complete? true)
            `(Rec [x#] (Map Any (U [Any :-> Any] x#)))))
  (is-clj (sub?-q
            `(HVec [[Any :-> Any]])
            `(Rec [x#] (Vec (U [Any :-> Any] x#)))))
  (is-clj (sub?-q
            `(Vec [Any :-> Any])
            `(Rec [x#] (Vec (U [Any :-> Any] x#)))))
  (is-clj (sub?-q
            `(Vec ':a)
            `(Rec [x#] (Vec (U ':a x#)))))
  (is-clj (not
            (sub?-q
              nil
              `(Rec [x#] (Vec (U ':a x#))))))
  (is (check-ns 'clojure.core.typed.test.rec-type)))

(deftest poly-rec-type-test
  ; without Rec type
  (is-tc-e
    (letfn> 
      [pfoo :- (All [x] [(Map Any x) -> (Seq x)])
       (pfoo [a] (vals a))]
      (pfoo
        (ann-form
          {}
          (Map Any Number)))))
  ; with Rec type, but non-polymorphic function
  (is-tc-e
    (letfn> 
      [pfoo :- [(Map Any Any) -> (Seq Any)]
       (pfoo [a] (vals a))]
      (pfoo
        (ann-form
          {}
          (Rec [x] (Map Any (U Number x)))))))
  ; with Rec type, polymorphic function
  (is-tc-e
    (letfn> 
      [pfoo :- (All [x] [(Map Any x) -> (Seq x)])
       (pfoo [a] (vals a))]
      (pfoo
        (ann-form
          {}
          (Rec [x] (Map Any (U Number x)))))))
  )

(deftest profile-fail-test
  ; ensure check-ns still runs even if timbre isn't loaded
  (is (check-ns-info 'clojure.core.typed.test.destructure
                     :profile true)))

; CTYP-105
(deftest hmap-absent-and-optional-subtype-test
  (is (sub?-q `(HMap :absent-keys #{:a})
              `(HMap :optional {:a Any})))
  (is (check-ns 'clojure.core.typed.test.ctyp105)))

(deftest trampoline-test
  (is (check-ns 'clojure.core.typed.test.trampoline)))

(deftest polydots-unparse-test
  (is-clj (= '[a b ...]
             (second
               (unparse-type
                 (parse-type
                   '(clojure.core.typed/All [a b ...])))))))

(deftest ignore-unsafe-cast-test
  (is-tc-e (unsafe/ignore-with-unchecked-cast
             (fn [] (+ 'a 1))
             String)
           String))

(deftest Get-test
  ;resolve
  (is-clj (= (fully-resolve-type (parse-clj `(Get '{:a Number} ':a)))
             (fully-resolve-type (parse-clj `Number))))
  (is-clj (both-subtype? (parse-clj `Number)
                         (parse-clj `(Get '{:a Number} ':a))))
  (is-tc-e 1 (Get '{:a Number} ':a))
  (is-tc-e (fn [a] (inc a)) 
           (Get '{:a [Number -> Number]} ':a))
  (is-tc-e (fn [a] (deref a))
           [(Get '{:a (clojure.core.typed/Atom1 Number)} ':a)
            -> Number])
  )

(deftest apply-hmap-test
  (is-tc-e (apply hash-map [:a 1 :b 2])
           (HMap :mandatory {:a Number
                             :b Number}
                 :complete? true)))

(deftest HVec-parse-ast-test
  (is (clojure.core.typed.parse-ast/parse-clj `(HVec [Number])))
  (is (clojure.core.typed.parse-ast/parse-clj `(HVec [Number]))))

(deftest hetergeoneous-parse-ast-test
  (is (clojure.core.typed.parse-ast/parse-clj '(List* Number)))
  (is (clojure.core.typed.parse-ast/parse-clj `(HSeq [Number])))
  (is (clojure.core.typed.parse-ast/parse-clj `(HVec [Number])))
  )

;(deftest collect-on-eval-test
;  (is (do (ann foo-bar Number)
;          (cf (def foo-bar 1))
;          (cf foo-bar)
;          true)))

;(deftest parse-with-inferred-variance
;  (is-clj (= (clj (parse-type '(TFn [[x :variance :inferred]] x)))
;             (parse-type '(TFn [[x :variance :covariant]] x)))))

;(sub? (TFn (Rec [m]
;                     (TFn [[x :variance :covariant]]
;                       (Rec [c]
;                         (IColl max-arg m c))))))

; test cast CTYP-118
(deftest cast-test
  (is (check-ns 'clojure.core.typed.test.CTYP-118-cast))
  (is-tc-err (fn [] (cast "a" "a")))
  (is-tc-err (fn [] (cast String "a" 1)))
  (is-tc-err (fn [] (cast #('ok) 2))))

(deftest optional-record-keys-test
  (is (check-ns 'clojure.core.typed.test.record-optional-key))
  (is (err/top-level-error-thrown?
        (check-ns 'clojure.core.typed.test.fail.record-no-nil)))
  (is (check-ns 'clojure.core.typed.test.record-poly-map-ctor)))

(deftest recur-rest-args-test
  (is (check-ns 'clojure.core.typed.test.recur-rest-arg))
  (is (err/top-level-error-thrown?
        (check-ns 'clojure.core.typed.test.fail.recur-non-seq-rest)))
  (is (err/top-level-error-thrown?
        (check-ns 'clojure.core.typed.test.fail.recur-empty-seq))))

(deftest poly-record-test
  (is (check-ns 'clojure.core.typed.test.poly-record)))

(deftest polymorphic-hmap-test
  (is-cf (clojure.core.typed/letfn>
           [f :- (clojure.core.typed/All [m]
                      [(HMap 
                         :mandatory {:m m}
                         :optional {:o Number}) 
                       -> clojure.core.typed/Any])
            (f [a])]
           (f {:m 1 :o 2}))))

(deftest ctyp97-tvar-scoping-test
  (is (check-ns 'clojure.core.typed.test.ctyp97-tvar-scoping)))

;TODO
;(deftest ctyp124)

(deftest get-bounded-tvar-test
  (is (check-ns 'clojure.core.typed.test.get-bounded-tvar)))

(deftest promise-test
  (is (check-ns 'clojure.core.typed.test.promise)))

(deftest pred-scoping-test
  (is (check-ns 'clojure.core.typed.test.pred-scoping)))

(deftest hvec-count-test
  (is (not
        (sub?-q `(I (CountRange 1) (HVec [clojure.core.typed/Any ~'*]))
                `(CountRange 0 0)))))

(deftest annotate-user-defined-polydot
  (is-tc-e (fn [x & y] x) 
           (All [x y ...] [x y ... y -> x]))
  (is-tc-e (fn [f a] (f a))
           [(All [x] [(HSequential [x *]) -> x])
            (HSequential [Any *]) -> Any])
  (is-tc-e (fn [a] (first a)) 
           [(I (CountRange 1) (HVec [Any *])) -> Any])
  (is-tc-e (fn [a] (first a)) 
           [(I (CountRange 1) (HSequential [Any *])) -> Any])
  (is-tc-e (fn [& y] (when-not (empty? y) (first y))) 
           (All [x y] [y * -> (U nil y)]))
  (is-tc-e (fn [x & y] x) 
           (All [a b ...] [a b ... b -> a]))

  (is (check-ns 'clojure.core.typed.test.hsequential))

  (is-tc-err (fn [x c & y] x) 
             (All [x y ...] [x y ... y -> x])))

(deftest kw-args-seq-complete-test
  (is-tc-err
    (apply concat {:a 1 :b 2})
    (Seq Keyword))
  (is-tc-e (apply concat {:a 1 :b 2})
           (Seq (U Keyword Number)))
  (is (subtype? (-kw-args-seq :mandatory {(-val :a) (-val 1)}
                              :complete? true)
                (parse-clj `(Seq (U Keyword Number))))))

(deftest add-HSequential
  (is-tc-e [1 2 3] (HSequential [Number *]))
  (is-tc-e '(1 2 3) (HSequential [Number *]))
  (is-tc-e '(1 2 3) (HSequential [(Value 1) (Value 2) (Value 3)])))

(deftest nilable-non-empty-rest-args-test
  (is-tc-e (fn [& args]
             (ann-form args (U nil (NonEmptySeq Any))))))

(deftest fail-on-reflection-test
  (is (caught-top-level-errors #{4}
        (check-ns 'clojure.core.typed.test.fail.reflection))))

(deftest tc-ignore-test
  (is-tc-e (fn [] (tc-ignore (+ 'a 1)))))

(deftest loop-macro-test
  (is-tc-e (fn [] (loop [a 1] (recur a))))
  (is-tc-e (fn [] (loop [a :- Number 1] (recur a))))
  (is-tc-err
    (fn []
      (loop [a :- Symbol 1] (recur a)))))

(deftest nth-test
  (is-tc-err
    (fn []
      (nth #{1 2} 0)))
  (is-tc-err
    (fn []
      (nth (ann-form #{1 2} (Seqable clojure.core.typed/Any)) 
           0)))
  (is-tc-err
    (fn []
      (nth (ann-form [1 2] (Seqable clojure.core.typed/Any)) 
           0)))
  (is-tc-err
    (fn [] (nth {1 0} 0)))
  (is-tc-e (nth [1] 0))
  (is-tc-e (nth '(1) 0))
  (is-tc-e (nth "a" 0))
  (is-tc-e (let [nth (ann-form nth 
                               (clojure.core.typed/All [x y] 
                                 [(U (clojure.lang.Indexed x) 
                                     (I clojure.lang.Sequential (Seqable x))) 
                                  Int -> clojure.core.typed/Any]))]
             (nth "a" 0)))
  (is-clj (do
            (dotimes [_ 100]
              (cs-gen #{'x} {'x no-bounds} {}
                      (-val "a")
                      (Un (RClass-of clojure.lang.Indexed [(make-F 'x)])
                          (In (RClass-of clojure.lang.Sequential) 
                              (RClass-of clojure.lang.Seqable [(make-F 'x)])))))
            true))
  (is-clj (some
            #{(RClass-of clojure.lang.Indexed [-any])}
             (mapv fully-resolve-type (RClass-supers* (RClass-of 'java.util.ArrayList)))))
  (is (sub?-q `java.util.ArrayList 
              `(clojure.lang.Indexed Any)))
  (is-tc-e (nth (ann-form (java.util.ArrayList. [1 2 3]) (java.util.RandomAccess clojure.core.typed/Any)) 0))
  (is-tc-e (nth (java.util.ArrayList. [1 2 3]) 0))
  ; this used to fail randomly
  (is (do (dotimes [n 20]
            (tc-e (let [nth (ann-form 
                              nth (clojure.core.typed/All [x y] 
                                       [(U (clojure.lang.Indexed x) 
                                           (I clojure.lang.Sequential (Seqable x))) 
                                        Int -> clojure.core.typed/Any]))]
                    (nth "a" 0))))
          true))
  (is (apply = (for [n (range 20)]
                 (clj
                   (cs-gen #{}
                           {'x no-bounds
                            'y no-bounds}
                           {}
                           (parse-clj `(Value "a") )
                           (with-bounded-frees {(make-F 'x) no-bounds}
                             (parse-clj `(U (I clojure.lang.Sequential (clojure.lang.Seqable ~'x)) (clojure.lang.Indexed ~'x))))))))))

(deftest nested-poly-test
  (is (Poly* ['a] [no-bounds]
             (Poly* ['x] [no-bounds] -any)))
  (is (Poly* ['a] [no-bounds]
             (PolyDots* ['x] [no-bounds] -any)))
  (is (PolyDots* ['a] [no-bounds]
             (Poly* ['x] [no-bounds] -any)))
  (is (parse-clj '(clojure.core.typed/All [b] [clojure.core.typed/Any -> (clojure.core.typed/All [b ...] [clojure.core.typed/Any -> clojure.core.typed/Any])])))
  (is (parse-clj '(clojure.core.typed/All [b] [b -> (clojure.core.typed/All [b ...] [b ... b -> clojure.core.typed/Any])])))
  (is (parse-clj '(clojure.core.typed/All [b ...] [b ... b -> (clojure.core.typed/All [b ...] [b ... b -> clojure.core.typed/Any])])))
  (is (parse-clj '(clojure.core.typed/All [b ...] [b ... b -> (clojure.core.typed/All [b ...] '[])]))))

(deftest instantiate-polydots-test
  (is (let [sym (gensym)]
        (= sym
           (-> (PolyDots-body* [sym] (parse-clj '(clojure.core.typed/All [b ...] ['[b ... b] -> clojure.core.typed/Any])))
               :types
               first
               :dom
               first
               :drest
               :name))))
  (is (= (-> (parse-clj '(clojure.core.typed/All [b ...] ['[b ... b] -> clojure.core.typed/Any]))
             :scope
             :body
             :types
             first
             :dom
             first
             :drest
             :name)
         0)))

(cf (tc/fn [f :- (clojure.core.typed/All [b ...]
                   [-> [b ... b -> clojure.core.typed/Any]])] 
      (f)))
(cf (tc/fn [f :- (clojure.core.typed/All [b ...]
                   ['[b ... b] ... b -> [b ... b -> clojure.core.typed/Any]])] 
      (f [1 2] [1 2])))
(cf (tc/fn [f :- (clojure.core.typed/All [b ...]
                   [-> (HVec [b ... b])])] 
      (f)))
(cf (tc/fn [f :- (clojure.core.typed/All [b ...]
                   [-> (HSequential [b ... b])])] 
      (f)))

#_(check-form* '(do (ns unit-test.blah
                    (:require [clojure.core.typed :as t]))
                  #_(t/ann-form (+ 1 1) Number)))

#_(cf (ns unit-test.blah
      (:require [clojure.core.typed :as t])))
#_(cf @#'clojure.core/*loaded-libs*
    (clojure.core.typed/Ref1 (clojure.core.typed/Set clojure.lang.Symbol)))


#_(tc-e nil)

#_(cf @((inst ref Number) 1))

;FIXME
#_(deftest first-class-poly-test
  (is-tc-err
    (fn [f] (second (f [1 2])))
    [(clojure.core.typed/All [b]
          [b -> b])
     -> '1])
  ; ensure b ... b does not leak into the return type
  (is (cf (fn [f] (f))
          [(clojure.core.typed/All [b ...]
                [-> [b ... b -> clojure.core.typed/Any]])
           -> clojure.core.typed/Any]))
  (is
    (tc-e 
      (do (ann ^:no-check foo (clojure.core.typed/All [a b ...] [-> '[a *]]))
          (def foo)
          (fn []
            (foo)))))
  (is
    (tc-e 
      (do (ann ^:no-check foo (clojure.core.typed/All [b] [-> '[b *]]))
          (def foo)
          (fn []
            (foo)))))
  (is
    (tc-e 
      (do (ann ^:no-check foo (clojure.core.typed/All [b] [-> [b * -> clojure.core.typed/Any]]))
          (def foo)
          (fn []
            (foo)))
      [-> [clojure.core.typed/Any * -> clojure.core.typed/Any]]))
  (is
    (tc-e 
      (do (ann ^:no-check foo (clojure.core.typed/All [b ...] [-> [b ... b -> clojure.core.typed/Any]]))
          (def foo)
          (fn []
            (foo)))
      [-> [clojure.core.typed/Any * -> clojure.core.typed/Any]]))
  (is
    (= 
      (tc-e 
        (do (ann ^:no-check foo (clojure.core.typed/All [b ...] [-> '[b ... b]]))
            (def foo)
            (fn []
              (foo))))
      (ret (parse-type '[-> '[clojure.core.typed/Any *]])
           (-true-filter)
           -empty)))
  (is
    (tc-e 
      (do (ann ^:no-check foo (clojure.core.typed/All [b ...] [-> (HSequential [b ... b])]))
          (def foo)
          (fn []
            (foo)))))
  (is
    (tc-e 
      (do (ann ^:no-check foo (clojure.core.typed/All [b ...] [-> '[b ... b]]))
          (def foo)
          (fn []
            (foo)))))
  (is (cf (fn [f] (f))
          [(clojure.core.typed/All [b ...]
                [-> '[b ... b]])
           -> clojure.core.typed/Any]))
  (is (cf (fn [f] (f [1 2]))
          [(clojure.core.typed/All [b ...]
                ['[b ... b] -> '[b ... b]])
           -> '['1 '2]]))
  (is (cf (fn [f] (f (fn [a] a)))
          [(clojure.core.typed/All [b ...]
                [[b ... b -> clojure.core.typed/Any] -> [b ... b -> clojure.core.typed/Any]])
           -> [clojure.core.typed/Any clojure.core.typed/Any -> clojure.core.typed/Any]]))
  (is (cf (fn [f] (f))
          [(clojure.core.typed/All [b ...]
                [-> [b ... b -> clojure.core.typed/Any]])
           -> clojure.core.typed/Any #_['1 '2 -> clojure.core.typed/Any]]))
  (is-tc-err
    (fn [f] (second (f [1 2])))
    [(clojure.core.typed/All [b ...]
          ['[b ... b] -> '[b ... b]])
     -> '1]))

(deftest deref-var-test
  (is-tc-e @#'+ 
           [Number -> Number]))

(deftest static-pred-test
  (is (check-ns 'clojure.core.typed.test.pred-hmap)))

(deftest infer-def-test
  (is-tc-e (do (def a 1)
               (defn b [_] 'a)
               (b a))
           Symbol))

(deftest for-test
  (is-tc-e (for [a :- Num [1 2 3]]
             (inc a))
           (Seq Any))
  (is-tc-e (for [a :- Num [1 2 3]] :- Num
             (inc a))
           (Seq Num))
  (is-tc-err (for [a [1 2 3]]
               (inc a))))

(deftest file-not-found-error-test
  (is (err/top-level-error-thrown?
        (check-ns 'this.doesnt-exist))))

(deftest CTYP146-test
  (is (check-ns 'clojure.core.typed.test.CTYP146)))

(deftest defn-test
  (is-tc-e (defn foo [a :- Number]
             (inc a))))

(deftest atom-test
  (is-tc-e @(atom 1) Any)
  (is-tc-e @(atom :- Number 1) Number))

(deftest ref-test
  (is-tc-e @(ref 1) Any)
  (is-tc-e @(ref :- Number 1) Number))

(deftest performance-CTYP83
  (is (check-ns 'clojure.core.typed.test.CTYP-83-performance)))

(deftest count-set-test
  (is-tc-e (let [v :- (Vec Int) [1 2 3]
                 _ (assert (#{1 2 3} (count v)))]
             (first v))
           Int)
  (is-tc-err (let [v :- (Vec Int) [1 2 3]
                   _ (assert (#{0 1 2 3} (count v)))]
               (first v))
             Int)
  (is-tc-e (let [v :- (Vec Int) [1 2 3]
                 _ (assert (#{1 2 3} (count v)))]
             (nth v 0 nil))
           Int)
  (is-tc-err (let [v :- (Vec Int) [1 2 3]
                   _ (assert (#{0 1 2 3} (count v)))]
               (nth v 0 nil))
             Int)
  (is-tc-e (let [v :- (Vec Int) [1 2 3]
                 _ (assert (#{1 2 3} (count v)))]
             (nth v 0))
           Int)
  ; we let nth fail at runtime here
  (is-tc-e (let [v :- (Vec Int) [1 2 3]
                 _ (assert (#{0 1 2 3} (count v)))]
             (nth v 0))
           Int))

(deftest keyword-default-arg-test
  (is-tc-e (:a {} 0) Int)
  (is-tc-e (:a {} 0) Int)
  (is-tc-err (:a {}) Int)
  (is-tc-e
    (fn [m :- (Map Keyword Long)] :- Long
      (:a m 0)))
  (is-tc-e
    (fn [m :- (Map Keyword Long)] :- Long
      (get m :a 0))))

(defn top-tfn1 []
  (parse-type `(TFn [[~'x :variance :covariant]] Any)))

(defn bot-tfn1 []
  (parse-type `(TFn [[~'x :variance :covariant]] Nothing)))

(defmacro with-hk-m [& body]
  `(with-bounded-frees {(make-F '~'m) (-bounds (top-tfn1) (bot-tfn1))}
     ~@body))

(deftest subtype-hk-app
  (is-clj (with-hk-m
            (sub?-q `(~'m Int) `(~'m Int))))
  (is-clj (with-hk-m
            (sub?-q `(~'m Int) `(~'m Num))))
  (is-clj (with-hk-m
            (sub?-q `(~'m Int) `(U (~'m Int) (clojure.lang.Reduced Int)))))
  (is-clj (with-hk-m
            (sub?-q `(~'m Int) `(U (~'m Int) (clojure.lang.Reduced Int)))))
  (is-clj (with-hk-m
            (sub?-q `(~'m Int) `(U (~'m Num) (clojure.lang.Reduced Int)))))
  )

#_(deftest reduce-test
  (is-tc-err (reduce (fn ([] :- nil) ([x :- Num y :- Num] :- nil)) [1]))
  (is-tc-e (reduce (fn ([] :- Num 1) ([x :- Num y :- Num] :- Num 1)) [1]))
  (is-tc-err (reduce (fn ([x :- Num y :- Num] :- Num 1)) [1]))
  (is-tc-err (reduce (fn ([x :- Num y :- Num] :- Num 1)) [1]))
  (is-tc-e (reduce (fn ([x :- Num, y :- Num] 1) ([] 1)) [1])))

(deftest reduced?-test
  (testing "a plain old object" (is-tc-e (reduced? :a)))
  (testing "a nil"              (is-tc-e (reduced? nil)))
  (testing "an Any"             (is-tc-e (fn [x :- Any] (reduced? x)))))

;(deftest dotted-apply-test
;  (is-tc-e
;    (do (ann foo (All [x y ...] [[y ... y -> x] -> [y ... y -> x]]))
;        (defn foo
;          [f]
;          (let [mem (memoize (fn [& args] #(apply f args)))]
;            (fn [& args]
;              ((apply mem args))))))))

(ann-form vector [Number * -> '[Number]])
#_(cf (inst vector Number Number))
#_(is (cf (juxt (inst vector clojure.core.typed/Any))))
#_(is (cf (juxt (ann-form vector [Number * -> '[Number]]))))
#_(cf (t/juxt first :- [(Seq Number) -> (U nil Number)]
              rest :- [(Seq Number) -> (Seq Number)]))

;(clojure.core.typed/All [b ...] [b ... b -> (HVec [b ... b])]) <: [java.lang.Number * -> (HVec [java.lang.Number])]
