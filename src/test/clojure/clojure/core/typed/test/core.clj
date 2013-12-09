(ns clojure.core.typed.test.core
  (:require [clojure.test :refer :all]
            [clojure.tools.analyzer :refer [ast analyze-form]]
            [clojure.tools.analyzer.hygienic :refer [ast-hy]]
            [clojure.repl :refer [pst]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.core.typed.init]
            [clojure.core.typed.utils :as u :refer [with-ex-info-handlers top-level-error?]]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.check :as chk :refer [expr-type tc-t combine-props env+ update check-funapp
                                                      tc-equiv]]
            [clojure.core.typed.collect-phase :as collect]
            [clojure.core.typed.inst :as inst]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.type-rep :refer :all]
            [clojure.core.typed.type-ctors :refer :all]
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
            [clojure.core.typed.subst :refer [subst-all]]
            [clojure.core.typed.test.rbt]
            [clojure.core.typed.test.person]
            [clojure.tools.trace :refer [trace-vars untrace-vars
                                         trace-ns untrace-ns]])
; we want clojure.lang.Seqable to be scoped here. 
; There :refer :all of clojure.core.typed adds another Seqable which
; is less useful here.
  (:use [clojure.core.typed :as tc :exclude [Seqable]])
  (:import (clojure.lang ISeq ASeq IPersistentVector Atom IPersistentMap
                         ExceptionInfo Var Seqable)))

(load-if-needed)

(defn subtype? [& rs]
  (impl/with-clojure-impl
    (apply sub/subtype? rs)))

(defn both-subtype? [s t]
  (and (subtype? s t)
       (subtype? t s)))

(defn check [& as]
  (impl/with-clojure-impl
    (apply chk/check as)))

(defmacro is-cf [& args]
  `(is (do
         (cf ~@args)
         true)))

(defmacro is-clj [& args]
  `(is (clj ~@args)))

(defmacro clj [& body]
  `(impl/with-clojure-impl ~@body))

;Aliases used in unit tests
(defmacro declare-map-aliases []
  `(do
     (cf (clojure.core.typed/def-alias clojure.core.typed.test.core/MyName ~'(HMap :mandatory {:a (Value 1)})))
     (cf (clojure.core.typed/def-alias clojure.core.typed.test.core/MapName ~'(HMap :mandatory {:a clojure.core.typed.test.core/MyName})))
     (cf (clojure.core.typed/def-alias clojure.core.typed.test.core/MapStruct1 ~'(HMap :mandatory {:type (Value :MapStruct1) 
                                                                                        :a clojure.core.typed.test.core/MyName})))
     (cf (clojure.core.typed/def-alias clojure.core.typed.test.core/MapStruct2 ~'(HMap :mandatory {:type (Value :MapStruct2) 
                                                                                        :b clojure.core.typed.test.core/MyName})))
     (cf (clojure.core.typed/def-alias clojure.core.typed.test.core/UnionName ~'(U clojure.core.typed.test.core/MapStruct1 
                                                                                   clojure.core.typed.test.core/MapStruct2)))))

(defmacro is-with-aliases [& body]
  `(is-clj (do (declare-map-aliases)
           (impl/with-clojure-impl ~@body))))

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
  (is-clj (= (Poly-body* '(x) (parse-type '(All [x] x)))
            (make-F 'x)))
  (is-clj (= (Poly-body* '(x y) (parse-type '(All [x y] x)))
             (make-F 'x)))
  (is-clj (= (Poly-body* '(x y) (parse-type '(All [x y] y)))
             (make-F 'y)))
  (is-clj (= (Poly-body* '(a b c d e f g h i) (parse-type '(All [a b c d e f g h i] e)))
             (make-F 'e))))

(deftest parse-type-fn-test
  (is-clj (= (parse-type '[nil * -> nil])
         (make-FnIntersection (make-Function () -nil -nil))))
  (is-clj (= (parse-type '(All [x ...] [nil ... x -> nil]))
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
  (is-clj (= (parse-type '(All [x x1 [y :< x] z] [x -> y]))
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
(defmacro sub? [s t]
  `(impl/with-clojure-impl
     (subtype? (parse-type '~s)
               (parse-type '~t))))

(deftest subtype-test
  (is-clj (sub? Integer Integer))
  (is-clj (sub? Integer Object))
  (is-clj (not (sub? Object Integer)))
  (is-clj (sub? Object Object))
  (is-clj (sub? Integer Number))
  (is-clj (sub? (clojure.lang.Seqable Integer)
            (clojure.lang.Seqable Integer)))
  (is-clj (sub? (clojure.lang.Seqable Integer)
            (clojure.lang.Seqable Number)))
  (is-clj (not
        (sub? (clojure.lang.Seqable Number)
              (clojure.lang.Seqable Integer))))
  (is-clj (sub? (clojure.lang.Cons Integer)
            (clojure.lang.Cons Number)))
  (is-clj (sub? (clojure.lang.Cons Integer)
            (clojure.lang.Seqable Number))))

(deftest subtype-java-exceptions-test
  (is-clj (subtype? (RClass-of IndexOutOfBoundsException nil)
                (RClass-of Exception nil))))

(deftest subtype-intersection
  (is-clj (not (subtype? (RClass-of Seqable [-any])
                     (In (RClass-of Seqable [-any])
                         (make-CountRange 1))))))

(deftest subtype-Object
  (is-clj (subtype? (RClass-of clojure.lang.IPersistentList [-any]) (RClass-of Object nil))))

(deftest subtype-hmap
  (is (sub? (HMap :mandatory {:a '1} :complete? true)
            (HMap :mandatory {:a Number} :complete? true)))
  (is (sub? (HMap :mandatory {:a '1} :complete? true)
            (HMap :mandatory {} :complete? false)))
  (is (sub? (HMap :mandatory {:a '1 :b '2 :c '3} :complete? true)
            (HMap :mandatory {:a '1 :b '2} :complete? false)))
  (is (not (sub? '{:a nil}
                 '{:a '1})))
  (is (not (sub? (HMap :mandatory {:a '1} :complete? true)
                 (HMap :mandatory {} :complete? true))))
  (is (not (sub? (HMap :mandatory {:a '1 :b '2} :complete? true)
                 (HMap :mandatory {:a '1 :b '2 :c '3} :complete? false)))))

(deftest subtype-poly
  (is-clj (subtype? (parse-type '(All [x] (clojure.lang.ASeq x)))
                    (parse-type '(All [y] (clojure.lang.Seqable y))))))

(deftest subtype-rec
  (is-clj (subtype? (parse-type 'Integer)
                    (parse-type '(Rec [x] (U Integer (clojure.lang.Seqable x))))))
  (is-clj (subtype? (parse-type '(clojure.lang.Seqable (clojure.lang.Seqable Integer)))
                    (parse-type '(Rec [x] (U Integer (clojure.lang.Seqable x))))))
  (is-clj (not (subtype? (parse-type 'Number)
                         (parse-type '(Rec [x] (U Integer (clojure.lang.Seqable x)))))))
  (is-clj (sub? (HMap :mandatory {:op (Value :if)
                  :test (HMap :mandatory {:op (Value :var)
                               :var (clojure.lang.Var Nothing Any)})
                  :then (HMap :mandatory {:op (Value :nil)})
                  :else (HMap :mandatory {:op (Value :false)})})
            (Rec [x] 
                 (U (HMap :mandatory {:op (Value :if)
                           :test x
                           :then x
                           :else x})
                    (HMap :mandatory {:op (Value :var)
                           :var (clojure.lang.Var Nothing Any)})
                    (HMap :mandatory {:op (Value :nil)})
                    (HMap :mandatory {:op (Value :false)})))))

  (is-clj (sub? (Rec [x] (U Integer (clojure.lang.ILookup x x)))
            (Rec [x] (U Number (clojure.lang.ILookup x x))))))

(deftest trans-dots-test
  (is-clj (= (inst/manual-inst (parse-type '(All [x b ...]
                                                 [x ... b -> x]))
                               (map parse-type '(Integer Double Float)))
             (parse-type '[Integer Integer -> Integer])))
  (is-clj (= (inst/manual-inst (parse-type '(All [x b ...]
                                                 [b ... b -> x]))
                               (map parse-type '(Integer Double Float)))
             (parse-type '[Double Float -> Integer])))
  ;map type
  (is-clj (= (inst/manual-inst (parse-type '(All [c a b ...]
                                                 [[a b ... b -> c] (clojure.lang.Seqable a) (clojure.lang.Seqable b) ... b -> (clojure.lang.Seqable c)]))
                               (map parse-type '(Integer Double Float)))
             (parse-type '[[Double Float -> Integer] (clojure.lang.Seqable Double) (clojure.lang.Seqable Float) -> (clojure.lang.Seqable Integer)]))))

;return ret for an expression f
(defmacro eret [f]
  `(let [ret# (-> (check-form-info '~f) :ret)]
     (assert (TCResult? ret#))
     ret#))

;return type for an expression f
(defmacro ety [f]
  `(-> (eret ~f) ret-t))

(deftest tc-invoke-fn-test
  (is-clj (subtype? (ety
                      ((clojure.core.typed/fn> [a :- Number, b :- Number] b)
                       1 2))
                    (parse-type 'Number)))
  ; manual instantiation "seq"
  (is-clj (subtype? (ety
                      ((clojure.core.typed/fn> [a :- (clojure.lang.Seqable Number), b :- Number] 
                                               ((clojure.core.typed/inst seq Number) a))
                       [1 2 1.2] 1))
                    (parse-type '(clojure.core.typed/Option (I (clojure.lang.ISeq java.lang.Number) (CountRange 1))))))
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
                    (parse-type '(U nil (I (CountRange 1) (clojure.lang.ISeq Number))))))
  (is-clj (subtype? (ety
                      ((clojure.core.typed/fn> [a :- (clojure.lang.IPersistentMap Any Number), b :- Number] 
                                               ((clojure.core.typed/inst get Number Nothing) a b))
                       (zipmap [1] [2]) 1))
                    (parse-type '(U nil Number)))))

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
         (ret (-val 2) (-FS -top -bot) (->EmptyObject))))
  (is-clj (= (tc-t (if false 1 2))
         (ret (-val 2) (-FS -top -bot) (->EmptyObject))))
  (is-clj (= (tc-t (if 1 1 2))
         (ret (-val 1) (-FS -top -bot) (->EmptyObject)))))

(deftest empty-fn-test
  (is-clj (= (tc-t (clojure.core/fn []))
         (ret (make-FnIntersection
                (Function-maker [] (make-Result -nil
                                            (-FS -bot -top)
                                            (->EmptyObject))
                            nil nil nil))
              (-FS -top -bot)
              (->EmptyObject))))
  (is-clj (= (tc-t (fn [] 1))
         (ret (make-FnIntersection
                (Function-maker [] (make-Result (-val 1)
                                              (-FS -top -bot)
                                              (->EmptyObject))
                              nil nil nil))
              (-FS -top -bot)
              (->EmptyObject))))
  (is-clj (= (tc-t (let []))
         (ret -nil (-FS -bot -top) (->EmptyObject)))))

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
         (ret 
           (parse-type 
             '(Fn [(U '{:op (Value :var)} '{:op (Value :if)}) -> (U ':var ':if) 
                   :filters {:then (is (U (Value :var) (Value :if)) 0 [(Key :op)]), 
                             :else (| (is (HMap :absent-keys #{:op}) 0) 
                                      (is (U nil false) 0 [(Key :op)]))} 
                   :object {:path [(Key :op)], :id 0}]))
           (-FS -top -bot)
           -empty))))

(deftest refine-test
  (is-clj (= (tc-t 
           (clojure.core.typed/fn> [a :- (U (HMap :mandatory {:op (Value :if)})
                                            (HMap :mandatory {:op (Value :var)}))]
                           (when (= (:op a) :if) 
                             a)))
         (ret (make-FnIntersection
                (Function-maker
                    [(Un (-hmap {(-val :op) (-val :if)})
                         (-hmap {(-val :op) (-val :var)}))]
                    (make-Result (Un -nil (-hmap {(-val :op) (-val :if)}))
                                 (-FS (-and (-filter (-val :if) 0 [(->KeyPE :op)])
                                            (-not-filter (Un -false -nil) 0)
                                            (-filter (-hmap {(-val :op) (-val :if)}) 0))
                                           ; what are these filters doing here?
                                      (-or (-and (-filter (-val :if) 0 [(->KeyPE :op)])
                                                 (-filter (Un -false -nil) 0))
                                           (-not-filter (-val :if) 0 [(->KeyPE :op)])))
                                 -empty)
                    nil nil nil))
              (-FS -top -bot)
              -empty))))


#_(deftest dotted-infer-test
  (is-cf (map number? [1])))

(deftest check-invoke
  ; wrap in thunk to prevent evaluation (analyzer currently evaluates forms)
  (is (u/top-level-error-thrown? (cf (fn [] (symbol "a" 'b)))))
  (is (both-subtype? (ety (symbol "a" "a"))
                     (clj (RClass-of clojure.lang.Symbol)))))

(deftest check-do-test
  (is-clj (= (ety (do 1 2))
         (-val 2))))

(deftest tc-var-test
  (is-clj (subtype? (ret-t (tc-t seq?))
                    (parse-type '(predicate (clojure.lang.ISeq Any))))))

(deftest heterogeneous-ds-test
  (is-clj 
    (not (subtype? (parse-type '(HMap :mandatory {:a (Value 1)}))
                     (RClass-of ISeq [-any]))))
  (is-clj 
    (not (subtype? (parse-type '(Vector* (Value 1) (Value 2)))
                     (RClass-of ISeq [-any]))))
  (is-clj
    (subtype? (parse-type '(Seq* (Value 1) (Value 2)))
                (RClass-of ISeq [-any])))
  (is-clj 
    (subtype? (parse-type '(List* (Value 1) (Value 2)))
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
  (is-clj (= (map set (combine-props [(->ImpFilter (-not-filter -false 'a)
                                               (-filter -true 'b))]
                                 [(-not-filter (Un -nil -false) 'a)]
                                 (atom true)))
         [#{} #{(-not-filter (Un -nil -false) 'a)
                (-filter -true 'b)}])))

(deftest env+-test
  ;test basic TypeFilter
  ;update a from Any to (Value :a)
  (is-clj (let [props [(-filter (-val :a) 'a)]
            flag (atom true)]
        (and (= (let [env {'a -any}
                      lenv (-PropEnv env props)]
                  (env+ lenv [] flag))
                (-PropEnv {'a (-val :a)} props))
             @flag)))
  ;test positive KeyPE
  ;update a from (U (HMap :mandatory {:op :if}) (HMap :mandatory {:op :var})) => (HMap :mandatory {:op :if})
  (is-clj (let [props [(-filter (-val :if) 'a [(->KeyPE :op)])]
            flag (atom true)]
        (and (= (let [env {'a (Un (-hmap {(-val :op) (-val :if)})
                                  (-hmap {(-val :op) (-val :var)}))}
                      lenv (-PropEnv env props)]
                  (env+ lenv [] flag))
                (-PropEnv {'a (-hmap {(-val :op) (-val :if)})} props))
             @flag)))
  ;test negative KeyPE
  (is-clj (let [props [(-not-filter (-val :if) 'a [(->KeyPE :op)])]
            flag (atom true)]
        (and (= (let [env {'a (Un (-hmap {(-val :op) (-val :if)})
                                  (-hmap {(-val :op) (-val :var)}))}
                      lenv (-PropEnv env props)]
                  (env+ lenv [] flag))
                (-PropEnv {'a (-hmap {(-val :op) (-val :var)})} props))
             @flag)))
  ;test impfilter
  (is-clj (let [{:keys [l props]}
            (env+ (-PropEnv {'a (Un -false -true) 'b (Un -nil -true)}
                             [(->ImpFilter (-not-filter -false 'a)
                                           (-filter -true 'b))])
                  [(-not-filter (Un -nil -false) 'a)]
                  (atom true))]
        (and (= l {'a -true, 'b -true})
             (= (set props)
                #{(-not-filter (Un -nil -false) 'a)
                  (-filter -true 'b)}))))
  ; more complex impfilter
  (is-with-aliases (= (env+ (-PropEnv {'and1 (Un -false -true)
                                       'tmap (Name-maker 'clojure.core.typed.test.core/UnionName)}
                                      [(->ImpFilter (-filter (Un -nil -false) 'and1)
                                                    (-not-filter (-val :MapStruct1)
                                                                 'tmap
                                                                 [(->KeyPE :type)]))
                                       (->ImpFilter (-not-filter (Un -nil -false) 'and1)
                                                    (-filter (-val :MapStruct1)
                                                             'tmap
                                                             [(->KeyPE :type)]))])
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
                (Function-maker [(-hmap {(-val :a) (-val 1)})]
                              (make-Result (-val 1) 
                                           (-FS -top -top)  ; have to throw out filters whos id's go out of scope
                                           ;(->Path [(->KeyPE :a)] 0) ; requires 'equivalence' filters
                                           -empty)
                              nil nil nil))
              (-FS -top -bot)
              -empty)))
  ;FIXME inferred filters are bit messy, but should be (-FS -bot (! Seq 0))
  #_(is-with-aliases (= (-> (tc-t (clojure.core.typed/fn> [a :- clojure.core.typed.test.core/UnionName]
                                   (seq? a)))
           ret-t)
         (make-FnIntersection
           (Function-maker [(Name-maker 'clojure.core.typed.test.core/UnionName)]
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
                (Function-maker [(-hmap {(-val :a) (-val 1)})]
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
           (make-Function [(Un (-hmap {(-val :a) (-val 1)})
                               (-hmap {(-val :b) (-val 2)}))]
                          (Un (-val 1) -any))))))

(deftest Name-resolve-test
  (is-with-aliases (= (tc-t (clojure.core.typed/fn> [tmap :- clojure.core.typed.test.core/MyName]
                                                    ;call to (apply hash-map tmap) should be eliminated
                                                    (let [{e :a} tmap]
                                                      e)))
                      (ret (make-FnIntersection 
                             (Function-maker [(Name-maker 'clojure.core.typed.test.core/MyName)]
                                         (make-Result (-val 1) (-FS -top -top) -empty)
                                         nil nil nil))
                           (-FS -top -bot) -empty)))
  (is-with-aliases (= (tc-t (clojure.core.typed/fn> [tmap :- clojure.core.typed.test.core/MapName]
                                                    (let [{e :a} tmap]
                                                      (assoc e :c :b))))
                      (ret (make-FnIntersection (Function-maker [(Name-maker 'clojure.core.typed.test.core/MapName)]
                                                            (make-Result (-hmap {(-val :a) (-val 1)
                                                                                 (-val :c) (-val :b)})
                                                                         (-FS -top -bot) -empty)
                                                            nil nil nil))
                           (-FS -top -bot) -empty)))
  ; Name representing union of two maps, both with :type key
  (is-with-aliases (subtype? 
                     (-> (tc-t (clojure.core.typed/fn> [tmap :- clojure.core.typed.test.core/UnionName]
                                                       (:type tmap)))
                         ret-t)
                     (parse-type '[clojure.core.typed.test.core/UnionName -> (U (Value :MapStruct2)
                                                                                (Value :MapStruct1))])))
  ; using = to derive paths
  (is-with-aliases (subtype? 
                     (-> (tc-t (clojure.core.typed/fn> [tmap :- clojure.core.typed.test.core/UnionName]
                                                       (= :MapStruct1 (:type tmap))))
                         ret-t)
                     (make-FnIntersection 
                       (make-Function 
                         [(Name-maker 'clojure.core.typed.test.core/UnionName)]
                         (Un -false -true)
                         nil nil
                         :filter (let [t (-val :MapStruct1)
                                       path [(->KeyPE :type)]]
                                   (-FS (-and 
                                          (-filter (-hmap {(-val :type) (-val :MapStruct1)
                                                           (-val :a) (Name-maker 'clojure.core.typed.test.core/MyName)})
                                                   0)
                                          (-filter (-val :MapStruct1) 0 path)
                                          (-filter t 0 path))
                                        (-not-filter t 0 path)))))))
  ; using filters derived by =
  (is-with-aliases (subtype? (-> (tc-t (clojure.core.typed/fn> [tmap :- clojure.core.typed.test.core/UnionName]
                                                               (if (= :MapStruct1 (:type tmap))
                                                                 (:a tmap)
                                                                 (:b tmap))))
                                 ret-t)
                             (parse-type '[clojure.core.typed.test.core/UnionName -> clojure.core.typed.test.core/MyName])))
  ; following paths with test of conjuncts
  ;FIXME
  #_(is-clj (= (tc-t (clojure.core.typed/fn> [tmap :- clojure.core.typed.test.core/UnionName]
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
         (ret (make-FnIntersection (Function-maker [(Name-maker 'clojure.core.typed.test.core/UnionName)]
                              (let [t (Un (-val 1)
                                          (-hmap {(-val :type) (-val :MapStruct1)
                                                               (-val :c) (-val :d)
                                                               (-val :a) (Name-maker 'clojure.core.typed.test.core/MyName)}))]
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
  (is-clj (= (update (Un (-hmap {(-val :type) (-val :Map1)})
                         (-hmap {(-val :type) (-val :Map2)}))
                     (-filter (-val :Map1) 'tmap [(->KeyPE :type)]))
             (-hmap {(-val :type) (-val :Map1)})))
  ;test that update resolves Names properly
  (is-with-aliases (= (update (Name-maker 'clojure.core.typed.test.core/MapStruct2)
                              (-filter (-val :MapStruct1) 'tmap [(->KeyPE :type)]))
                      (Un)))
  ;test that update resolves Names properly
  ; here we refine the type of tmap with the equivalent of following the then branch 
  ; with test (= :MapStruct1 (:type tmap))
  (is-with-aliases (= (update (Name-maker 'clojure.core.typed.test.core/UnionName)
                              (-filter (-val :MapStruct1) 'tmap [(->KeyPE :type)]))
                      (-hmap {(-val :type) (-val :MapStruct1) 
                              (-val :a) (Name-maker 'clojure.core.typed.test.core/MyName)})))
  (is-with-aliases (= (update (Name-maker 'clojure.core.typed.test.core/UnionName)
                              (-not-filter (-val :MapStruct1) 'tmap [(->KeyPE :type)]))
                      (-hmap {(-val :type) (-val :MapStruct2) 
                              (-val :b) (Name-maker 'clojure.core.typed.test.core/MyName)})))
  (is-clj (= (update (Un -true -false) (-filter (Un -false -nil) 'a nil)) 
             -false)))

(deftest overlap-test
  (is-clj (not (overlap -false -true)))
  (is-clj (not (overlap (-val :a) (-val :b))))
  (is-clj (overlap (RClass-of Number) (RClass-of Integer)))
  (is-clj (not (overlap (RClass-of Number) (RClass-of clojure.lang.Symbol))))
  (is-clj (not (overlap (RClass-of Number) (RClass-of String))))
  (is-clj (overlap (RClass-of clojure.lang.Seqable [-any]) (RClass-of clojure.lang.IMeta [-any])))
  (is-clj (overlap (RClass-of clojure.lang.Seqable [-any]) (RClass-of clojure.lang.PersistentVector [-any])))
  )

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
         (make-Result (Un (-hmap {(-val :a) (-val :b)
                                  (-val :c) (-val 1)})
                          (-hmap {(-val :b) (-val :c)
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
              -empty))))

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
                  (RClass-of ASeq [(make-F 'x)]))))) ;result
  (is-clj (clj
        (= (infer {'x no-bounds} ;tv env
                  {}
                  [(-hvec [(-val 1) (-val 2) (-val 3)])] ;actual
                  [(RClass-of Seqable [(make-F 'x)])] ;expected
                  (RClass-of ASeq [(make-F 'x)])))))) ;result

(deftest arith-test
  (is-clj (subtype? (:t (tc-t (+)))
                (RClass-of Number)))
  (is-clj (subtype? (:t (tc-t (+ 1 2)))
                (RClass-of Number)))
  ;wrap in thunks to prevent evaluation
  (is (u/top-level-error-thrown? (cf (fn [] (+ 1 2 "a")))))
  (is (u/top-level-error-thrown? (cf (fn [] (-)))))
  (is (u/top-level-error-thrown? (cf (fn [] (/))))))

(deftest tc-constructor-test
  (is-clj (= (tc-t (Exception. "a"))
         (ret (RClass-of Exception)
              (-FS -top -bot)
              (->EmptyObject)))))

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

(deftest count-subtype-test
  (is-clj (subtype? (make-CountRange 1)
                (make-CountRange 1)))
  (is-clj (not (subtype? (make-CountRange 1)
                     (make-ExactCountRange 1))))
  (is-clj (subtype? (make-ExactCountRange 1)
                (make-CountRange 1)))
  (is-clj (subtype? (make-ExactCountRange 4)
                (make-CountRange 1)))
  (is-clj (subtype? (make-ExactCountRange 4)
                (make-CountRange 0)))
  (is-clj (subtype? (make-CountRange 2)
                (make-CountRange 1)))
  )


(deftest names-expansion-test
  (is (do
        (cf (clojure.core.typed/def-alias clojure.core.typed.test.core/MyAlias
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
  (is (u/top-level-error-thrown? (cf (map + [1 2] [1 2] [4 5] [6 7] [4 4] {3 4}))))
  (is (u/top-level-error-thrown? (cf (map + [1 2] [1 2] [4 5] [6 7] [4 4] #{'a 4})))))

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
  (is-clj (subtype? (ret-t (tc-t (let [[a b :as c] (clojure.core.typed/ann-form [1 2] (clojure.lang.Seqable Number))] 
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
  (is-cf (clojure.core.typed/ann-form (clojure.core.typed/inst merge Any Any) [nil -> nil])))

(deftest poly-filter-test
  (is-clj (both-subtype? 
            (ret-t (tc-t (let [a (clojure.core.typed/ann-form [1] (clojure.core.typed/Coll clojure.core.typed/AnyInteger))]
                           (if (seq a)
                             (first a)
                             'a))))
            (parse-type '(U clojure.core.typed/AnyInteger (Value a))))))

(deftest type-fn-test 
  (is-clj (clj
        (= (with-bounded-frees {(make-F 'm) (-bounds (parse-type '(TFn [[x :variance :covariant]] Any))
                                                     (parse-type '(TFn [[x :variance :covariant]] Nothing)) )}
             (check-funapp
               (-> (ast 'a) ast-hy) ;dummy
               [(-> (ast 1) ast-hy)];dummy
               (ret (parse-type '(All [x]
                                      [x -> (m x)])))
               [(ret -nil)]
               nil))
           (ret (TApp-maker (make-F 'm) [-nil]))))))

;TODO how to handle casts. CTYP-12
;Also need tc-t to bind *delayed-errors*
#_(deftest prims-test
  (is-clj (= (ret-t (tc-t (Math/sqrt 1)))
         (parse-type 'double))))

(deftest hmap-subtype
  (is-cf {} (clojure.lang.APersistentMap Any Any)))

;; `do` is special at the top level, tc-ignore should expand out to `do`
(tc-ignore
 (defprotocol some-proto (some-proto-method [_]))
 some-proto-method)

(deftest array-test
  (is-clj (= (Class/forName "[I") 
         (class (into-array> int [1]))))
  (is-clj (clj (= (Class/forName "[Ljava.lang.Object;") 
              (class (into-array> (U nil int) [1])))))
  (is-clj (clj (= (Class/forName "[Ljava.lang.Number;") 
              (class (into-array> (U nil Number) [1])))))
  (is-clj (clj (= (Class/forName "[Ljava.lang.Object;") 
              (class (into-array> (U clojure.lang.Symbol Number) [1])))))
  (is-clj (= (Class/forName "[Ljava.lang.Object;") 
         (class (into-array> Object (U clojure.lang.Symbol Number) [1]))))
  )

(deftest class-pathelem-test
  (is-clj (= (-> (tc-t #(class %))
           ret-t :types first :rng Result-object*)
         (->Path [(->ClassPE)] 0)))
  (is-clj (subtype? (-> (tc-t 
                      #(= Number (class %)))
                  ret-t)
                (FnIntersection-maker
                  [(make-Function
                     [-any]
                     (RClass-of 'boolean)
                     nil nil
                     :filter (-FS (-and (-filter (-val Number) 0 [(->ClassPE)])
                                        ;the important filter, updates first argument to be Number if predicate is true
                                        (-filter (RClass-of Number) 0))
                                  (-not-filter (-val Number) 0 [(->ClassPE)])))]))))

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

(deftest array-subtype-test
  (is-clj (sub? (Array int) (Array int)))
  (is-clj (sub? (Array int) (ReadOnlyArray int)))
  (is-clj (sub? (Array Long) (clojure.lang.Seqable Long)))
  ;FIXME
  ;(is-clj (not (sub? (Array Object) (clojure.lang.Seqable Long))))
  (is-clj (not (sub? (ReadOnlyArray int) (Array int)))))

(deftest flow-assert-test
  (is-clj (subtype?
            (-> (tc-t (fn [a]
                        {:pre [(integer? a)]}
                        a))
                ret-t)
            (parse-type '[Any -> clojure.core.typed/AnyInteger])))
  (is-clj (subtype? 
            (-> (tc-t (let [a (read-string "1")
                            _ (assert (integer? a))]
                        (+ 10 a)))
                ret-t)
            (parse-type 'clojure.core.typed/AnyInteger)))
  ;postconditions
  (is-clj (subtype?
            (-> (tc-t (fn [a]
                        {:post [(vector? %)]}
                        a))
                ret-t)
            (parse-type '[Any -> (clojure.lang.IPersistentVector Any)]))))

(deftest complete-hmap-test
  (is-clj (subtype? (-complete-hmap {})
                    (parse-type '(clojure.lang.APersistentMap Nothing Nothing))))
  (is-clj (not
            (subtype? (-hmap {})
                      (parse-type '(clojure.lang.APersistentMap Nothing Nothing)))))
  (is-clj (subtype? (-> (tc-t {}) ret-t)
                    (parse-type '(clojure.lang.APersistentMap Nothing Nothing)))))

(deftest dotted-on-left-test
  (is-cf (memoize (fn []))))

(deftest string-as-seqable-test
  (is-clj (subtype? 
            (RClass-of String)
            (RClass-of Seqable [-any])))
  (is-clj (subtype? 
            (-val "a")
            (RClass-of Seqable [-any])))
  (is-cf (seq "a"))
  (is-cf (first "a") Character)
  (is-cf (first (clojure.core.typed/ann-form "a" String)) (clojure.core.typed/Option Character)))

(deftest recursive-cf-test
  (is (thrown? Exception
               (cf (clojure.core.typed/cf 1 Number)
                   Any))))

(deftest top-function-subtype-test
  (is-clj (subtype? (parse-type '[Any -> Any])
                (parse-type 'AnyFunction))))

(deftest intersection-simplify-test
  (is-cf (let [a (clojure.core.typed/ann-form [] (U (Extends [Number] :without [(clojure.lang.IPersistentVector Number)])
                                                    (clojure.lang.IPersistentVector Number)))]
           (when (vector? a)
             a))
         (U nil (clojure.lang.IPersistentVector Number))))

(deftest kw-args-test
  (is (check-ns 'clojure.core.typed.test.kw-args)))

(deftest get-APersistentMap-test
  (is-cf (get (clojure.core.typed/ann-form {} (clojure.lang.APersistentMap Number Number)) :a)
         (U nil Number)))

(deftest enum-field-non-nilable-test
  (is-cf (java.util.concurrent.TimeUnit/NANOSECONDS)
         java.util.concurrent.TimeUnit))

;;;; Checking deftype implementation of protocol methods

(defmacro caught-top-level-errors [nfn & body]
  `(with-ex-info-handlers
     [top-level-error? (fn [data# _#]
                         (~nfn (count (:errors data#))))]
     ~@body
     false))

(deftest new-instance-method-return-test
  (is (check-ns 'clojure.core.typed.test.protocol))
  (is (caught-top-level-errors #{2}
        (check-ns 'clojure.core.typed.test.protocol-fail))))
;;;;

;FIXME CTYP-71
;(deftest let-filter-unscoping-test
;  (is-cf (fn [a]
;            (and (< 1 2) a))
;         [(U nil Number) -> Any :filters {:then (is Number 0)}]))

(deftest map-literal-containing-funapp-test
  (is-cf {:bar (identity 1)}))

(deftest doseq>-test
  (is-cf (clojure.core.typed/doseq> [a :- (U clojure.core.typed/AnyInteger nil), [1 nil 2 3]
                   :when a]
            (inc a)))
  ;wrap in thunk to prevent evaluation
  (is-clj (u/top-level-error-thrown?
               (cf (fn []
                     (clojure.core.typed/doseq> [a :- (U clojure.core.typed/AnyInteger nil), [1 nil 2 3]]
                                                (inc a)))))))

(deftest for>-test
  (is-cf
    (clojure.core.typed/for> :- Number
                             [a :- (U nil Number), [1 nil 2 3]
                              b :- Number, [1 2 3]
                              :when a]
                             (+ a b))
    (clojure.core.typed/Seq Number))
  (is-clj (u/top-level-error-thrown?
        (cf
          (clojure.core.typed/for> :- Number
                                   [a :- (U clojure.lang.Symbol nil Number), [1 nil 2 3]
                                    b :- Number, [1 2 3]]
                                   (+ a b))))))

(deftest dotimes>-test
  (is-cf (clojure.core.typed/dotimes> [i 100] (inc i)) nil))

(deftest records-test
  (is (check-ns 'clojure.core.typed.test.records))
  (is (check-ns 'clojure.core.typed.test.records2)))

(deftest string-methods-test
  (is-cf (.toUpperCase "a") String))

(deftest common-destructuring-test
  (is (check-ns 'clojure.core.typed.test.destructure)))

(deftest loop-errors-test
  (is (caught-top-level-errors #{1}
        (cf (loop [a 1] a))))
  (is (caught-top-level-errors #{2}
        (cf (clojure.core.typed/loop> [a :- String, 1] a)))))

(deftest map-indexed-test
  (is-cf (map-indexed (clojure.core.typed/inst vector clojure.core.typed/AnyInteger Long Any Any Any Any) 
                      [1 2])
         (clojure.lang.Seqable '[clojure.core.typed/AnyInteger Long])))

(deftest letfn>-test
  (is-cf (clojure.core.typed/letfn> [a :- [Number -> Number]
                                     (a [b] b)]
           (a 1))
         Number)
  ;interdependent functions
  (is-cf (clojure.core.typed/letfn> [a :- [Number -> Number]
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
    (= (-or (-filter (RClass-of clojure.lang.Symbol) 'id [(->KeyPE :a)])
            (-filter (RClass-of String) 'id [(->KeyPE :a)]))
       (-filter (Un (RClass-of clojure.lang.Symbol)
                    (RClass-of String))
                'id [(->KeyPE :a)])))
  
  ;(& (is T a pth)
  ;   (when (is T a pth)
  ;     (is T' 'b)))
  ;  simplifies to 
  ;(& (is T a pth)
  ;   (is T' 'b))
  ;  FIXME
;  (is-clj 
;    (= (-and (-filter (RClass-of clojure.lang.Symbol) 'id [(->KeyPE :a)])
;             (-imp (-filter (RClass-of clojure.lang.Symbol) 'id [(->KeyPE :a)])
;               (-filter (RClass-of String) 'id2 [(->KeyPE :a) (->KeyPE :b)])))
;       (-and (-filter (RClass-of clojure.lang.Symbol) 'id [(->KeyPE :a)])
;             (-filter (RClass-of String) 'id2 [(->KeyPE :a) (->KeyPE :b)]))))
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
    (both-subtype? (clj (update (Un -nil (-hmap {(-val :foo) (RClass-of Number)}))
                                (-filter (Un -false -nil) 'id [(->KeyPE :foo)])))
                   -nil))
  (is-clj 
    (both-subtype? (update (Un -nil (-hmap {(-val :foo) (RClass-of Number)}))
                           (-not-filter (Un -false -nil) 'id [(->KeyPE :foo)]))
                   (-hmap {(-val :foo) (RClass-of Number)})))
  ; if (:foo a) is nil, either a has a :foo entry with nil, or no :foo entry
  (is-clj (both-subtype? (update (-hmap {})
                                 (-filter -nil 'id [(->KeyPE :foo)]))
                         (make-HMap {} {(-val :foo) -nil}))))

(deftest multimethod-test
  (is (check-ns 'clojure.core.typed.test.mm)))

(defmacro throws-tc-error? [& body]
  `(with-ex-info-handlers
     [u/tc-error? (constantly true)]
     ~@body
     false))

(deftest instance-field-test
  (is-cf (.ns ^clojure.lang.Var #'clojure.core/map))
  (is (caught-top-level-errors #{2}
        (cf (fn [] (.ns ^clojure.lang.Var 'a))))))

(deftest HMap-syntax-test
  (is (= (parse-type '(HMap :absent-keys #{:op}))
         (-hmap {} #{(-val :op)} true))))

(deftest map-filter-test
  (is-cf (clojure.core.typed/ann-form (fn [a] (:op a))
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
  (is-cf #(= :if (:op %))
         [(U '{:op ':if} '{:op ':case})
          -> Boolean
          :filters {:then (& (is '{:op (Value :if)} 0)
                             (is ':if 0 [(Key :op)]))
                    :else (! ':if 0 [(Key :op)])}])
  (is-cf (clojure.core.typed/fn> [a :- (U '{:op ':if} '{:op ':case})
                                  b :- (U '{:op ':if} '{:op ':case})]
                                 (if (= :if (:op a))
                                   (= :case (:op b))
                                   false)))
  (is-cf (fn [a b] 
           (let [and__3941__auto__ (clojure.core/symbol? a)] 
             (if (clojure.core.typed/print-filterset "test" and__3941__auto__)
               (clojure.core/number? b) 
               and__3941__auto__)))))

(deftest warn-on-unannotated-vars-test
  (is (check-ns 'clojure.core.typed.test.warn-on-unannotated-var)))

(deftest number-ops-test
  (is-cf (min (Integer. 3) 10) Number))

(deftest ctor-infer-test
  (is-cf (java.io.File. "a"))
  (is-cf (let [a (or "a" "b")]
           (java.io.File. a)))
  (is (throws-tc-error?
        (cf (fn [& {:keys [path] :or {path "foo"}}]
            (clojure.core.typed/print-env "a")
            (java.io.File. path))
          [& :optional {:path String} -> java.io.File]))))

;(fn> [a :- (U (Extends Number :without [(IPerVec Any)])
;              (Extends (IPV Any) :without [Number])
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
;         :without [(IPersistentMap Any Any)])

(deftest extends-test
  ; without extends: never returns a (IPV Number) because we can have
  ; a type (I (IPM Any Any) (IPV Any))
  (is (u/top-level-error-thrown?
        (cf (fn [a]
              (if (vector? a)
                a
                nil))
            [(U (clojure.lang.IPersistentVector Number)
                (clojure.lang.IPersistentMap Any Any))
             -> (U nil (clojure.lang.IPersistentVector Number))])))
  ; can use assertions to prove non-overlapping interfaces
  (is-cf (fn [a]
           {:pre [(or (and (vector? a)
                           (not (map? a)))
                      (and (map? a)
                           (not (vector? a))))]}
           (if (vector? a)
             a
             nil))
         [(U (clojure.lang.IPersistentVector Number)
             (clojure.lang.IPersistentMap Any Any))
          -> (U nil (clojure.lang.IPersistentVector Number))])
  ; or use static types
  (is-cf (fn [a]
           (if (vector? a)
             a
             nil))
         [(U (Extends [(clojure.lang.IPersistentVector Number)]
                      :without [(clojure.lang.IPersistentMap Any Any)])
             (Extends [(clojure.lang.IPersistentMap Any Any)]
                      :without [(clojure.lang.IPersistentVector Any)]))
          -> (U nil (clojure.lang.IPersistentVector Number))])
  ; technically it's ok to implement Number and IPM
  (is-cf (fn [a]
           {:pre [(number? a)]}
           (clojure.core.typed/print-env "a")
           (+ 1 a))
         [(clojure.lang.IPersistentMap Any Any) -> Number]))

(deftest complete-hash-subtype-test
  (is-clj (sub? (HMap :optional {} :complete? true)
            (clojure.lang.IPersistentMap Integer Long))))

(deftest set!-test
  (is (check-ns 'clojure.core.typed.test.set-bang)))

(deftest flow-unreachable-test
  ; this will always throw a runtime exception, which is ok.
  (is-cf (fn [a] 
           {:pre [(symbol? a)]}
           (clojure.core.typed/print-env "a") 
           (clojure.core.typed/ann-form a clojure.lang.Symbol))
         [Long -> clojure.lang.Symbol]))

; FIXME this is wrong, should not just be nil
#_(deftest array-first-test
    (is-cf (let [a (clojure.core.typed/into-array> Long [1 2])]
             (first a))))

(deftest every?-update-test
  (is-cf (let [a (clojure.core.typed/ann-form [] (U nil (clojure.core.typed/Coll Any)))]
           (assert (every? number? a))
           a)
         (U nil (clojure.core.typed/Coll Number))))

(deftest keys-vals-update-test
  (is-clj (both-subtype? 
            (update (RClass-of IPersistentMap [-any -any])
                    (-filter (RClass-of Seqable [(RClass-of Number)])
                             'a [(->KeysPE)]))
            (RClass-of IPersistentMap [(RClass-of Number) -any])))
  ; test with = instead of subtype to catch erroneous downcast to (IPersistentMap Nothing Any)
  (is-clj (both-subtype?
            (-> 
              (tc-t (let [m (clojure.core.typed/ann-form {} (clojure.lang.IPersistentMap Any Any))]
                      (assert (every? number? (keys m)))
                      m))
              ret-t)
            (parse-type '(clojure.lang.IPersistentMap Number Any))))
  (is-clj (both-subtype? 
            (-> (tc-t (let [m (clojure.core.typed/ann-form {} (clojure.lang.IPersistentMap Any Any))]
                        (assert (every? number? (keys m)))
                        (assert (every? number? (vals m)))
                        m))
                ret-t)
            (parse-type '(clojure.lang.IPersistentMap Number Number))))
  (is-cf (fn [m]
            {:pre [(every? number? (vals m))]}
            m)
          [(clojure.lang.IPersistentMap Any Any) -> (clojure.lang.IPersistentMap Any Number)])
  (is-cf (fn [m]
            {:pre [(every? symbol? (keys m))
                   (every? number? (vals m))]}
            m)
          [(clojure.lang.IPersistentMap Any Any) -> (clojure.lang.IPersistentMap clojure.lang.Symbol Number)]))

; a sanity test for intersection cache collisions
(deftest intersect-cache-test
  (is-clj (=
       (Poly-body*
         ['foo1 'foo2]
         (Poly* '[x y] 
                [no-bounds no-bounds]
                (In (make-F 'x) (make-F 'y))))
       (In (make-F 'foo1) (make-F 'foo2)))))

(deftest latent-filter-subtype-test 
  (is-clj (not (subtype? (parse-type '(Fn [Any -> Any :filters {:then (is Number 0)}]))
                         (parse-type '(Fn [Any -> Any :filters {:then (is Nothing 0)}]))))))

;CTYP-27
;(deftest nth-inline-test
;  (is (cf (fn [s] (clojure.lang.RT/nth s 0 nil))
;          (All [x] (Fn #_[nil -> nil]
;                       #_[(I (clojure.lang.Seqable x) (ExactCount 0)) -> nil]
;                       [(I (clojure.lang.Seqable x) (CountRange 1)) -> x]
;                       #_[(U nil  (clojure.lang.Seqable x)) -> (U nil  x)])))))

;TODO
;(deftest filter-seq-test

(deftest subtype-tfn-test
  (is-clj (sub? (TFn [[x :variance :covariant]] Number)
            (TFn [[x :variance :covariant]] Any)))
  (is-clj (not (sub? (TFn [[x :variance :covariant]] Any)
                 (TFn [[x :variance :covariant]] Number))))
  (is-clj (sub? (clojure.lang.IPersistentMap Any Any)
            ((TFn [[x :variance :covariant]] (clojure.lang.IPersistentMap Any Any)) Any))))

(deftest invoke-tfn-test
  (is-clj (inst/manual-inst (parse-type '(All [[x :< (TFn [[x :variance :covariant]] Any)]]
                                              (x Any)))
                            [(parse-type '(TFn [[x :variance :covariant]] Number))]))
  (is-clj (inst/manual-inst (parse-type '(All [[x :< (TFn [[x :variance :covariant]] Number)]]
                                              (x Any)))
                            [(parse-type '(TFn [[x :variance :covariant]] Number))]))
  (is-clj (inst/manual-inst (parse-type '(All [x
                                               [y :< x]] 
                                              Any))
                            [(parse-type 'Any)
                             (parse-type 'Any)])))

#_(fully-resolve-type (parse-type '((All [a] (TFn [[x :variance :covariant :< a]] a)) Number)))


(deftest filter-seq-test
  ;  TODO possible extension for filter
;  (is (cf (filter :a (clojure.core.typed/ann-form [] (clojure.lang.Seqable '{:b Number})))
;          (clojure.lang.Seqable '{:b Number :a Any})))
  (is-cf (filter (clojure.core.typed/inst identity (U nil Number)) [1 nil])
         (clojure.lang.Seqable Number))
  (is-cf (let [filter (clojure.core.typed/ann-form filter
                        (All [x y]
                             [[x -> Any :filters {:then (! y 0)}] 
                              (U nil (clojure.lang.Seqable x)) -> (clojure.core.typed/Seq (I x (Not y)))]))]
           (filter (clojure.core.typed/inst identity (U nil Number)) [1 nil]))
         (clojure.lang.Seqable Number))
  (is-cf (let [filter (clojure.core.typed/ann-form filter
                        (All [x y]
                             [[x -> Any :filters {:then (is y 0)}] 
                              (U nil (clojure.lang.Seqable x)) -> (clojure.core.typed/Seq y)]))]
           (filter number? [1 nil]))
         (clojure.lang.Seqable Number)))

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
;              (clojure.lang.IMapEntry Any Any)
;              (TFn [[x :variance :covariant
;                     :< (U nil (clojure.lang.IMapEntry Any Any))]]
;                   x)
;              (All [a1 b1]
;                   (TFn [[x :variance :covariant
;                          :< (U nil (clojure.lang.IMapEntry a1 b1))]]
;                        (clojure.lang.APersistentMap a1 b1))))
;            (clojure.core.typed/Coll Any)))
;  (is-clj (sub? (TFn [[x :variance :covariant]]
;                   x)
;            (TFn [[x :variance :covariant]]
;                 Any)))
;  (is-clj (sub? (TFn [[x :variance :covariant
;                     :< (U nil (clojure.lang.IMapEntry Any Any))]]
;                   x)
;            (TFn [[x :variance :covariant]]
;                 Any)))
;  (is-clj (sub? (All [a1 b1]
;                 (TFn [[x :variance :covariant
;                        :< (U nil (clojure.lang.IMapEntry a1 b1))]]
;                      (clojure.lang.APersistentMap a1 b1)))
;            (Rec [c]
;                 (TFn [[x :variance :covariant]] 
;                      (clojure.lang.IPersistentCollection 
;                        Any 
;                        (TFn [[x :variance :covariant]] Any) 
;                        c))))))

(deftest not-type-subtype
  (is-clj (not (sub? (Not nil) nil)))
  (is-clj (sub? (Not nil) (Not nil)))
  (is-clj (not (sub? (Not nil) (Not false))))
  (is-clj (sub? (Not (U false nil)) (Not false))))

(deftest negative-filter-test
  (is-cf (let [a (clojure.core.typed/ann-form (fn [a b] (assert false))
                                              (All [x y]
                                                   [[x -> Any :filters {:then (! y 0)}]
                                                    (U nil (clojure.lang.Seqable x)) -> (clojure.core.typed/Seq (I x (Not y)))]))]
           ;need to instantiate negative types for now
           (fn [] (a (clojure.core.typed/inst identity (U nil Number)) [1 nil])))
         [-> (clojure.lang.Seqable Number)]))



(deftest Not-combine-test
  (is-clj (= (In -nil (NotType-maker -nil))
         (Un)))
  (is-clj (clj
        (= (In (RClass-of Integer) (NotType-maker (RClass-of Number)))
           (Un))))

  (is-clj (not (overlap (RClass-of Integer) (NotType-maker (RClass-of Number)))))
  (is-clj (clj (overlap (RClass-of Number) (NotType-maker -nil))))

  (is-clj (overlap (NotType-maker (RClass-of Number))
               (NotType-maker (RClass-of Integer))))

  (is-clj (not (overlap (NotType-maker (RClass-of Number)) 
                    (RClass-of Integer))))

  (is-clj (= (In (RClass-of Number) (NotType-maker -nil))
         (RClass-of Number)))

  (is-clj (= (In (Un (RClass-of Number) -nil) -nil)
         -nil))
  (is-clj (= (In (Un (RClass-of Number) -nil) (NotType-maker -nil))
         (RClass-of Number)))
  (is-clj (= (In (RClass-of Number) (NotType-maker -nil))
         (RClass-of Number)))

  (is-clj (clj (= (In (Un -nil (RClass-of Number)) (NotType-maker (Un -false -nil)))
              (RClass-of Number))))

  (is-clj (overlap (Un -nil (RClass-of Number)) (NotType-maker (Un -false -nil))))

  (is-clj (= (In (Un -nil (RClass-of Number)) (NotType-maker -nil))
         (RClass-of Number)))
  (is-clj (= (Un (In -nil (NotType-maker -nil))
             (In (RClass-of Number) (NotType-maker -nil)))
         (RClass-of Number)))

  (is-clj (= (In (NotType-maker (RClass-of Number))
             (NotType-maker (RClass-of Integer)))
         (NotType-maker (RClass-of Number))))

  (is-clj (subtype? (RClass-of Number) (NotType-maker -nil)))
  (is-clj (subtype? (RClass-of Number) (NotType-maker (RClass-of Integer))))
  (is-clj (not (subtype? (RClass-of Integer) (NotType-maker (RClass-of Number)))))
  (is-clj (not (subtype? (NotType-maker -nil) (RClass-of Number))))

  (is-clj (= (let [i (subst-all {'x (->t-subst (Un (RClass-of Number) -nil) no-bounds) 
                                 'y (->t-subst -nil no-bounds)} 
                                (In (make-F 'x) (NotType-maker (make-F 'y))))
                   _ (assert (Intersection? i))]
               (apply In (:types i)))
             (RClass-of Number)))

  (is-clj (overlap (make-F 'x)
               (NotType-maker (make-F 'y))))
  (is-clj (overlap (B-maker 0)
               (NotType-maker (B-maker 1))))
  (is-clj (not (subtype? (B-maker 0)
                     (NotType-maker (B-maker 1)))))
  (is-clj (not= (In (make-F 'x)
                (NotType-maker (make-F 'y)))
            (Un))))

(deftest intersection-csgen-test
  (is-clj (clj (cs-gen #{} {'a no-bounds} {}
                   (In (RClass-of Seqable [(RClass-of Number)])
                       (make-CountRange 1))
                   (In (RClass-of Seqable [(make-F 'a)])
                       (make-CountRange 1))))))

(deftest iterable-as-seqable-test
  (is-cf (first (clojure.core.typed/ann-form [] Iterable))))

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
;            (clojure.core.typed.async/Chan Any)))
;
;  (is-clj (sub? (clojure.core.async.impl.protocols/ReadPort clojure.core.typed.test.async/Query)
;            (clojure.core.async.impl.protocols/ReadPort Any)))
;
;  (is-clj (sub? (clojure.core.async.impl.protocols/Channel clojure.core.typed.test.async/Query)
;            (clojure.core.async.impl.protocols/Channel Any)))
;
;  (is-clj (sub? (Extends [(clojure.core.async.impl.protocols/Channel clojure.core.typed.test.async/Query)
;                      (clojure.core.async.impl.protocols/WritePort clojure.core.typed.test.async/Query)
;                      (clojure.core.async.impl.protocols/ReadPort clojure.core.typed.test.async/Query)])
;            (Extends [(clojure.core.async.impl.protocols/Channel Any)
;                      (clojure.core.async.impl.protocols/WritePort Any)
;                      (clojure.core.async.impl.protocols/ReadPort Any)])))
;  (is-clj (sub? (Extends [(clojure.core.async.impl.protocols/Channel clojure.core.typed.test.async/Query)
;                      (clojure.core.async.impl.protocols/WritePort clojure.core.typed.test.async/Query)
;                      (clojure.core.async.impl.protocols/ReadPort clojure.core.typed.test.async/Query)])
;            (Extends [(clojure.core.async.impl.protocols/ReadPort Any)]))))
;  )
;
;(when-async-dep
;(deftest Extends-cs-gen-test
;  (is-clj (check-ns 'clojure.core.typed.test.async))
;  (is-clj (cs-gen #{} {'x no-bounds} {} 
;          (parse-type '(clojure.lang.LazySeq Any))
;          (with-bounded-frees {(make-F 'x) no-bounds}
;            (parse-type '(clojure.lang.LazySeq x)))))
;
;  (is-clj (cs-gen #{} {'x no-bounds} {} 
;              (parse-type '(clojure.core.async.impl.protocols/ReadPort Any))
;              (with-bounded-frees {(make-F 'x) no-bounds}
;                (parse-type '(clojure.core.async.impl.protocols/ReadPort x)))))
;  (is-clj (cf ((clojure.core.typed/inst identity  (Extends [(clojure.core.async.impl.protocols/ReadPort Any)]))
;           (clojure.core.typed/ann-form (clojure.core.typed.async/chan> Any) 
;                              (Extends [(clojure.core.async.impl.protocols/ReadPort Any)])))
;          (Extends [(clojure.core.async.impl.protocols/ReadPort Any)])))
;  (is-clj (cf (identity
;           (clojure.core.typed/ann-form (clojure.core.typed.async/chan> Any) 
;                                        (Extends [(clojure.core.async.impl.protocols/ReadPort Any)])))
;          (Extends [(clojure.core.async.impl.protocols/ReadPort Any)])))
;  (is-clj (cf (identity
;           (clojure.core.typed/ann-form (clojure.core.typed.async/chan> Any) 
;                     (clojure.core.async.impl.protocols/ReadPort Any)))
;          (clojure.core.async.impl.protocols/ReadPort Any)))
;  (is-clj (cf (seq (clojure.core.typed/ann-form [] (clojure.core.typed/Coll (clojure.core.typed.async/Chan Any))))
;          (U nil (clojure.core.typed/Coll (clojure.core.typed.async/Chan Any)))))
;  (is-clj (cf (seq (clojure.core.typed/ann-form [] (clojure.core.typed/Coll (clojure.core.typed/Seqable Any))))
;          (U nil (clojure.core.typed/Coll (clojure.core.typed/Seqable Any)))))
;  (is-clj (subtype? (parse-type '(clojure.core.typed.async/Chan Any))
;                -any))
;
;  (is-clj (overlap (parse-type '(clojure.core.typed.async/Chan Any))
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
;  (is-clj (cf (clojure.core.typed/letfn> [af :- (All [a]
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

;(cf (seq (ann-form (seq []) (Seqable Any))))
;(cf (seq (ann-form (seq []) (ISeq Any))))

(deftest mm-warn-on-unannotated-vars-test
  (is (check-ns 'clojure.core.typed.test.mm-warn-on-unannotated)))

(deftest HMap-parse-fail-test
  (is (u/tc-error-thrown? (clj (parse-type '(HMap :mandatory {:a Any} :absent-keys #{:a}))))))

(deftest HMap-absent-complete-test
  (is-clj (not (sub? (HMap :mandatory {:a Any}) (HMap :absent-keys #{:a}))))
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
  (is-cf (map (clojure.core.typed/inst vector Number Number Any Any Any Any) [1] [2])
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
  (is-cf (clojure.core.typed/typed-deps clojure.core.async)))

(deftest def-expected-test
  (is-cf (do
           (clojure.core.typed/ann foo1 Any)
           (clojure.core.typed/ann-form (def foo1 1) Any)))
  (is-cf (do
           (clojure.core.typed/ann foo2 Any)
           (clojure.core.typed/ann-form (def foo2) Any))))

(deftest CTYP-42
  (is (check-ns 'clojure.core.typed.test.succeed.CTYP-42-record-extend-protocol)))

(deftest atom>-test
  (is-cf (clojure.core.typed/atom> (clojure.core.typed/Vec Any) [])
         (clojure.core.typed/Atom1 (clojure.core.typed/Vec Any))))

(deftest CTYP-48
  (is-cf (fn [a] (:a a))
         [Nothing -> Any]))

(deftest CTYP-49 
  (is (check-ns 'clojure.core.typed.test.succeed.CTYP49-unreachable)))

#_(deftest CTYP-47-Fn-as-IFn
  (is-cf (fn [] #())
          [-> clojure.lang.IFn]))

(deftest plain-defprotocol-test
  (is (u/top-level-error-thrown? (cf (defprotocol Foo (bar [this])))))
  (is (u/top-level-error-thrown? 
        (check-ns 'clojure.core.typed.test.fail.CTYP-45))))

(deftest HMap-absent-key-update-test
  ;ensure absent keys are preserved when passed through occurrence typing's `update`
  (is-cf
    (let [a (clojure.core.typed/ann-form {:a 1} (HMap :mandatory {:a Number}
                                                      :optional {:b Number,
                                                                 :c Number}))]
      (when (:b a)
        (clojure.core.typed/ann-form a (HMap :mandatory {:a Number}
                                             :optional {:b Number,
                                                        :c Number}))))))

(deftest non-empty-map-test
  (is-cf (map inc [1 2 3])
         (clojure.core.typed/NonEmptySeq Number)))

;CTYP-53
(deftest hmap-cast-test
  (is (both-subtype?
        (ety
          (clojure.core.typed/fn> 
            [m :- (HMap)]
            (assert (:foo m))
            m))
        (parse-clj '['{} -> 
                     '{}
                     :filters {:then (! (U nil false) 0)
                               :else (is (U nil false) 0)}
                     :object {:id 0}])))
  (is (both-subtype? 
        (ety
          (clojure.core.typed/fn> 
            :- (HMap :mandatory {:foo (clojure.core.typed/Vec Any)})
            [m :- (HMap)]
            (assert (vector? (:foo m)))
            m))
        (parse-clj '[(HMap) -> 
                     (HMap :mandatory {:foo (clojure.core.typed/Vec Any)})
                     :filters {:then (! (U nil false) 0)
                               :else (is (U nil false) 0)}
                     :object {:id 0}])))
  (is (both-subtype? 
        (ety 
          (clojure.core.typed/fn> 
            [m :- (HMap :mandatory {:bar Any})]
            (assert (nil? (:foo m)))
            m))
        (parse-clj '[(HMap :mandatory {:bar Any}) -> 
                     (U (HMap :mandatory {:bar Any, :foo nil})
                        (HMap :mandatory {:bar Any}
                              :absent-keys #{:foo}))
                     :filters {:then (! (U nil false) 0)
                               :else (is (U nil false) 0)}
                     :object {:id 0}])))
  (is
    (both-subtype?
      (ety
        (clojure.core.typed/fn> 
          [m :- '{}]
          (assert (not (vector? (:foo m))))
          m))
      (parse-clj '[(HMap) -> 
                   ; not sure if this should simplify to (HMap)
                   (U (HMap :mandatory {:foo Any})
                      (HMap :absent-keys #{:foo}))
                   :filters {:then (! (U nil false) 0)
                             :else (is (U nil false) 0)}
                   :object {:id 0}])))
  (is 
    (clj
      (let [t1 (clj (update (parse-type '(HMap))
                            (parse-filter '(is (clojure.core.typed/Vec Any) m [(Key :foo)]))))
            t2 (clj (parse-type '(HMap :mandatory {:foo (clojure.core.typed/Vec Any)})))]
        (both-subtype? t1 t2)))))

;CTYP-60
(deftest absent-keys-test
  (is (not (sub? (HMap :mandatory {:a String}
                       :complete? true)
                 (HMap :absent-keys #{:a}))))
  (is 
    (u/top-level-error-thrown?
      (cf {:a "a"} (HMap :absent-keys #{:a})))))

;CTYP-61
(deftest hmap-assoc-test
  (is-cf (assoc (clojure.core.typed/ann-form {} (HMap :optional {:a Any})) :b "v")
         (HMap :mandatory {:b (Value "v")} :optional {:a Any}))
  (is-cf (assoc (clojure.core.typed/ann-form {} (HMap :optional {:a Any})) :a "v")
         (HMap :mandatory {:a (Value "v")})))

(deftest CTYP-37-defprotocol-better-error
  (is (u/top-level-error-thrown?
        (check-ns 'clojure.core.typed.test.fail.CTYP-37))))

(defmacro equal-types-noparse [l r]
  `(clj (is (let [l# (ety ~l)
                  r# ~r]
              (or (both-subtype? l# r#)
                  (do (println "Actual" l#)
                      (println "Expected" r#)
                      (println "In" (quote ~l))
                      nil))))))

(defmacro equal-types [l r]
  `(equal-types-noparse ~l (parse-type (quote ~r))))

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
  
  (equal-types (assoc (clojure.core.typed/ann-form {} (HMap :optional {:a Any})) :b "v")
               (HMap :mandatory {:b (Value "v")} :optional {:a Any}))
  
  (equal-types (assoc (clojure.core.typed/ann-form {} (HMap :optional {:a Any})) :a "v")
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
  
  (equal-types-noparse (assoc [0] 0 (if (clojure.core.typed/ann-form 1 Any) 1 2))
                       (-hvec [(Un (-val 1) (-val 2))]
                              :filters [(-true-filter)]
                              :objects [-empty]))
  
  ; Basic types
  (equal-types (assoc {} 'a 5)
               (clojure.lang.IPersistentMap 'a '5))
  
  (equal-types (assoc {:b 6} 'a 5)
               (clojure.lang.IPersistentMap (U 'a ':b) (U '5 '6)))
  
  (equal-types (assoc (clojure.core.typed/ann-form nil
                                                   (U nil (clojure.lang.IPersistentMap Any Any)))
                 :a 5)
               (clojure.lang.IPersistentMap Any Any))
  
  (equal-types (assoc (clojure.core.typed/ann-form {} (clojure.lang.IPersistentMap Any Any)) :a 5)
               (clojure.lang.IPersistentMap Any Any))
  
  (equal-types (assoc (clojure.core.typed/ann-form [] (clojure.lang.IPersistentVector Any)) 0 2)
               (clojure.lang.IPersistentVector Any))
  
  ;; TODO: assocs on records
  
  )

(deftest CTYP-62-equiv-test
  (is (tc-equiv := 
                (ret (-val "a"))
                (ret -any))
      (ret (Un -false -true)))
  (is (= (eret (= "a" (clojure.core.typed/ann-form 1 Any)))
         (ret (Un -false -true))))
  (is (= (eret (= "a" (clojure.core.typed/ann-form 1 Any)))
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
  
  ; this merge doesn't actually give us any information about :b
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
               '{:a Any :b Any})
  
  (equal-types (merge (clojure.core.typed/ann-form {:a 6} '{:a Number})
                      (clojure.core.typed/ann-form {:b "s"} '{:b String}))
               '{:a Any :b String})
  
  ; incomplete covering mandatory
  (equal-types (merge {:a 5}
                      (clojure.core.typed/ann-form {:a 10} '{:a '10}))
               '{:a '10})
  
  ; incomplete covering optional
  (equal-types (merge {:a 5}
                      (clojure.core.typed/ann-form {} (HMap :optional {:a (Value 10)})))
               (U '{:a (Value 5)}
                  '{:a (Value 10)}))
  
  
  ; both incomplete optionals
  (equal-types (merge (clojure.core.typed/ann-form {} (HMap :optional {:a '5}))
                      (clojure.core.typed/ann-form {} (HMap :optional {:a '10})))
               (U '{:a '5}
                  '{:a '10}
                  (HMap :mandatory {} :absent-keys #{:a} :complete? false)))
  
  ; (Option HMap) first argument incomplete
  (equal-types (merge (clojure.core.typed/ann-form {:a 5} (U nil '{:a '5}))
                      (clojure.core.typed/ann-form {:b 8} (HMap :mandatory {:b Number} :complete? true)))
               (U (HMap :mandatory {:b Number} :complete? true)
                  '{:a '5 :b Number}))
  
  ;; nil (HMap :absent-keys #{:a}) -> (HMap :absent-keys #{:a})
  ;; '{:a 5} '{} -> '{:a 5}
  ;; nil '{:a Number} -> '{:a Number}
  ;; '{:a 5} '{:a Number} -> '{:a Number}
  ; All together: (U '{:a Number} (HMap :absent-keys #{:a})) or (HMap :optional {:a Number})
  (equal-types (merge (clojure.core.typed/ann-form {:a 5} (U nil '{:a '5}))
                      (clojure.core.typed/ann-form {} (HMap :optional {:a Number} :complete? false)))
               (HMap :optional {:a Number}))
  
  ; Basic maps
  (equal-types (merge (clojure.core.typed/ann-form {} (clojure.lang.IPersistentMap Any Any)) {:a 5})
               (clojure.lang.IPersistentMap Any Any))
  
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
  (is (u/top-level-error-thrown?
        (cf (defrecord Unannotated [])))))

(deftest datatype-method-recur-test
  (is (check-ns 'clojure.core.typed.test.datatype-recur)))

(deftest record-annotated-as-datatype-test
  ; record annotated as datatype
  (is (u/top-level-error-thrown?
        (cf (do (clojure.core.typed/ann-datatype IncorrectRec [])
                (defrecord IncorrectRec [])))))
  ; datatype annotated as record
  (is (u/top-level-error-thrown?
        (cf (do (clojure.core.typed/ann-record IncorrectDt [])
                (deftype IncorrectDt []))))))

(deftest recursive-ann-test
  (is (check-ns 'clojure.core.typed.test.recursive)))

(deftest comparable-inline-test
  (is-cf (fn [v x] (compare v x)) (Fn [Comparable Any -> Number])))

(deftest CTYP-71-simplify-test
                                              ; must be resolvable to trigger the bug
  (is-cf (clojure.core.typed/fn> [a :- (U nil (clojure.core.typed/Nilable java.util.Date))] 
                                 (when a (clojure.core.typed/ann-form a java.util.Date)))))

;(clj (compact [(-filter (parse-type 'Number) 0)
;               (-not-filter (Un -false -nil) 0)]
;              false))

(deftest CTYP-84-hlist-ancestor-test
  (is-cf (seq '(1)) (clojure.core.typed/NonEmptySeq Number)))

(deftest CTYP-78-finally-expected-test
  (is (check-ns 'clojure.core.typed.test.finally)))

(deftest CTYP-77-invoke-nonliteral-kw-test
  (is (check-ns 'clojure.core.typed.test.non-literal-val-fn)))

(deftest CTYP-74-malformed-TApp-test
  (is (u/tc-error-thrown? (parse-type '([Any -> Any])))))

(deftest CTYP-73-reduced-test
  (is-cf (reduced 1) (clojure.lang.Reduced Number))
  (is-cf @(reduced 1) Number)
  (is-cf (reduce (clojure.core.typed/ann-form
                   (fn [a b] (if (= a b) 1 (reduced 1)))
                   [Number Number -> (U (clojure.lang.Reduced Number) Number)])
                 1 [1 2 3])
         Number))

(deftest Assoc-test
  (is-cf {:a 1} (Assoc '{} ':a Number))
  (is-cf {:a 1} (Assoc (U '{:a Number} '{:a Double}) ':a Long))
  (is-cf (fn [a] (assoc a 1 2))
         (All [[x :> (clojure.core.typed/Map Nothing Nothing) :< (clojure.core.typed/Map Number Number)]]
              [x -> (clojure.core.typed/Map Number Number)]))
  (is-cf (fn [a] (assoc a :a 1)) 
         (All [[x :> (clojure.core.typed/Map Nothing Nothing) :< (clojure.core.typed/Map Any Any)]] 
              [x -> (Assoc x ':a Number)]))
  (is-cf (let [f (clojure.core.typed/ann-form 
                   (fn [a] (assoc a :a 1)) 
                   (All [[x :< (clojure.core.typed/Map Any Any)]] 
                        [x -> (Assoc x ':a Number)]))]
           (clojure.core.typed/ann-form 
             (f {:b 1})
             '{:b Number :a Number})))
  (is-cf (fn [a] (assoc a :a 1)) 
         (All [[x :< (clojure.core.typed/Map Any Any)]] [x -> (Assoc x ':a Number)]))

  (is-cf (let [add-a (-> #(assoc % :a 1)
                         (clojure.core.typed/ann-form
                           (All [[x :< (clojure.core.typed/Map Any Any)]]
                                [x -> (Assoc x ':a Number)])))]
           (clojure.core.typed/ann-form
             (add-a {})
             '{:a Number})
           (clojure.core.typed/ann-form
             (-> (add-a {}) :a)
             Number)))
  (is-cf {:a 1 :b 2}
         (U (Assoc '{} ':a Number)
            (Assoc '{} ':b Number)
            (Assoc '{} ':a Number ':b Number))))

;(deftest Get-test
;  (is-cf 1 (Get '{:a Number} ':a)))

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
  (is-cf (first [1 'a]) Number)
  (is-cf (-> {:a 1} first second) Number))

(deftest variable-hvec-test
  (is (sub? '[Number Number Number] '[Number Number *]))
  (is (sub? '[Number Number Number] '[Number Number *]))
  (is (sub? '[Number Number Integer *] '[Number Number *]))
  (is (not (sub? '[Number Number clojure.lang.Symbol *] '[Number Number *])))
  (is-cf [1 2 3] '[Number Number *])
  (is-cf [1 2 3] '[Number Number Number Number *])
  (is-cf (first [1 'a 2]) Number)
  (is-cf (second [1 2 3]) Number))

(deftest CTYP-85-abo-test
  (is-cf (fn [] (fn [b] b))
         [-> [Any -> Any]]))

(deftest array-reflection-test
  (is-cf (fn make-process [script]
           {:post [%]}
           (let [^Runtime r (Runtime/getRuntime)
                 _ (assert r)
                 ^"[Ljava.lang.String;" arr (clojure.core.typed/into-array> String ["echo 'hello'"])]
             (.exec r arr)))
         [String -> java.lang.Process]))

(deftest expected-IPersistentMap-test
  (is-cf {:a #(+ %1 %2)}
         (clojure.core.typed/Map Any [Number Number -> Number])))

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
  (is-cf (clojure.core.typed/defn> add-two :- clojure.core.typed/AnyInteger [a :- clojure.core.typed/AnyInteger]
           (+ a 2))
         (clojure.core.typed/Var1 [clojure.core.typed/AnyInteger -> clojure.core.typed/AnyInteger]))
  (is-cf (clojure.core.typed/defn> add-three 
           (:- clojure.core.typed/AnyInteger [a :- clojure.core.typed/AnyInteger]
               (+ a 3)))
         (clojure.core.typed/Var1 [clojure.core.typed/AnyInteger -> clojure.core.typed/AnyInteger])))

(deftest def>-test
  (is (check-ns 'clojure.core.typed.test.def-arrow))
  (is-cf (clojure.core.typed/def> a :- Number 1)
         (clojure.core.typed/Var1 Number)))

(deftest nested-keyword-update-test
  ; ordinary IPersistentMap does not get updated
  (is-cf (fn []
           (let [a (clojure.core.typed/ann-form {} (clojure.core.typed/Map Any Any))]
             (if (number? (-> a :a :b))
               a
               (assert nil))))
         [-> (clojure.core.typed/Map Any Any)])
  ; HMaps can gain "one level" of known entries.
  (is-cf (fn []
           (let [a (clojure.core.typed/ann-form {} '{})]
             (if (number? (-> a :a :b))
               a
               (assert nil))))
         [-> (HMap :optional {:a Any})])
  ; update a (HMap) with (is Any a [(Key :a) (Key :b)])
  ; returns a (HMap :optional {:a Any})
  ; Only one level is updated, we can't say any more about the inner
  ; :b key.
  (is-clj  (let [t (parse-clj '(HMap))
                 path [(->KeyPE :a) (->KeyPE :b)]
                 lo+ (-filter (parse-clj 'Number) 'a path)
                 lo- (-not-filter (parse-clj 'Number) 'a path)
                 expected+ (parse-clj '(HMap :optional {:a Any}))
                 expected- (parse-clj '(HMap :optional {:a Any}))]
             (and (both-subtype? (update t lo+) expected+)
                  (both-subtype? (update t lo-) expected+))))
  ; negative absent keys. The absent entry :a is not a Number (KeyPE does not support defaults), so we
  ; just return the original type
  (is-clj (let [t (parse-type '(HMap :absent-keys #{:a}))]
            (= t
               (update t (-not-filter (parse-clj 'Number) 'a [(->KeyPE :a) (->KeyPE :b)])))))

  ; When we update a (HMap) that has no information about an :a key, sometimes we can prove
  ; the updated type always has an :a key.
  ;
  ; Here we restrict to a '{:a Number} because the path is a Number, which is never nil. We assume
  ; nil is the not-found type.
  (is-clj (let [t (parse-type '(HMap))]
            (both-subtype? (parse-type '(HMap :mandatory {:a Number}))
                           (update t (-filter (RClass-of Number) 'a [(->KeyPE :a)])))))

  ; We restrict (HMap) to (HMap :optional {:a Any}), which is slightly less accurate, because
  ; we can't prove that the HMap :a entry is never nil. 
  (is-clj (let [t (parse-type '(HMap))]
            (both-subtype? (parse-type '(HMap :optional {:a Any}))
                           (update t (-not-filter (RClass-of Number) 'a [(->KeyPE :a)]))))))


(deftest poly-inst-scoping-test
  (is-cf (fn [a] (clojure.core.typed/inst identity foo))
         (All [foo] [Any -> Any]))
  (is-cf
    (fn [f coll]
      (clojure.core.typed/fn> 
        [x :- a
         y :- (clojure.core.typed/Seqable b)]))
    (All [a b] [Any Any -> Any])))

(deftest unparse-free-scoping-test
  (is (= (unparse-type (parse-type '(All [a b] (Fn [Any Any -> Any]))))
         (quote (All [a b] (Fn [Any Any -> Any])))))

  (is (= (clj (unparse-type (parse-type '(TFn [[a :variance :covariant]] a))))
         (quote (TFn [[a :variance :covariant]] a))))
  (is (= '[(All [a b] (Fn [Any Any -> (Fn [a b -> nil])])) {:then tt, :else ff}]
         (cf
           (fn [f coll]
             (clojure.core.typed/fn> 
               [x :- a
                y :- b]))
           (All [a b] [Any Any -> Any])))))

(deftest infer-bounds-test
  (is (= (infer-bounds -any nil)
         (infer-bounds -any -nothing)
         (infer-bounds nil nil)
         (-bounds -any -nothing)))
  (is-clj (let [t (parse-type '(clojure.core.typed/Seq Number))]
            (= (infer-bounds t nil)
               (-bounds t -nothing)))))

(deftest consistent-variance-test
  (is-clj (let [t (parse-type '(TFn [[x :variance :covariant]]
                                    x))]
            (TypeFn-body* (TypeFn-fresh-symbols* t) t)
            true))
  (is-clj (let [t (parse-type '(TFn [[x :variance :contravariant]]
                                    Any))]
            (TypeFn-body* (TypeFn-fresh-symbols* t) t)
            true)))

(deftest hvec-abstract-test
  (is-cf (fn [a b] [(class a) (class b)])
         [Any Any
          -> (HVec [(U nil Class) (U nil Class)]
                   :objects [{:path [Class], :id 0} {:path [Class], :id 1}])]))

; just a sanity check so keyword arguments don't accidentally break
(deftest check-ns-kw-args-test
  (is (check-ns 'clojure.core.typed.test.protocol :collect-only true)))

;(sub? (All [x] (TFn [[a :variance :covariant]] Any))
;      (Rec [m] (TFn [[a :variance :covariant]] m)))

(deftest nested-tfn-test
  (is (check-ns 'clojure.core.typed.test.nested-tfn-operator)))

(deftest parse-forbidden-rec-test
  (is-clj (throws-tc-error?
            (parse-type '(Rec [x] x))))
  (is-clj (throws-tc-error?
            (parse-type '(Rec [x] (I x Number)))))
  (is-clj (throws-tc-error?
            (parse-type '(Rec [x] (U x Number)))))
  (is-clj (throws-tc-error?
            (parse-type '(Rec [x] (U (I x Number) Double))))))

(deftest parse-value-test
  (is-clj (throws-tc-error?
            (parse-type '(Value))))
  (is-clj (throws-tc-error?
            (parse-type '(Value 1 2 3))))
  (is-clj (throws-tc-error?
            (parse-type 'a)))
  (is-clj (throws-tc-error?
            (parse-type ':a)))
  (is-clj (throws-tc-error?
            (parse-type '1))))

(deftest parse-TFn-bad-args-test
  (is-clj (throws-tc-error?
            (parse-type '(TFn [[x :variance :covariant :argh]]
                              Any)))))

(deftest parse-HMap-bad-args-test
  (is-clj (throws-tc-error?
            (parse-type '(HMap :foo))))
  (is-clj (throws-tc-error?
            (parse-type '(HMap :foo :foo))))
  (is-clj (throws-tc-error?
            (parse-type '(HMap :mandatory {} :mandatory {}))))
  (is-clj (throws-tc-error?
            (parse-type '(HMap mandatory {})))))

(deftest hmap-intersection-test
  (is-cf {:a 1} (I '{} '{:a Number}))
  (is-cf {:a 1 :b 2} (I '{:b Number} '{:a Number}))
  (is-cf {:foo 3 :bar "hi"} (I '{:foo clojure.core.typed/Int} '{:bar String}))
  (is-cf {:a 1 :b 2} (I '{:b Number} '{:a Number}))
  (is-cf (do (clojure.core.typed/def-alias HMapAlias1 '{:a Number})
             (clojure.core.typed/def-alias HMapAlias2 '{:b Number})
             (clojure.core.typed/ann-form {:a 1 :b 2}
                                          (I HMapAlias1 HMapAlias2))))
  (is-cf (do (clojure.core.typed/def-alias HMapAliasInt1 '{:foo clojure.core.typed/Int})
             (clojure.core.typed/def-alias HMapAliasStr2 '{:bar String})
             (clojure.core.typed/ann-form {:foo 3 :bar "hi"}
                                          (I HMapAliasInt1 HMapAliasStr2)))))

(deftest rclass-invariant-test
  (is-clj
    (subtype? 
      (RClass-of 'clojure.lang.ChunkBuffer
                 [(RClass-of 'java.lang.Number)])
      (RClass-of 'clojure.lang.ChunkBuffer
                 [(Name-maker 'java.lang.Number)]))))

(deftest protocol-method-ann-test
  ; p
  (is-clj (let [names '[x1 x2]
                bnds [no-bounds no-bounds]
                mt (with-bounded-frees (zipmap (map make-F names)
                                               bnds)
                     (parse-type '(All [m1]
                                    [Any x1 m1 -> x2])))]
            (both-subtype? (collect/protocol-method-var-ann
                             mt names bnds)
                           (parse-type 
                             '(All [x1 x2 m1]
                                [Any x1 m1 -> x2]))))))

(deftest deftype-poly-ancestor-test
  (is (check-ns 'clojure.core.typed.test.protocol-scoping)))

;(deftest parse-with-inferred-variance
;  (is-clj (= (clj (parse-type '(TFn [[x :variance :inferred]] x)))
;             (parse-type '(TFn [[x :variance :covariant]] x)))))

;(sub? (TFn (Rec [m]
;                     (TFn [[x :variance :covariant]]
;                       (Rec [c]
;                         (IColl max-arg m c))))
