(ns clojure.core.typed.test.core
  (:import (clojure.lang ISeq ASeq IPersistentVector Atom IPersistentMap
                         Keyword ExceptionInfo Symbol Var))
  (:require [clojure.test :refer :all]
            [clojure.tools.analyzer :refer [ast]]
            [clojure.tools.analyzer.hygienic :refer [ast-hy]]
            [clojure.repl :refer [pst]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.core.typed :as tc, :refer :all]
            [clojure.core.typed.init]
            [clojure.core.typed.utils :as u :refer [with-ex-info-handlers top-level-error?]]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.check :as chk :refer [expr-type tc-t combine-props env+ update check-funapp]]
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
            [clojure.core.typed.cs-gen :refer :all]
            [clojure.core.typed.cs-rep :refer :all]
            [clojure.core.typed.subst :refer [subst-all]]
            [clojure.core.typed.test.rbt]
            [clojure.core.typed.test.person]
            [clojure.tools.trace :refer [trace-vars untrace-vars
                                         trace-ns untrace-ns]]))

; we want clojure.lang.Seqable to be scoped here. 
; There :refer :all of clojure.core.typed adds another Seqable which
; is less useful here.
(ns-unmap *ns* 'Seqable)
(import (clojure.lang Seqable))

(defn subtype? [& rs]
  (impl/with-clojure-impl
    (apply sub/subtype? rs)))

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
                    (make-FnIntersection (make-Function () -nil nil (DottedPretype-maker -nil 'x)))
                    '(x)))))

(deftest poly-constructor-test
  (is-clj (= (Poly-body*
           '(x)
           (Poly* '(x) [no-bounds]
                  (make-F 'x)
                  '(x)))
         (make-F 'x)))
  (is-clj (= (Poly-body*
           '(x)
           (Poly* '(x)
                  [(Bounds-maker -nil -false nil)]
                  (make-F 'x)
                  '(x)))
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
                                                        nil nil)))
                           '(x x1 y z))))))
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
  (is-clj (not (subtype? (constant-type '{:a nil})
                     (constant-type '{:a 1}))))
  (is-clj (subtype? (constant-type '{:a 1 :b 2 :c 3})
                (constant-type '{:a 1 :b 2}))))

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
                               :var clojure.lang.Var})
                  :then (HMap :mandatory {:op (Value :nil)})
                  :else (HMap :mandatory {:op (Value :false)})})
            (Rec [x] 
                 (U (HMap :mandatory {:op (Value :if)
                           :test x
                           :then x
                           :else x})
                    (HMap :mandatory {:op (Value :var)
                           :var clojure.lang.Var})
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

;return type for an expression f
(defmacro ety [f]
  `(impl/with-clojure-impl
     (-> (ast ~f) ast-hy check expr-type ret-t)))

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
                      :object -empty))
                  '(x)))))
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

(deftest get-special-test
  (is-clj (subtype? 
            (ety 
              (clojure.core.typed/fn> [a :- (HMap :mandatory {:a Number})]
                                      (get a :a)))
            (parse-type
              '(Fn ['{:a java.lang.Number} -> java.lang.Number 
                    :filters {:then (is java.lang.Number 0 [(Key :a)]), 
                              :else (| (is (HMap :absent-keys #{:a}) 0) 
                                       (is (U nil false) 0 [(Key :a)]))} 
                    :object {:path [(Key :a)], :id 0}])))))

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
  (is (= (ety (symbol "a" "a"))
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
  (is-clj (= (ret-t (tc-t (let [a (clojure.core.typed/ann-form [1] (clojure.core.typed/Coll clojure.core.typed/AnyInteger))]
                            (if (seq a)
                              (first a)
                              'a))))
             (parse-type '(U clojure.core.typed/AnyInteger (Value a))))))

(deftest type-fn-test 
  (is-clj (clj
        (= (with-bounded-frees [[(make-F 'm) (-bounds (parse-type '(TFn [[x :variance :covariant]] Any))
                                                      (parse-type '(TFn [[x :variance :covariant]] Nothing)) )]]
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
  (is-clj (tc-t (isa? 1 1)))
  (is-clj (tc-t #(isa? (class %) Number))))

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

(deftest let-filter-unscoping-test
  (is-cf (fn [a]
            (and (< 1 2) a))
          [(U nil Number) -> Any :filters {:then (is Number 0)}]))

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
    (clojure.lang.LazySeq Number))
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
  (is (cf (map-indexed (clojure.core.typed/inst vector clojure.core.typed/AnyInteger Long Any Any Any Any) 
                       [1 2])
          (clojure.lang.Seqable '[clojure.core.typed/AnyInteger Long]))))

(deftest letfn>-test
  (is (cf (clojure.core.typed/letfn> [a :- [Number -> Number]
                                      (a [b] b)]
            (a 1))
          Number))
  ;interdependent functions
  (is (cf (clojure.core.typed/letfn> [a :- [Number -> Number]
                                      (a [c] (b c))
                                      
                                      b :- [Number -> Number]
                                      (b [d] (do a d))]
            (a 1))
          Number)))

;FIXME convert datatypes+records to RClasses
(deftest protocol-untyped-ancestor-test
  (is (check-ns 'clojure.core.typed.test.protocol-untyped-extend)))

(deftest kw-args-fail-test
  (is (caught-top-level-errors #{1}
        (check-ns 'clojure.core.typed.test.kw-args-undeclared-fail))))

(deftest filter-combine-test
  (is (check-ns 'clojure.core.typed.test.filter-combine)))

(deftest or-filter-simplify-test
  ;(| (is-clj T  'a)
  ;   (is-clj T' 'a))
  ; simplifies to
  ;(is-clj (U T T') 'a)
  (is-clj (clj 
        (= (-or (-filter (RClass-of clojure.lang.Symbol) 'id)
                (-filter (RClass-of String) 'id))
           (-filter (Un (RClass-of clojure.lang.Symbol)
                        (RClass-of String))
                    'id)))))

(deftest or-filter-update-test
  (is-clj (clj
        (= (update -any
                   (-or (-filter (RClass-of clojure.lang.Symbol) 'id)
                        (-filter (RClass-of String) 'id)))
           (Un (RClass-of clojure.lang.Symbol)
               (RClass-of String))))))

(deftest path-update-test
  (is-clj (clj (= (update (Un -nil (-hmap {(-val :foo) (RClass-of Number)}))
                      (-not-filter (Un -false -nil) 'id [(->KeyPE :foo)]))
              (-hmap {(-val :foo) (RClass-of Number)}))))
  ; if (:foo a) is nil, either a has a :foo entry with nil, or no :foo entry
  ; TODO
  #_(is-clj (= (update (-hmap {})
                 (-filter -nil 'id [(->KeyPE :foo)]))
         (make-HMap {} {(-val :foo) -nil}))))

(deftest multimethod-test
  (is (check-ns 'clojure.core.typed.test.mm)))

(deftest instance-field-test
  (is (cf (.ns ^clojure.lang.Var #'clojure.core/map))))

(deftest HMap-syntax-test
  (is (= (parse-type '(HMap :absent-keys #{:op}))
         (-hmap {} #{(-val :op)} true))))

(deftest map-filter-test
  (is (cf (clojure.core.typed/ann-form (fn [a] (:op a))
                                       [(U '{:op ':if} '{:op ':case})
                                        -> (U ':if ':case)
                                        :filters {:then (is (U ':case ':if) 0 [(Key :op)])
                                                  :else (| (is (HMap :absent-keys #{:op}) 0)
                                                           (is (U false nil) 0 [(Key :op)]))}
                                        :object {:id 0
                                                 :path [(Key :op)]}])))
  ; {:then (is :if 0 [:op])
  ;  :else (| (! :if 0 [:op])
  ;           (is (HMap :absent-keys #{:op}) 0))}
  (is (cf #(= :if (:op %))
          [(U '{:op ':if} '{:op ':case})
           -> Boolean
           :filters {:then (& (is '{:op (Value :if)} 0)
                              (is ':if 0 [(Key :op)]))
                     :else (! ':if 0 [(Key :op)])}]))
  (is (cf (clojure.core.typed/fn> [a :- (U '{:op ':if} '{:op ':case})
                b :- (U '{:op ':if} '{:op ':case})]
            (if (= :if (:op a))
              (= :case (:op b))
              false))))
  (is (cf (fn [a b] 
            (let [and__3941__auto__ (clojure.core/symbol? a)] 
              (if (clojure.core.typed/print-filterset "test" and__3941__auto__)
                (clojure.core/number? b) 
                and__3941__auto__))))))

(deftest warn-on-unannotated-vars-test
  (is (check-ns 'clojure.core.typed.test.warn-on-unannotated-var)))

(deftest number-ops-test
  (is (cf (min (Integer. 3) 10) Number)))

(defmacro throws-tc-error? [& body]
  `(with-ex-info-handlers
     [u/tc-error? (constantly true)]
     ~@body
     false))

(deftest ctor-infer-test
  (is (cf (java.io.File. "a")))
  (is (cf (let [a (or "a" "b")]
            (java.io.File. a))))
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
  (is (cf (fn [a]
            {:pre [(or (and (vector? a)
                            (not (map? a)))
                       (and (map? a)
                            (not (vector? a))))]}
            (if (vector? a)
              a
              nil))
          [(U (clojure.lang.IPersistentVector Number)
              (clojure.lang.IPersistentMap Any Any))
           -> (U nil (clojure.lang.IPersistentVector Number))]))
  ; or use static types
  (is (cf (fn [a]
            (if (vector? a)
              a
              nil))
          [(U (Extends [(clojure.lang.IPersistentVector Number)]
                       :without [(clojure.lang.IPersistentMap Any Any)])
              (Extends [(clojure.lang.IPersistentMap Any Any)]
                       :without [(clojure.lang.IPersistentVector Any)]))
           -> (U nil (clojure.lang.IPersistentVector Number))]))
  ; technically it's ok to implement Number and IPM
  (is (cf (fn [a]
            {:pre [(number? a)]}
            (clojure.core.typed/print-env "a")
            (+ 1 a))
          [(clojure.lang.IPersistentMap Any Any) -> Number])))

(deftest complete-hash-subtype-test
  (is-clj (sub? (HMap :optional {} :complete? true)
            (clojure.lang.IPersistentMap Integer Long))))

(deftest set!-test
  (is (check-ns 'clojure.core.typed.test.set-bang)))

(deftest flow-unreachable-test
  ; this will always throw a runtime exception, which is ok.
  (is (cf (fn [a] 
            {:pre [(symbol? a)]}
            (clojure.core.typed/print-env "a") 
            (clojure.core.typed/ann-form a clojure.lang.Symbol))
          [Long -> clojure.lang.Symbol])))

; FIXME this is wrong, should not just be nil
#_(deftest array-first-test
  (is (cf (let [a (clojure.core.typed/into-array> Long [1 2])]
            (first a)))))

(deftest every?-update-test
  (is (cf (let [a (clojure.core.typed/ann-form [] (U nil (clojure.core.typed/Coll Any)))]
            (assert (every? number? a))
            a)
          (U nil (clojure.core.typed/Coll Number)))))

(deftest keys-vals-update-test
  (is-clj (= (update (RClass-of IPersistentMap [-any -any])
                     (-filter (RClass-of Seqable [(RClass-of Number)])
                              'a [(->KeysPE)]))
             (RClass-of IPersistentMap [(RClass-of Number) -any])))
  ; test with = instead of subtype to catch erroneous downcast to (IPersistentMap Nothing Any)
  (is-clj (= (tc-t (let [m (clojure.core.typed/ann-form {} (clojure.lang.IPersistentMap Any Any))]
                     (assert (every? number? (keys m)))
                     m))
             (ret (fully-resolve-type (parse-type '(clojure.lang.IPersistentMap Number Any))))))
  (is-clj (= (tc-t (let [m (clojure.core.typed/ann-form {} (clojure.lang.IPersistentMap Any Any))]
                     (assert (every? number? (keys m)))
                     (assert (every? number? (vals m)))
                     m))
             (ret (fully-resolve-type (parse-type '(clojure.lang.IPersistentMap Number Number))))))
  (is (cf (fn [m]
            {:pre [(every? number? (vals m))]}
            m)
          [(clojure.lang.IPersistentMap Any Any) -> (clojure.lang.IPersistentMap Any Number)]))
  (is (cf (fn [m]
            {:pre [(every? symbol? (keys m))
                   (every? number? (vals m))]}
            m)
          [(clojure.lang.IPersistentMap Any Any) -> (clojure.lang.IPersistentMap clojure.lang.Symbol Number)])))

; a sanity test for intersection cache collisions
(deftest intersect-cache-test
  (is-clj (=
       (Poly-body*
         ['foo1 'foo2]
         (Poly* '[x y] 
                [no-bounds no-bounds]
                (In (make-F 'x) (make-F 'y))
                '[x y]))
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
  (is (cf (filter (clojure.core.typed/inst identity (U nil Number)) [1 nil])
          (clojure.lang.Seqable Number))))

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
  (is (cf (let [a (clojure.core.typed/ann-form (fn [a b] (assert false))
                            (All [x y]
                                 [[x -> Any :filters {:then (! y 0)}]
                                  (U nil (clojure.lang.Seqable x)) -> (clojure.lang.LazySeq (I x (Not y)))]))]
            ;need to instantiate negative types for now
            (fn [] (a (clojure.core.typed/inst identity (U nil Number)) [1 nil])))
          [-> (clojure.lang.Seqable Number)])))



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

  (is-clj (= (subst-all {'x (->t-subst (Un (RClass-of Number) -nil) no-bounds) 
                         'y (->t-subst -nil no-bounds)} 
                        (In (make-F 'x) (NotType-maker (make-F 'y))))
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
  (is (cf (first (clojure.core.typed/ann-form [] Iterable)))))

; See CTYP-29 for discussion. f in (map f coll) needs to be only a single arity
; to help inference.
(deftest map-over-multiarity-fn-test
  (is (cf (map (clojure.core.typed/ann-form + [Number -> Number]) 
               (clojure.core.typed/ann-form [] (clojure.lang.Seqable Number)))))
  (is (cf (map inc [(or (first (range)) 0) 1])
          (clojure.lang.Seqable clojure.core.typed/AnyInteger)))
  (is (cf (fn [x] (map (clojure.core.typed/ann-form inc [Number -> Number]) [x 1]))
          [Number -> (clojure.lang.Seqable Number)]))
  (is-clj (subtype? (ret-t (tc-t [(or (first (range)) 2) 1]))
                (RClass-of Seqable [(RClass-of Number)])))
  (is (cf (fn [x] 
            (map (clojure.core.typed/ann-form inc [Number -> Number]) 
                 [x 2 3])) 
          [Number -> (clojure.lang.LazySeq Number)])))

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
  (is (thrown? Error (clj (parse-type '(HMap :mandatory {:a Any} :absent-keys #{:a}))))))

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
  (is (cf {:a #(+ % 1)} (HMap :optional {:a [Number -> Number]}))))

(deftest fnil-test
  (is (cf ((fnil + 0) 2)))
  (is (cf ((fnil + 0) nil)))
  ; can Typed Racket do better here?
  (is (cf ((fnil (clojure.core.typed/ann-form + [Number * -> Number])
                 0) 
           2.2))))

(cf (every? (fn [a] a) [1]))

;
;TODO destructuring on records
;TODO this is non-nil (last (take 100 (iterate update-without-plot initial-state)))
;TODO 
;          {final-grid :grid,
;           :as final-state} (last (take 100 (iterate update-without-plot initial-state)))
;          _ (assert final-state)
;          ; be smart enough to infer final-grid cannot be nil just from the above assertion.
;          _ (assert final-grid)

;TODO support (some #{...} coll)
;TODO (apply == (non-empty-seq))
;TODO tests for inferring upper/lower bounds
