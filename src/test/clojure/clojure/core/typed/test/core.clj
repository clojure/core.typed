(ns clojure.core.typed.test.core
  (:refer-clojure :exclude [defrecord])
  (:import (clojure.lang Seqable ISeq ASeq IPersistentVector Atom IPersistentMap
                         Keyword))
  (:require [clojure.test :refer :all]
            [analyze.core :refer [ast]]
            [analyze.hygienic :refer [ast-hy]]
            [clojure.repl :refer [pst]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.core.typed :as tc, :refer :all, :exclude [subtype? check]]
            [clojure.core.typed.test.rbt]
            [clojure.core.typed.test.person]
            [clojure.tools.trace :refer [trace-vars untrace-vars
                                         trace-ns untrace-ns]]))

(defn subtype? [& rs]
  (ensure-clojure)
  (apply tc/subtype? rs))

(defn check [& as]
  (ensure-clojure)
  (apply tc/check as))

;(check-ns 'clojure.core.typed.test.deftype)

(deftest add-scopes-test
  (is (let [body (make-F 'a)]
        (= (add-scopes 0 body)
           body)))
  (is (let [body (make-F 'a)]
        (= (add-scopes 1 body)
           (->Scope body))))
  (is (let [body (make-F 'a)]
        (= (add-scopes 3 body)
           (-> body ->Scope ->Scope ->Scope)))))

(deftest remove-scopes-test
  (is (let [scope (->Scope (make-F 'a))]
        (= (remove-scopes 0 scope)
           scope)))
  (is (let [body (make-F 'a)]
        (= (remove-scopes 1 (->Scope body))
           body))))

(deftest parse-type-test
  (is (= (Poly-body* '(x) (parse-type '(All [x] x)))
         (make-F 'x)))
  (is (= (Poly-body* '(x y) (parse-type '(All [x y] x)))
         (make-F 'x)))
  (is (= (Poly-body* '(x y) (parse-type '(All [x y] y)))
         (make-F 'y)))
  (is (= (Poly-body* '(a b c d e f g h i) (parse-type '(All [a b c d e f g h i] e)))
         (make-F 'e))))

(deftest parse-type-fn-test
  (is (= (parse-type '[nil * -> nil])
         (make-FnIntersection (make-Function () -nil -nil))))
  (is (= (parse-type '(All [x ...] [nil ... x -> nil]))
         (PolyDots* '(x) [no-bounds]
                    (make-FnIntersection (make-Function () -nil nil (->DottedPretype -nil 'x)))))))

(deftest poly-constructor-test
  (is (= (Poly-body*
           '(x)
           (Poly* '(x) [no-bounds]
                  (make-F 'x)
                  '(x)))
         (make-F 'x)))
  (is (= (Poly-body*
           '(x)
           (Poly* '(x)
                  [(->Bounds -nil -false nil)]
                  (make-F 'x)
                  '(x)))
         (make-F 'x)))
  (is (= (parse-type '(All [x x1 [y :< x] z] [x -> y]))
         (let [no-bounds-scoped (->Bounds
                                  (add-scopes 4 -any)
                                  (add-scopes 4 (Un))
                                  nil)]
           (->Poly 4
                   [no-bounds-scoped
                    no-bounds-scoped
                    (->Bounds 
                      (add-scopes 4 (->B 3))
                      (add-scopes 4 (Un))
                      nil)
                    no-bounds-scoped]
                   (add-scopes 4
                               (make-FnIntersection
                                 (make-Function [(->B 3)] (->B 1)
                                                nil nil)))
                   '(x x1 y z))))))
(defmacro sub? [s t]
  `(subtype? (parse-type '~s)
             (parse-type '~t)))

(deftest subtype-test
  (is (subtype? (parse-type 'Integer)
                (parse-type 'Integer)))
  (is (subtype? (parse-type 'Integer)
                (parse-type 'Object)))
  (is (not (sub? Object Integer)))
  (is (sub? Object Object))
  (is (subtype? (parse-type 'Integer)
                (parse-type 'Number)))
  (is (subtype? (parse-type '(clojure.lang.Seqable Integer))
                (parse-type '(clojure.lang.Seqable Integer))))
  (is (subtype? (parse-type '(clojure.lang.Seqable Integer))
                (parse-type '(clojure.lang.Seqable Number))))
  (is (not
        (subtype? (parse-type '(clojure.lang.Seqable Number))
                  (parse-type '(clojure.lang.Seqable Integer)))))
  (is (subtype? (parse-type '(clojure.lang.Cons Integer))
                (parse-type '(clojure.lang.Cons Number))))
  (is (subtype? (parse-type '(clojure.lang.Cons Integer))
                (parse-type '(clojure.lang.Seqable Number)))))

(deftest subtype-java-exceptions-test
  (is (subtype? (RClass-of IndexOutOfBoundsException nil)
                (RClass-of Exception nil))))

(deftest subtype-intersection
  (is (not (subtype? (RClass-of Seqable [-any])
                     (In (RClass-of Seqable [-any])
                         (make-CountRange 1))))))

(deftest subtype-Object
  (is (subtype? (RClass-of clojure.lang.IPersistentList [-any]) (RClass-of Object nil))))

(deftest subtype-hmap
  (is (not (subtype? (constant-type '{:a nil})
                     (constant-type '{:a 1}))))
  (is (subtype? (constant-type '{:a 1 :b 2 :c 3})
                (constant-type '{:a 1 :b 2}))))

(deftest subtype-poly
  (is (subtype? (parse-type '(All [x] (clojure.lang.ASeq x)))
                (parse-type '(All [y] (clojure.lang.Seqable y))))))

(deftest subtype-rec
  (is (subtype? (parse-type 'Integer)
                (parse-type '(Rec [x] (U Integer (clojure.lang.Seqable x))))))
  (is (subtype? (parse-type '(clojure.lang.Seqable (clojure.lang.Seqable Integer)))
                (parse-type '(Rec [x] (U Integer (clojure.lang.Seqable x))))))
  (is (not (subtype? (parse-type 'Number)
                     (parse-type '(Rec [x] (U Integer (clojure.lang.Seqable x)))))))
  (is (sub (HMap {:op (Value :if)
                  :test (HMap {:op (Value :var)
                               :var clojure.lang.Var})
                  :then (HMap {:op (Value :nil)})
                  :else (HMap {:op (Value :false)})})
            (Rec [x] 
                 (U (HMap {:op (Value :if)
                           :test x
                           :then x
                           :else x})
                    (HMap {:op (Value :var)
                           :var clojure.lang.Var})
                    (HMap {:op (Value :nil)})
                    (HMap {:op (Value :false)})))))

 #_(is (sub? (Rec [x] (U Integer (clojure.lang.ILookup x x)))
            (Rec [x] (U Number (clojure.lang.ILookup x x)))))
  )

;FIXME expanding dotted pretypes
#_(deftest trans-dots-test
  (is (= (manual-inst (parse-type '(All [x b ...]
                                        [x ... b -> x]))
                      (map parse-type '(Integer Double Float)))
         (parse-type '[Integer Integer -> Integer])))
  (is (= (manual-inst (parse-type '(All [x b ...]
                                        [b ... b -> x]))
                      (map parse-type '(Integer Double Float)))
         (parse-type '[Double Float -> Integer])))
  ;map type
  (is (= (manual-inst (parse-type '(All [c a b ...]
                                        [[a b ... b -> c] (clojure.lang.Seqable a) (clojure.lang.Seqable b) ... b -> (clojure.lang.Seqable c)]))
                      (map parse-type '(Integer Double Float)))
         (parse-type '[[Double Float -> Integer] (clojure.lang.Seqable Double) (clojure.lang.Seqable Float) -> (clojure.lang.Seqable Integer)]))))

;return type for an expression f
(defmacro ety [f]
  `(do (ensure-clojure)
     (-> (ast ~f) ast-hy check expr-type ret-t)))

(deftest tc-invoke-fn-test
  (is (subtype? (ety
                  ((clojure.core.typed/fn> [[a :- Number] [b :- Number]] b)
                     1 2))
                (parse-type 'Number)))
  ; manual instantiation "seq"
  (is (subtype? (ety
                  ((clojure.core.typed/fn> [[a :- (clojure.lang.Seqable Number)] [b :- Number]] 
                                   ((clojure.core.typed/inst seq Number) a))
                     [1 2 1.2] 1))
                (parse-type '(clojure.core.typed/Option (I (clojure.lang.ISeq java.lang.Number) (CountRange 1))))))
  ; inferred "seq"
  (is (= (ety
           (clojure.core.typed/fn> [[a :- (clojure.lang.Seqable Number)] 
                            [b :- Number]] 
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
  #_(is (= (ety
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
  (is (subtype? (ety
                  ((clojure.core.typed/fn> [[a :- (clojure.lang.Seqable Number)] [b :- Number]] 
                                   (seq a))
                     [1 2 1.2] 1))
                (parse-type '(U nil (I (CountRange 1) (clojure.lang.ISeq Number))))))
  (is (subtype? (ety
                  ((clojure.core.typed/fn> [[a :- (clojure.lang.IPersistentMap Any Number)] [b :- Number]] 
                                   ((clojure.core.typed/inst get Number) a b))
                     (zipmap [1] [2]) 1))
                (parse-type '(U nil Number)))))

(deftest get-special-test
  (is (= (ety 
           (clojure.core.typed/fn> [[a :- (HMap {:a Number})]]
                           (get a :a)))
         (make-FnIntersection
           (make-Function [(-hmap {(-val :a) (RClass-of Number)})]
                          (RClass-of Number)
                          nil nil
                          :filter (-FS (-filter (RClass-of Number) 0 [(->KeyPE :a)])
                                       -top)
                          :object (->Path [(->KeyPE :a)] 0))))))

(deftest truth-false-values-test
  (is (= (tc-t (if nil 1 2))
         (ret (->Value 2) (-FS -top -bot) (->EmptyObject))))
  (is (= (tc-t (if false 1 2))
         (ret (->Value 2) (-FS -top -bot) (->EmptyObject))))
  (is (= (tc-t (if 1 1 2))
         (ret (->Value 1) (-FS -top -bot) (->EmptyObject)))))

(deftest empty-fn-test
  (is (= (tc-t (clojure.core/fn []))
         (ret (make-FnIntersection
                (->Function [] (make-Result -nil
                                            (-FS -bot -top)
                                            (->EmptyObject))
                            nil nil nil))
              (-FS -top -bot)
              (->EmptyObject))))
  (is (= (tc-t (fn [] 1))
         (ret (make-FnIntersection
                (->Function [] (make-Result (->Value 1)
                                              (-FS -top -bot)
                                              (->EmptyObject))
                              nil nil nil))
              (-FS -top -bot)
              (->EmptyObject))))
  (is (= (tc-t (let []))
         (ret -nil (-FS -bot -top) (->EmptyObject)))))

(deftest path-test
  (is (= (tc-t (fn [a] (let [a 1] a)))
         (ret (make-FnIntersection
                (->Function [-any]
                              (make-Result (-val 1)
                                           (-FS -top -top)
                                           -empty)
                              nil nil nil))
              (-FS -top -bot) -empty)))
  (is (= (tc-t (let [a nil] a))
         (ret -nil (-FS -top -top) -empty))))

(deftest equiv-test
  (is (= (tc-t (= 1))
         (tc-t (= 1 1))
         (tc-t (= 1 1 1 1 1 1 1 1 1 1))
         (ret (Un -true -false) (-FS -top -top) (->EmptyObject))))
  (is (= (tc-t (= 'a 'b))
         (tc-t (= 1 2))
         (tc-t (= :a :b))
         (tc-t (= :a 1 'a))
         (ret (Un -true -false) (-FS -top -top) -empty)))
  (is (= (tc-t (= :Val (-> {:a :Val} :a)))
         (ret (Un -true -false) (-FS -top -top) -empty))))

(deftest name-to-param-index-test
  ;a => 0
  (is (= (tc-t 
           (clojure.core.typed/fn> [[a :- (U (HMap {:op (Value :if)})
                                     (HMap {:op (Value :var)}))]] 
                           (:op a)))
         (ret (make-FnIntersection
                (->Function
                    [(Un (-hmap {(->Value :op) (->Value :if)})
                         (-hmap {(->Value :op) (->Value :var)}))]
                    (let [t (Un (->Value :if) (->Value :var))
                          i 0
                          p [(->KeyPE :op)]]
                      (make-Result t
                                   (-FS (-filter t i p) -top)
                                   (->Path p 0)))
                    nil nil nil))
                  (-FS -top -bot)
                  -empty))))

(deftest refine-test
  (is (= (tc-t 
           (clojure.core.typed/fn> [[a :- (U (HMap {:op (Value :if)})
                                     (HMap {:op (Value :var)}))]] 
                           (when (= (:op a) :if) 
                             a)))
         (ret (make-FnIntersection
                (->Function
                    [(Un (-hmap {(-val :op) (-val :if)})
                         (-hmap {(-val :op) (-val :var)}))]
                    (make-Result (Un -nil (-hmap {(-val :op) (-val :if)}))
                                 (-FS (-and (-filter (-val :if) 0 [(->KeyPE :op)])
                                            (-not-filter (Un -false -nil) 0)
                                            (-filter (-hmap {(-val :op) (-val :if)}) 0))
                                           ; what are these filters doing here?
                                      (-or (-and (-filter (->Value :if) 0 [(->KeyPE :op)])
                                                 (-filter (Un -false -nil) 0))
                                           (-not-filter (->Value :if) 0 [(->KeyPE :op)])))
                                 -empty)
                    nil nil nil))
              (-FS -top -bot)
              -empty))))


#_(deftest dotted-infer-test
  (is (cf (map number? [1]))))

(deftest check-invoke
  (is (thrown? Exception (ety (symbol "a" 'b))))
  (is (= (ety (symbol "a" "a"))
         (RClass-of clojure.lang.Symbol))))

(deftest check-do-test
  (is (= (ety (do 1 2))
         (->Value 2))))

(deftest tc-var-test
  (is (= (tc-t seq?)
         (ret (make-FnIntersection
                (->Function [-any]
                              (make-Result (RClass-of 'boolean) 
                                           (-FS (-filter (RClass-of ISeq [-any]) 0)
                                                (-not-filter (RClass-of ISeq [-any]) 0))
                                           -empty)
                              nil nil nil))
              (-FS -top -top) -empty))))

(deftest heterogeneous-ds-test
  (is (not (subtype? (parse-type '(HMap {:a (Value 1)}))
                     (RClass-of ISeq [(->Top)]))))
  (is (not (subtype? (parse-type '(Vector* (Value 1) (Value 2)))
                     (RClass-of ISeq [(->Top)]))))
  (is (subtype? (parse-type '(Seq* (Value 1) (Value 2)))
                (RClass-of ISeq [(->Top)])))
  (is (subtype? (parse-type '(List* (Value 1) (Value 2)))
                (RClass-of ISeq [(->Top)])))
  (is (= (tc-t [1 2])
         (ret (->HeterogeneousVector [(->Value 1) (->Value 2)]) -true-filter -empty)))
  (is (= (tc-t '(1 2))
         (ret (->HeterogeneousList [(->Value 1) (->Value 2)]) -true-filter -empty)))
  (is (= (tc-t {:a 1})
         (ret (-complete-hmap {(->Value :a) (->Value 1)}) -true-filter -empty)))
  (is (= (tc-t {})
         (ret (-complete-hmap {}) -true-filter -empty)))
  (is (= (tc-t [])
         (ret (->HeterogeneousVector []) -true-filter -empty)))
  (is (= (tc-t '())
         (ret (->HeterogeneousList []) -true-filter -empty))))

(deftest implied-atomic?-test
  (is (implied-atomic? (-not-filter -false 'a)(-not-filter (Un -nil -false) 'a))))

(deftest combine-props-test
  (is (= (map set (combine-props [(->ImpFilter (-not-filter -false 'a)
                                               (-filter -true 'b))]
                                 [(-not-filter (Un -nil -false) 'a)]
                                 (atom true)))
         [#{} #{(-not-filter (Un -nil -false) 'a)
                (-filter -true 'b)}])))

(deftest env+-test
  ;test basic TypeFilter
  ;update a from Any to (Value :a)
  (is (let [props [(-filter (->Value :a) 'a)]
            flag (atom true)]
        (and (= (let [env {'a -any}
                      lenv (->PropEnv env props)]
                  (env+ lenv [] flag))
                (->PropEnv {'a (->Value :a)} props))
             @flag)))
  ;test positive KeyPE
  ;update a from (U (HMap {:op :if}) (HMap {:op :var})) => (HMap {:op :if})
  (is (let [props [(-filter (->Value :if) 'a [(->KeyPE :op)])]
            flag (atom true)]
        (and (= (let [env {'a (Un (-hmap {(->Value :op) (->Value :if)})
                                  (-hmap {(->Value :op) (->Value :var)}))}
                      lenv (->PropEnv env props)]
                  (env+ lenv [] flag))
                (->PropEnv {'a (-hmap {(->Value :op) (->Value :if)})} props))
             @flag)))
  ;test negative KeyPE
  (is (let [props [(-not-filter (->Value :if) 'a [(->KeyPE :op)])]
            flag (atom true)]
        (and (= (let [env {'a (Un (-hmap {(->Value :op) (->Value :if)})
                                  (-hmap {(->Value :op) (->Value :var)}))}
                      lenv (->PropEnv env props)]
                  (env+ lenv [] flag))
                (->PropEnv {'a (-hmap {(->Value :op) (->Value :var)})} props))
             @flag)))
  ;test impfilter
  (is (let [{:keys [l props]}
            (env+ (->PropEnv {'a (Un -false -true) 'b (Un -nil -true)}
                             [(->ImpFilter (-not-filter -false 'a)
                                           (-filter -true 'b))])
                  [(-not-filter (Un -nil -false) 'a)]
                  (atom true))]
        (and (= l {'a -true, 'b -true})
             (= (set props)
                #{(-not-filter (Un -nil -false) 'a)
                  (-filter -true 'b)}))))
  ; more complex impfilter
  (is (= (env+ (->PropEnv {'and1 (Un -false -true)
                           'tmap (->Name 'clojure.core.typed.test.core/UnionName)}
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
  (is (= (:l (env+ (->PropEnv {'and1 (RClass-of Seqable [-any])} [])
                   [(-filter (RClass-of IPersistentVector [-any]) 'and1)]
                   (atom true)))
         {'and1 (RClass-of IPersistentVector [-any])})))

(deftest destructuring-special-ops
  ;FIXME for destructuring rest args
;  (is (= (tc-t (let [a '(a b)]
;                 (seq? a)))
;         (ret -true -true-filter -empty)))
  (is (= (tc-t (let [a {:a 1}]
                 (if (seq? a)
                   (apply hash-map a)
                   a)))
         (ret (-hmap {(->Value :a) (->Value 1)})
              ;FIXME should true-filter ?
              (-FS -top -top) -empty)))
  (is (= (tc-t (clojure.core.typed/fn> [[{a :a} :- (HMap {:a (Value 1)})]]
                               a))
         (ret (make-FnIntersection 
                (->Function [(-hmap {(->Value :a) (->Value 1)})]
                              (make-Result (->Value 1) 
                                           (-FS -top -top)  ; have to throw out filters whos id's go out of scope
                                           ;(->Path [(->KeyPE :a)] 0) ; requires 'equivalence' filters
                                           -empty)
                              nil nil nil))
              (-FS -top -bot)
              -empty)))
  ;FIXME inferred filters are bit messy, but should be (-FS -bot (! Seq 0))
  #_(is (= (-> (tc-t (clojure.core.typed/fn> [[a :- clojure.core.typed.test.core/UnionName]]
                                   (seq? a)))
           ret-t)
         (make-FnIntersection
           (->Function [(->Name 'clojure.core.typed.test.core/UnionName)]
                         (make-Result -false 
                                      ;FIXME why isn't this (-FS -bot (-not-filter (RClass-of ISeq [-any]) 0)) ?
                                      (-FS -bot -top)
                                      -empty)
                         nil nil nil))))
  (is (= (tc-t (let [{a :a} {:a 1}]
                 a))
         (ret (->Value 1) 
              (-FS -top -top) ; a goes out of scope, throw out filters
              -empty)))
  ;FIXME should be (-FS -bot (! ISeq 0))
  #_(is (= (tc-t (clojure.core.typed/fn> [[a :- (HMap {:a (Value 1)})]]
                               (seq? a)))
         (ret (make-FnIntersection
                (->Function [(-hmap {(->Value :a) (->Value 1)})]
                              (make-Result -false -false-filter -empty)
                              nil nil nil))
              (-FS -top -bot)
              -empty)))
  ;roughly the macroexpansion of map destructuring
  (is (= (tc-t (clojure.core.typed/fn> 
                 [[map-param :- clojure.core.typed.test.rbt/badRight]]
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
  ; NOTE: commented out because, for now, it's an error to get a non-existant key
;  (is (= (tc-t (clojure.core.typed/fn> [[{a :a} :- (U (HMap {:a (Value 1)})
;                                              (HMap {:b (Value 2)}))]]
;                               a))
;         (ret (make-FnIntersection (->Function [(Un (-hmap {(->Value :a) (->Value 1)})
;                                   (-hmap {(->Value :b) (->Value 2)}))]
;                              (make-Result (Un (->Value 1) -nil) (-FS -top -top) -empty)
;                              nil nil nil))
;              (-FS -top -bot)
;              -empty)))
              )

(def-alias MyName (HMap {:a (Value 1)}))
(def-alias MapName (HMap {:a clojure.core.typed.test.core/MyName}))

(def-alias MapStruct1 (HMap {:type (Value :MapStruct1)
                             :a clojure.core.typed.test.core/MyName}))
(def-alias MapStruct2 (HMap {:type (Value :MapStruct2)
                             :b clojure.core.typed.test.core/MyName}))
(def-alias UnionName (U MapStruct1 MapStruct2))

(deftest Name-resolve-test
  (is (= (tc-t (clojure.core.typed/fn> [[tmap :- clojure.core.typed.test.core/MyName]]
                               ;call to (apply hash-map tmap) should be eliminated
                               (let [{e :a} tmap]
                                 e)))
         (ret (make-FnIntersection 
                (->Function [(->Name 'clojure.core.typed.test.core/MyName)]
                              (make-Result (->Value 1) (-FS -top -top) -empty)
                              nil nil nil))
              (-FS -top -bot) -empty)))
  (is (= (tc-t (clojure.core.typed/fn> [[tmap :- clojure.core.typed.test.core/MapName]]
                               (let [{e :a} tmap]
                                 (assoc e :c :b))))
         (ret (make-FnIntersection (->Function [(->Name 'clojure.core.typed.test.core/MapName)]
                                               (make-Result (-hmap {(->Value :a) (->Value 1)
                                                                    (->Value :c) (->Value :b)})
                                                            (-FS -top -bot) -empty)
                              nil nil nil))
              (-FS -top -bot) -empty)))
  ; Name representing union of two maps, both with :type key
  (is (subtype? 
        (-> (tc-t (clojure.core.typed/fn> [[tmap :- clojure.core.typed.test.core/UnionName]]
                                  (:type tmap)))
          ret-t)
        (parse-type '[clojure.core.typed.test.core/UnionName -> (U (Value :MapStruct2)
                                                      (Value :MapStruct1))])))
  ; using = to derive paths
  (is (subtype? 
        (-> (tc-t (clojure.core.typed/fn> [[tmap :- clojure.core.typed.test.core/UnionName]]
                                  (= :MapStruct1 (:type tmap))))
          ret-t)
        (make-FnIntersection 
          (make-Function 
            [(->Name 'clojure.core.typed.test.core/UnionName)]
            (Un -false -true)
            nil nil
            :filter (let [t (-val :MapStruct1)
                          path [(->KeyPE :type)]]
                      (-FS (-and 
                             (-filter (-hmap {(-val :type) (-val :MapStruct1)
                                              (-val :a) (->Name 'clojure.core.typed.test.core/MyName)})
                                      0)
                             (-filter (-val :MapStruct1) 0 path)
                             (-filter t 0 path))
                           (-not-filter t 0 path)))))))
  ; using filters derived by =
  (is (subtype? (-> (tc-t (clojure.core.typed/fn> [[tmap :- clojure.core.typed.test.core/UnionName]]
                                          (if (= :MapStruct1 (:type tmap))
                                            (:a tmap)
                                            (:b tmap))))
                  ret-t)
                (parse-type '[clojure.core.typed.test.core/UnionName -> clojure.core.typed.test.core/MyName])))
  ; following paths with test of conjuncts
  ;FIXME
  #_(is (= (tc-t (clojure.core.typed/fn> [[tmap :- clojure.core.typed.test.core/UnionName]]
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
         (ret (make-FnIntersection (->Function [(->Name 'clojure.core.typed.test.core/UnionName)]
                              (let [t (Un (-val 1)
                                          (-hmap {(-val :type) (-val :MapStruct1)
                                                               (-val :c) (-val :d)
                                                               (-val :a) (->Name 'clojure.core.typed.test.core/MyName)}))]
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
  (is (= (update (Un (-hmap {(-val :type) (-val :Map1)})
                     (-hmap {(-val :type) (-val :Map2)}))
                 (-filter (->Value :Map1) 'tmap [(->KeyPE :type)]))
         (-hmap {(-val :type) (-val :Map1)})))
  ;test that update resolves Names properly
  (is (= (update (->Name 'clojure.core.typed.test.core/MapStruct2)
                 (-filter (-val :MapStruct1) 'tmap [(->KeyPE :type)]))
         (Un)))
  ;test that update resolves Names properly
  ; here we refine the type of tmap with the equivalent of following the then branch 
  ; with test (= :MapStruct1 (:type tmap))
  (is (= (update (->Name 'clojure.core.typed.test.core/UnionName)
                 (-filter (->Value :MapStruct1) 'tmap [(->KeyPE :type)]))
         (-hmap {(-val :type) (-val :MapStruct1) 
                              (-val :a) (->Name 'clojure.core.typed.test.core/MyName)})))
  (is (= (update (->Name 'clojure.core.typed.test.core/UnionName)
                 (-not-filter (->Value :MapStruct1) 'tmap [(->KeyPE :type)]))
         (-hmap {(-val :type) (-val :MapStruct2) 
                              (-val :b) (->Name 'clojure.core.typed.test.core/MyName)})))
  (is (= (update (Un -true -false) (-filter (Un -false -nil) 'a nil)) 
         -false)))

(deftest overlap-test
  (is (not (overlap -false -true)))
  (is (not (overlap (-val :a) (-val :b))))
  (is (overlap (RClass-of Number) (RClass-of Integer)))
  (is (not (overlap (RClass-of Number) (RClass-of clojure.lang.Symbol))))
  (is (not (overlap (RClass-of Number) (RClass-of String))))
  (is (overlap (RClass-of clojure.lang.Seqable [-any]) (RClass-of clojure.lang.IMeta [-any])))
  (is (overlap (RClass-of clojure.lang.Seqable [-any]) (RClass-of clojure.lang.PersistentVector [-any])))
  )

(def-alias SomeMap (U (HMap {:a (Value :b)})
                      (HMap {:b (Value :c)})))

(deftest assoc-test
  (is (= (tc-t (assoc {} :a :b))
         (ret (-complete-hmap {(->Value :a) (->Value :b)})
              (-FS -top -bot)
              -empty)))
  ;see `invoke-special` for assoc for TODO
  ;FIXME
  #_(is (= (-> (tc-t (-> (fn [m]
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
         
(comment
(-> (tc-t (clojure.core.typed/fn> [[tmap :- clojure.core.typed.test.rbt/badRight]]
                          (and (= :Black (-> tmap :tree))
                               (= :Red (-> tmap :left :tree))
                               (= :Red (-> tmap :right :tree))
                               (= :Red (-> tmap :right :left :tree)))))
                          ;(and (tc-pr-filters "first filter"
                          ;       (= :Black (-> tmap :tree)))
                          ;     (tc-pr-filters "second filter"
                          ;       (= :Red (-> tmap :left :tree)))
                          ;     (tc-pr-filters "third filter"
                          ;       (= :Red (-> tmap :right :tree)))
                          ;     (tc-pr-filters "fourth filter"
                          ;       (= :Red (-> tmap :right :left :tree))))
  ret-t :types first :rng :fl :else unparse-filter pprint)
)

;FIXME
#_(deftest filter-simplification
  (is (= (read-string "#clojure.core.typed.OrFilter{:fs #{#clojure.core.typed.NotTypeFilter{:type #clojure.core.typed.Value{:val :Black}, :path (#clojure.core.typed.KeyPE{:val :tree}), :id 0} #clojure.core.typed.AndFilter{:fs #{#clojure.core.typed.TypeFilter{:type #clojure.core.typed.Value{:val :Black}, :path (#clojure.core.typed.KeyPE{:val :tree}), :id 0} #clojure.core.typed.OrFilter{:fs #{#clojure.core.typed.NotTypeFilter{:type #clojure.core.typed.Value{:val :Red}, :path (#clojure.core.typed.KeyPE{:val :left} #clojure.core.typed.KeyPE{:val :tree}), :id 0} #clojure.core.typed.AndFilter{:fs #{#clojure.core.typed.TypeFilter{:type #clojure.core.typed.Value{:val :Red}, :path (#clojure.core.typed.KeyPE{:val :left} #clojure.core.typed.KeyPE{:val :tree}), :id 0} #clojure.core.typed.OrFilter{:fs #{#clojure.core.typed.AndFilter{:fs #{#clojure.core.typed.TypeFilter{:type #clojure.core.typed.Value{:val :Red}, :path (#clojure.core.typed.KeyPE{:val :right} #clojure.core.typed.KeyPE{:val :tree}), :id 0} #clojure.core.typed.NotTypeFilter{:type #clojure.core.typed.Value{:val :Red}, :path (#clojure.core.typed.KeyPE{:val :right} #clojure.core.typed.KeyPE{:val :left} #clojure.core.typed.KeyPE{:val :tree}), :id 0}}} #clojure.core.typed.NotTypeFilter{:type #clojure.core.typed.Value{:val :Red}, :path (#clojure.core.typed.KeyPE{:val :right} #clojure.core.typed.KeyPE{:val :tree}), :id 0}}}}}}}}}}}")
         (-or 
          (-not-filter (-val :Black) 0 [(-kpe :tree)]) 
           (-and 
             (-filter (-val :Black) 0 [(-kpe :tree)]) 
             (-or 
               (-and (-filter (-val :Red) 0 (map -kpe [:left :tree]))
                     (-or (-not-filter (-val  :Red) 0 (map -kpe [:right :tree]))
                          (-and (-not-filter (-val :Red) 0 (map -kpe [:right :left :tree]))
                                (-filter (-val :Red) 0 (map -kpe [:right :tree]))))) 
               (-not-filter (-val :Red) 0 (:left :tree)))))
         (-or (-not-filter (-val :Black) 0 [(->KeyPE :tree)])
              (-not-filter (-val :Red) 0 [(->KeyPE :right) (->KeyPE :tree)])
              (-not-filter (-val :Red) 0 (map ->KeyPE [:right :left :tree]))
              (-not-filter (-val :Red) 0 (map ->KeyPE [:left :tree]))))))

(deftest update-nested-hmap-test
  (is (= (update (-hmap {(-val :left) (->Name 'clojure.core.typed.test.rbt/rbt)})
                 (-filter (-val :Red) 'id [(->KeyPE :left) (->KeyPE :tree)]))
         (-hmap {(-val :left) 
                              (-hmap {(-val :tree) (-val :Red) 
                                                   (-val :entry) (->Name 'clojure.core.typed.test.rbt/EntryT) 
                                                   (-val :left) (->Name 'clojure.core.typed.test.rbt/bt) 
                                                   (-val :right) (->Name 'clojure.core.typed.test.rbt/bt)})}))))
         
(deftest rbt-test

  (is (= (tc-t (clojure.core.typed/fn> [[tmap :- clojure.core.typed.test.rbt/badRight]]
                               (let [and1 (= :Black (-> tmap :tree))]
                                 #_(tc-pr-env "first clause")
                                 (if and1
                                   (let [and1 (= :Red (-> tmap :left :tree))]
                                     #_(tc-pr-env "second then clause")
                                     (if and1
                                       (let [and1 (= :Red (-> tmap :right :tree))]
                                         #_(tc-pr-env "third then clause")
                                         (if and1
                                           (= :Red (-> tmap :right :left :tree))
                                           (do #_(tc-pr-env "last clause")
                                             and1)))
                                       (do #_(tc-pr-env "third else clause")
                                         and1)))
                                   (do #_(tc-pr-env "second else clause")
                                     and1))))))))

(deftest check-get-keyword-invoke-test
  ;truth valued key
  (is (= (tc-t (let [a {:a 1}]
                 (:a a)))
         (ret (->Value 1) (-FS -top -top) -empty)))
  ;false valued key, a bit conservative in filters for now
  (is (= (tc-t (let [a {:a nil}]
                 (:a a)))
         (ret -nil (-FS -top -top) -empty)))
  ;multiple levels
  (is (= (tc-t (let [a {:c {:a :b}}]
                 (-> a :c :a)))
         (ret (->Value :b) (-FS -top -top) -empty)))
  (is (= (tc-t (clojure.core/get {:a 1} :a))
         (tc-t (clojure.lang.RT/get {:a 1} :a))
         ;FIXME
         #_(tc-t ({:a 1} :a))
         (tc-t (:a {:a 1}))
         (ret (->Value 1)
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
  (is (= (promote-var (make-F 'x) '#{x})
         (->Top)))
  (is (= (demote-var (make-F 'x) '#{x})
         (Bottom)))
  (is (= (promote-var (RClass-of clojure.lang.ISeq [(make-F 'x)]) '#{x})
         (RClass-of clojure.lang.ISeq [(->Top)])))
  (is (= (demote-var (RClass-of clojure.lang.ISeq [(make-F 'x)]) '#{x})
         (RClass-of clojure.lang.ISeq [(Bottom)]))))

(deftest variances-test
  (is (= (fv-variances (make-F 'x))
         '{x :covariant}))
  (is (= (fv-variances (->Top))
         '{}))
  (is (= (fv-variances 
           (make-Function [] (RClass-of Atom [(make-F 'a) (make-F 'a)])))
         '{a :invariant}))
  (is (= (fv-variances 
           (make-Function [] (RClass-of Atom [-any (make-F 'a)])))
         '{a :covariant}))
  (is (= (fv-variances 
           (make-Function [] (RClass-of Atom [(make-F 'a) -any])))
         '{a :contravariant})))


(deftest fv-test
  (is (= (fv (make-F 'x))
         '#{x})))

(deftest fi-test
  (is (empty? (fi (make-F 'x)))))

(deftest cs-gen-test
  (is (= (cs-gen #{} ;V
                 (zipmap '[x y] (repeat no-bounds)) ;X
                 {} ;Y
                 (->Value 1) ;S
                 (make-F 'x)) ;T
         (->cset [(make-cset-entry {'x (->c (->Value 1) 'x (->Top) no-bounds)
                                    'y (->c (Un) 'y (->Top) no-bounds)})])))
  ;intersections correctly inferred
  (is (= (cs-gen '#{} {'x no-bounds} '{} 
                 (->HeterogeneousVector [(RClass-of Number)])
                 (In (RClass-of Seqable [(make-F 'x)]) (make-CountRange 1)))
         (->cset [(make-cset-entry {'x (->c (RClass-of Number) 'x -any no-bounds)})])))
;correct RClass ancestor inference
  (is (= (cs-gen #{} {'x no-bounds} {} 
                 (RClass-of IPersistentVector [(RClass-of Number)])
                 (RClass-of Seqable [(make-F 'x)]))
         (->cset [(make-cset-entry {'x (->c (RClass-of Number) 'x -any no-bounds)})]))))

(deftest subst-gen-test
  (let [cs (cs-gen #{} ;V
                   (zipmap '[x y] (repeat no-bounds)) ;X
                   {} ;Y
                   (->Value 1) ;S
                   (make-F 'x))]
    (is (= (subst-gen cs #{} (make-F 'x))
           {'x (->t-subst (->Value 1) no-bounds)
            'y (->t-subst (Un) no-bounds)}))))

(deftest infer-test
  (is (= (infer (zipmap '[x y] (repeat no-bounds)) ;tv env
                {}
                [(->Value 1) (->Value 2)] ;actual
                [(make-F 'x) (make-F 'y)] ;expected
                (make-F 'x)))) ;result
  (is (= (infer {'x no-bounds} ;tv env
                {}
                [(RClass-of IPersistentVector [(Un (-val 1) (-val 2) (-val 3))])] ;actual
                [(RClass-of Seqable [(make-F 'x)])] ;expected
                (RClass-of ASeq [(make-F 'x)])))) ;result
  (is (= (infer {'x no-bounds} ;tv env
                {}
                [(->HeterogeneousVector [(-val 1) (-val 2) (-val 3)])] ;actual
                [(RClass-of Seqable [(make-F 'x)])] ;expected
                (RClass-of ASeq [(make-F 'x)]))))) ;result

(deftest arith-test
  (is (subtype? (:t (tc-t (+)))
                (RClass-of Number)))
  (is (subtype? (:t (tc-t (+ 1 2)))
                (RClass-of Number)))
  (is (thrown? Exception (tc-t (+ 1 2 "a"))))
  (is (thrown? Exception (tc-t (-))))
  (is (thrown? Exception (tc-t (/)))))

(deftest tc-constructor-test
  (is (= (tc-t (Exception. "a"))
         (ret (RClass-of Exception)
              (-FS -top -bot)
              (->EmptyObject)))))

(deftest tc-throw-test
  (is (= (:t (tc-t (throw (Exception. "a"))))
         (Un))))

(deftest first-seq-test
  (is (subtype? (ret-t (tc-t (first [1 1 1])))
                (Un -nil (RClass-of Number))))
  (is (subtype? (In (RClass-of clojure.lang.PersistentList [-any])
                    (make-CountRange 1))
                (In (RClass-of Seqable [-any])
                    (make-CountRange 1))))
  (is (subtype? (ret-t (tc-t (let [l [1 2 3]]
                               (if (seq l)
                                 (first l)
                                 (throw (Exception. "Error"))))))
                (RClass-of Number)))
  (is (= (tc-t (first [1]))
         (ret (-val 1))))
  (is (= (tc-t (first []))
         (ret -nil)))
  (is (subtype? (ret-t (tc-t (first [1 2 3])))
                (RClass-of Number))))

(deftest intersection-maker-test
  (is (= (In -nil (-val 1))
         (Un)))
  (is (= (In (RClass-of Seqable [-any])
             -nil)
         (Un))))

(deftest count-subtype-test
  (is (subtype? (make-CountRange 1)
                (make-CountRange 1)))
  (is (not (subtype? (make-CountRange 1)
                     (make-ExactCountRange 1))))
  (is (subtype? (make-ExactCountRange 1)
                (make-CountRange 1)))
  (is (subtype? (make-ExactCountRange 4)
                (make-CountRange 1)))
  (is (subtype? (make-ExactCountRange 4)
                (make-CountRange 0)))
  (is (subtype? (make-CountRange 2)
                (make-CountRange 1)))
  )

(def-alias MyAlias
  (U nil (HMap {:a Number})))

(deftest names-expansion-test
  (is (subtype? (->Name 'clojure.core.typed.test.core/MyAlias) 
                (Un -nil (RClass-of Object)))))

(deftest ccfind-test
  (is (subtype? (-> (tc-t (clojure.core.typed/fn> [[a :- (clojure.lang.IPersistentMap Long String)]]
                                          (find a 1)))
                  :t :types first :rng :t)
                (Un (->HeterogeneousVector [(RClass-of Long) (RClass-of String)])
                    -nil))))

(deftest map-infer-test
  (is (subtype? (ret-t (tc-t (map + [1 2])))
                (RClass-of Seqable [(RClass-of Number)])))
  (is (subtype? (ret-t (tc-t (map + [1 2] [1 2] [4 5] [6 7] [4 4] [3 4])))
                (RClass-of Seqable [(RClass-of Number)])))
  (is (thrown? Exception (tc-t (map + [1 2] [1 2] [4 5] [6 7] [4 4] {3 4}))))
  (is (thrown? Exception (tc-t (map + [1 2] [1 2] [4 5] [6 7] [4 4] #{'a 4})))))

(deftest ann-form-test
  (is (= (ret-t (tc-t 
                  (clojure.core.typed/ann-form (atom 1) 
                                       (clojure.lang.Atom Number Number))))
         (parse-type '(clojure.lang.Atom Number Number)))))

(deftest atom-ops-test
  (is (subtype? (ret-t (tc-t
                         (reset!
                           (clojure.core.typed/ann-form (atom 1) 
                                                (clojure.lang.Atom Number Number))
                           10.1)))
                (RClass-of Number))))

(deftest apply-test
  ;conservative while not tracking keys "not" in a hmap
  (is (subtype? (ret-t (tc-t (apply merge [{:a 1}])))
                (RClass-of IPersistentMap [-any -any]))))

(deftest destructuring-test
  ;Vector destructuring with :as
  (is (= (ret-t (tc-t (let [[a b :as c] (clojure.core.typed/ann-form [1 2] (clojure.lang.Seqable Number))] 
                        [a b c])))
         (->HeterogeneousVector [(Un -nil (RClass-of Number))
                                 (Un -nil (RClass-of Number))
                                 (RClass-of Seqable [(RClass-of Number)])])))
  (is (= (ret-t (tc-t (let [[a b :as c] [1 2]] 
                        [a b c])))
         (-hvec [(-val 1)
                 (-val 2)
                 (-hvec [(-val 1) (-val 2)])])))
  ;Map destructuring of vector
  ;FIXME needs implementing, but gives a decent error msg
  #_(is (= (ret-t (tc-t (let [{a 0 b 1 :as c} [1 2]] 
                        [a b c])))
         (-hvec [(-val 1)
                 (-val 2)
                 (-hvec [(-val 1) (-val 2)])]))))

(deftest vararg-subtyping-test
  (is (subtype? (parse-type '[nil * -> nil])
                (parse-type '[nil -> nil])))
  (is (cf (clojure.core.typed/ann-form (clojure.core.typed/inst merge Any Any) [nil -> nil]))))

(deftest poly-filter-test
  (is (= (ret-t (tc-t (let [a (clojure.core.typed/ann-form [1] (clojure.lang.Seqable clojure.core.typed/AnyInteger))]
                        (if (seq a)
                          (first a)
                          'a))))
         (parse-type '(U clojure.core.typed/AnyInteger (Value a))))))

(deftest type-fn-test 
  (is (= (with-bounded-frees [[(make-F 'm) (tfn-bound (parse-type '(TFn [[x :variance :covariant]] Any)))]]
           (check-funapp
             (-> (ast 'a) ast-hy) ;dummy
             [(-> (ast 1) ast-hy)];dummy
             (ret (parse-type '(All [x]
                                    [x -> (m x)])))
           [(ret -nil)]
           nil))
         (ret (->TApp (make-F 'm) [-nil])))))

;TODO how to handle casts
#_(deftest prims-test
  (is (= (ret-t (tc-t (Math/sqrt 1)))
         (parse-type 'double))))

(deftest hmap-subtype
  (is (cf {} (clojure.lang.APersistentMap Any Any))))

;; `do` is special at the top level, tc-ignore should expand out to `do`
(tc-ignore
 (defprotocol some-proto (some-proto-method [_]))
 some-proto-method)

(deftest array-test
  (is (= (Class/forName "[I") 
         (class (into-array> int [1]))))
  (is (= (Class/forName "[Ljava.lang.Object;") 
         (class (into-array> (U nil int) [1]))))
  (is (= (Class/forName "[Ljava.lang.Number;") 
         (class (into-array> (U nil Number) [1]))))
  (is (= (Class/forName "[Ljava.lang.Object;") 
         (class (into-array> (U clojure.lang.Symbol Number) [1]))))
  (is (= (Class/forName "[Ljava.lang.Object;") 
         (class (into-array> Object (U clojure.lang.Symbol Number) [1]))))
  )

(deftest class-pathelem-test
  (is (= (-> (tc-t #(class %))
           ret-t :types first :rng Result-object*)
         (->Path [(->ClassPE)] 0)))
  (is (subtype? (-> (tc-t 
                      #(= Number (class %)))
                  ret-t)
                (->FnIntersection 
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
  (is (cf {:bar :b}
          '{:bar ':b}))
  ;correctly generalise
  (is (cf {(clojure.core.typed/ann-form :bar clojure.lang.Keyword) :b}
          (clojure.lang.IPersistentMap clojure.lang.Keyword ':b))))

(deftest isa-test
  (is (tc-t (isa? 1 1)))
  (is (tc-t #(isa? (class %) Number))))

(deftest array-primitive-hint-test
  (is (cf (let [^ints a (clojure.core.typed/into-array> int [(int 1)])]
            (alength a)))))

(deftest array-subtype-test
  (is (sub? (Array int) (Array int)))
  (is (sub? (Array int) (ReadOnlyArray int)))
  (is (not (sub? (ReadOnlyArray int) (Array int)))))

(deftest flow-assert-test
  (is (subtype?
        (-> (tc-t (fn [a]
                    {:pre [(integer? a)]}
                    a))
          ret-t)
        (parse-type '[Any -> clojure.core.typed/AnyInteger])))
  (is (subtype? 
        (-> (tc-t (let [a (read-string "1")
                        _ (assert (integer? a))]
                    (+ 10 a)))
          ret-t)
        (parse-type 'clojure.core.typed/AnyInteger)))
  ;postconditions
  (is (subtype?
        (-> (tc-t (fn [a]
                    {:post [(vector? a)]}
                    a))
          ret-t)
        (parse-type '[Any -> (clojure.lang.IPersistentVector Any)]))))

(deftest complete-hmap-test
  (is (subtype? (-complete-hmap {})
                (parse-type '(clojure.lang.APersistentMap Nothing Nothing))))
  (is (not
        (subtype? (-hmap {})
                  (parse-type '(clojure.lang.APersistentMap Nothing Nothing)))))
  (is (subtype? (-> (tc-t {}) ret-t)
                (parse-type '(clojure.lang.APersistentMap Nothing Nothing)))))

(deftest dotted-on-left-test
  (is (cf (memoize (fn [])))))

(deftest string-as-seqable-test
  (is (subtype? 
        (RClass-of String)
        (RClass-of Seqable [-any])))
  (is (subtype? 
        (-val "a")
        (RClass-of Seqable [-any])))
  (is (cf (seq "a")))
  (is (cf (first "a") Character))
  (is (cf (first (clojure.core.typed/ann-form "a" String)) (clojure.core.typed/Option Character))))

(deftest recursive-cf-test
  (is (cf (cf 1 Number)
          Any)))
