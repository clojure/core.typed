(in-ns 'typed.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type annotations

(def-alias AnyInteger (U Integer Long clojure.lang.BigInt BigInteger Short Byte))
(def-alias Atom1 (TFn [[x :variance :invariant]] (Atom x x)))
(def-alias Option (TFn [[x :variance :covariant]] (U nil x)))

(ann clojure.core/*ns* Namespace)
(ann clojure.core/namespace [(U Symbol String Keyword) -> (Option String)])
(ann clojure.core/ns-name [Namespace -> Symbol])
(ann clojure.core/name [(U String Named) -> String])
(ann clojure.core/in-ns [Symbol -> nil])
(ann clojure.core/import [Any * -> nil])
(ann clojure.core/identity (All [x] [x -> x
                                     :filters {:then (! (U nil false) 0)
                                               :else (is (U nil false) 0)}
                                     :object {:id 0}]))
(ann clojure.core/gensym (Fn [-> Symbol]
                             [String -> Symbol]))

;TODO flip filters
(ann clojure.core/complement (All [x] [[x -> Any] -> [x -> boolean]]))
; should preserve filters
(ann clojure.core/boolean [Any -> boolean])

(ann clojure.core/filter (All [x y]
                           [[x -> Any :filters {:then (is y 0)}] (Option (Seqable x)) -> (Seqable y)]))
(ann clojure.core/remove (All [x y]
                           [[x -> Any :filters {:else (is y 0)}] (Option (Seqable x)) -> (Seqable y)]))


(ann clojure.core/take-while (All [x y]
                               [[x -> Any :filters {:then (is y 0)}] (Option (Seqable x)) -> (Seqable y)]))
(ann clojure.core/drop-while (All [x]
                               [[x -> Any] (Option (Seqable x)) -> (Seqable x)]))

(ann clojure.core/split-with 
     (All [x y z] [[x -> Any :filters {:then (is y 0), :else (is z 0)}] (tc/Option (Seqable x)) 
                   -> '[(Seqable y) (Seqable z)]]))


(ann clojure.core/repeatedly 
     (All [x]
          (Fn [[-> x] -> (LazySeq x)]
              [[-> x] tc/AnyInteger -> (LazySeq x)])))


(ann clojure.core/some (All [x y] [[x -> y] (Option (Seqable x)) -> (Option y)]))

(ann clojure.core/concat (All [x] [(Option (Seqable x)) * -> (Seqable x)]))

(ann clojure.core/set (All [x] [(Option (Seqable x)) -> (PersistentHashSet x)]))
(ann clojure.core/list (All [x] [x * -> (PersistentList x)]))
(ann clojure.core/vector (All [x] [x * -> (APersistentVector x)]))

(ann clojure.core/not [Any -> boolean])
(ann clojure.core/constantly (All [x y] [x -> [y * -> x]]))

(ann clojure.core/disj
     (All [x]
          (Fn [(I (APersistentSet x) Sorted) Any Any * -> (I (APersistentSet x) Sorted)]
              [(APersistentSet x) Any Any * -> (APersistentSet x)]
              [(I (APersistentSet x) Sorted) Any Any * -> (I (IPersistentSet x) Sorted)]
              [(IPersistentSet x) Any Any * -> (IPersistentSet x)])))

(ann clojure.core/assoc
     (All [b c d]
       (Fn [(IPersistentMap b c) b c -> (IPersistentMap b c)]
           [(IPersistentVector d) AnyInteger d -> (IPersistentVector d)])))

(ann clojure.core/zipmap
     (All [k v]
       [(U nil (Seqable k)) (U nil (Seqable v)) -> (APersistentMap k v)]))

(comment
  (aget my-array 0 1 2)
  (aget (aget my-array 0) 1 2)
  (aget (aget (aget my-array 0) 1) 2)

  (App [(Associative a b) c d -> (Associative (U a c) (U b d))]
       (App [(Associative a b) c d -> (Associative (U a c) (U b d))]
            (App [(Associative a b) c d -> (Associative (U a c) (U b d))]
                 (Associative Keyword Number)
                 :a 1)
            :b 2)
       :c 3)

  (assoc my-map :a 1 :b 2 :c 3)
  (assoc (assoc my-map :a 1) :b 2 :c 3)
  (assoc (assoc (assoc my-map :a 1) :b 2) :c 3)

  (ann clojure.core/aset
       (Label [rec]
              (All [w [v :< w] :dotted [b]]
                   [(Array w _) AnyInteger v -> v]
                   [(Array _ r) AnyInteger b ... b
                    :recur (rec r b ... b)])))

  (ann clojure.core/aget 
       (Label [rec]
              (All [x :dotted [b]] 
                   (Fn [(Array _ x) AnyInteger -> x]
                       [(Array _ x) AnyInteger b ... b
                        :recur 
                        (rec x b ... b)]))))

  (ann clojure.core/assoc 
       (Label [rec]
              (All [[h :< (HMap {})] x y [k :< (I AnyValue Keyword)] [e :< k] :dotted [b]]
                   [h k v -> (I h (HMap k v))]
                   [(Associative y x) y x -> (Associative y x)]
                   [h k v b ... b
                    :recur (rec (I h (HMap {k v})) b ... b)]
                   [(Associative y x) y x b ... b
                    :recur (rec (Associative y x) b ... b)]
                   )))

  (ann clojure.core/dissoc
       (Label [rec]
              (All [[m :< (Associative _ _)] :dotted [b]]
                   [nil Any * -> nil]
                   [m -> m]
                   [m k b ... b
                    :recur
                    (rec (I m (HMap {} :without [k])) b ... b)])))

  (update-in {:a {:b 1}} [:a :b] inc)
  (update-in 
    (update-in {:a {:b 1}} [:a] inc) 
    [:b] 
    inc)

  (ann clojure.core/update-in
       (FixedPoint
         (All [[x :< (U nil (Associative Any Any))] k [l :< k] v r e
               :dotted [a b]]
              (Fn [(HMap {l v}) (Vector* k) [v a ... a -> r] a ... a -> (I x (HMap {l r}))]
                  [(HMap {l r}) (Vector* k b ... b) [v a ... a -> e] a ... a
                   :recur
                   [r (Vector* b ... b) [v a ... a -> e] a ... a]]))))

  ;(ann clojure.core/get-in 
  ;     (Label [rec]
  ;       (All [[x :< (U nil (Associative Any Any))] k :dotted [b]]
  ;            (Fn [x (Vector*) -> x]
  ;                [x (Vector*) _ -> x]
  ;                [(U nil (Associative _ y) (Vector* k b ... b) a -> x
  ;                ;TODO
  ;                [(U nil (Associative Any y)) (Vector* k) -> (U nil x)]
  ;                    )))))

  (ann clojure.core/partial 
       (Label [rec]
              (All [x [a :< x] r :dotted [b c]]
                   (Fn [[x c ... c -> r] a -> [c ... c -> r]]
                       [[x c ... c -> r] a b ... b
                        :recur
                        (rec [c ... c -> r] b ... b)]))))

  ;                                [[y -> x] [b ... b -> y] -> [b ... b -> x]]
  ;                                [[y -> x] [z -> y] [b ... b -> z] -> [b ... b -> x]]
  ;                                [[y -> x] [z -> y] [k -> z] [b ... b -> k] -> [b ... b -> x]]
  ;                                [[y -> x] [z -> y] [k -> z] [l -> k] [b ... b -> l] -> [b ... b -> x]]
  ;                                [[y -> x] [z -> y] [k -> z] [l -> k] [m -> l] [b ... b -> m] -> [b ... b -> x]]

  (ann clojure.core/juxt
                  (All [y b ... c ...]
                       [[b ... b -> y] [b ... b -> c] ... c -> [b ... b -> (DottedVec y c ... c)]]))
  )

;most useful case
(ann clojure.core/comp
     (All [x y b ...]
          [[x -> y] [b ... b -> x] -> [b ... b -> y]]))

(ann clojure.core/partial 
     (All [y a b c d e f g h i j k l m n o p z ...]
          (Fn [[a z ... z -> y] a -> [z ... z -> y]]
              [[a b z ... z -> y] a b -> [z ... z -> y]]
              [[a b c z ... z -> y] a b c -> [z ... z -> y]]
              [[a b c d z ... z -> y] a b c d -> [z ... z -> y]]
              [[a b c d e z ... z -> y] a b c d e -> [z ... z -> y]]
              [[a b c d e f z ... z -> y] a b c d e f -> [z ... z -> y]]
              [[a b c d e f g z ... z -> y] a b c d e f g -> [z ... z -> y]]
              [[a b c d e f g h z ... z -> y] a b c d e f g h -> [z ... z -> y]]
              [[a b c d e f g h i z ... z -> y] a b c d e f g h i -> [z ... z -> y]]
              [[a b c d e f g h i j z ... z -> y] a b c d e f g h i j -> [z ... z -> y]]
              [[a b c d e f g h i j k z ... z -> y] a b c d e f g h i j k -> [z ... z -> y]]
              [[a b c d e f g h i j k l z ... z -> y] a b c d e f g h i j k l -> [z ... z -> y]]
              [[a b c d e f g h i j k l m z ... z -> y] a b c d e f g h i j k l m -> [z ... z -> y]]
              [[a b c d e f g h i j k l m n z ... z -> y] a b c d e f g h i j k l m n -> [z ... z -> y]]
              [[a b c d e f g h i j k l m n o z ... z -> y] a b c d e f g h i j k l m n o -> [z ... z -> y]]
              [[a b c d e f g h i j k l m n o p z ... z -> y] a b c d e f g h i j k l m n o p -> [z ... z -> y]])))

(ann clojure.core/str [Any * -> String])
(ann clojure.core/prn-str [Any * -> String])
(ann clojure.core/pr-str [Any * -> String])

(ann clojure.core/print [Any * -> Any])
(ann clojure.core/println [Any * -> Any])
(ann clojure.core/pr [Any * -> Any])
(ann clojure.core/prn [Any * -> Any])


(ann clojure.core/atom (All [x] [x -> (Atom x x)]))

(comment
  (ann clojure.core/atom (All [x] [x & {(U nil (IPersistentMap Any Any)) :meta 
                                        (U nil [x -> Any]) :validator}
                                   -> (Atom x x)]))
  )

(ann clojure.core/deref (All [x y]
                             (Fn [(IDeref x) -> x]
                                 [(IDeref x) AnyInteger y -> (U x y)])))
(ann clojure.core/reset! (All [w r]
                              [(Atom w r) w -> r]))

(ann clojure.core/swap! (All [w r b ...] 
                             [(Atom w r) [r b ... b -> w] b ... b -> w]))

(ann clojure.core/symbol
     (Fn [(U Symbol String) -> Symbol]
         [String String -> Symbol]))

(ann clojure.core/seq? (predicate (ISeq Any)))
(ann clojure.core/set? (predicate (IPersistentSet Any)))
(ann clojure.core/vector? (predicate (IPersistentVector Any)))
(ann clojure.core/nil? (predicate nil))

(ann clojure.core/meta (All [x]
                            (Fn [(IMeta x) -> x]
                                [Any -> nil])))
(ann clojure.core/with-meta (All [[x :< clojure.lang.IObj] y]
                              [x y -> (I x (IMeta y))]))

(ann clojure.core/string? (predicate String))

(ann clojure.core/class [Any -> (Option Class) :object {:id 0 :path [Class]}])

(ann clojure.core/seq (All [x]
                        [(Option (Seqable x)) -> (Option (I (ISeq x) (CountRange 1)))
                         :filters {:then (is (CountRange 1) 0)
                                   :else (| (is nil 0)
                                            (is (ExactCount 0) 0))}]))

(ann clojure.core/empty? [(Option (Seqable Any)) -> boolean
                          :filters {:then (| (is (ExactCount 0) 0)
                                             (is nil 0))
                                    :else (is (CountRange 1) 0)}])

(ann clojure.core/map
     (All [c a b ...]
          [[a b ... b -> c] (U nil (Seqable a)) (U nil (Seqable b)) ... b -> (LazySeq c)]))

(ann clojure.core/mapcat
     (All [c b ...]
          [[b ... b -> (Option (Seqable c))] (Option (Seqable b)) ... b -> (LazySeq c)]))

(ann clojure.core/merge-with
     (All [k v]
          (Fn [[v v -> v] nil * -> nil]
              [[v v -> v] (IPersistentMap k v) * -> (IPersistentMap k v)]
              [[v v -> v] (Option (IPersistentMap k v)) * -> (Option (IPersistentMap k v))])))

(ann clojure.core/reduce
     (All [a c]
          (Fn 
            ;Without accumulator
            ; default
            ; (reduce + my-coll)
            [[a c -> a] (I (Seqable c) (CountRange 1)) -> a]
            [(Fn [a c -> a] [-> a]) (Option (Seqable c)) -> a]
            ; default
            ; (reduce + 3 my-coll)
            [[a c -> a] a (Option (Seqable c)) -> a])))

(comment
(ann clojure.core/reduce
     (All [a c d]
          (Fn 
            ;Without accumulator
            ; empty coll, f takes no args
            ; (reduce + []) => 0, (reduce + nil) => 0
            [[-> c] (U nil (I (ExactCount 0) (Seqable c))) -> c]
            ; coll count = 1, f is not called
            ; (reduce + [1]) => 1
            [Any (I (ExactCount 1) (Seqable c)) -> c]
            ; coll count >= 2
            ; (reduce + [1 2]) => 3
            [[c c -> c] (I (CountRange 2) (Seqable c)) -> c]
            ; default
            ; (reduce + my-coll)
            [(Fn [c c -> c] [-> c]) (U nil (Seqable c)) -> c]
            ;With accumulator
            ; empty coll, f not called, returns accumulator
            ; (reduce + 3 []) => 3
            [Any a (U nil (I (ExactCount 0) (Seqable Any))) -> a]
            ; default
            ; (reduce + 3 my-coll)
            [[a c -> a] a (U nil (Seqable c)) -> a])))
  )

(ann clojure.core/first
     (All [x]
          (Fn [(Option (I (Seqable x) (ExactCount 0))) -> nil]
              [(I (Seqable x) (CountRange 1)) -> x]
              [(Option (Seqable x)) -> (Option x)])))

(ann clojure.core/second
     (All [x]
          (Fn [(Option (I (Seqable x) (CountRange 0 1))) -> nil]
              [(I (Seqable x) (CountRange 2)) -> x]
              [(Option (Seqable x)) -> (Option x)])))

(ann clojure.core/rest
     (All [x]
          [(Option (Seqable x)) -> (ISeq x)]))

(ann clojure.core/next
     (All [x]
          [(Option (Seqable x)) -> (Option (I (ISeq x) (CountRange 1)))
           :filters {:then (& (is (CountRange 2) 0)
                              (! nil 0))
                     :else (| (is (CountRange 0 1) 0)
                              (is nil 0))}]))

(ann clojure.core/conj
     (All [x y]
          (Fn [(IPersistentVector x) x x * -> (IPersistentVector x)]
              [(APersistentMap x y)
               (U nil (IMapEntry x y) (Vector* x y))
               (U nil (IMapEntry x y) (Vector* x y)) * -> (APersistentMap x y)]
              [(IPersistentMap x y)
               (U nil (IMapEntry x y) (Vector* x y))
               (U nil (IMapEntry x y) (Vector* x y)) * -> (IPersistentMap x y)]
              [(IPersistentSet x) x x * -> (IPersistentSet x)]
              [(ISeq x) x x * -> (ASeq x)]
              [(IPersistentCollection Any) Any Any * -> (IPersistentCollection Any)])))

(ann clojure.core/find
     (All [x y]
          [(IPersistentMap x y) Any -> (Option (Vector* x y))]))

(ann clojure.core/get
     (All [x]
          (Fn 
            [(IPersistentSet x) Any -> (Option x)]
            [java.util.Map Any -> (Option Any)]
            [String Any -> (Option Character)]
            [nil Any -> nil]
            [(Option (ILookup Any x)) Any -> (Option x)])))

(ann clojure.core/merge 
     (All [k v]
          (Fn [nil * -> nil]
              [(IPersistentMap k v) * -> (IPersistentMap k v)]
              [(Option (IPersistentMap k v)) * -> (Option (IPersistentMap k v))])))

(ann clojure.core/= [Any Any * -> (U true false)])


(ann clojure.core/integer? (predicate AnyInteger))
(ann clojure.core/number? (predicate Number))

(ann clojure.core/+ (Fn [AnyInteger * -> AnyInteger]
                        [Number * -> Number]))
(ann clojure.core/- (Fn [AnyInteger AnyInteger * -> AnyInteger]
                        [Number Number * -> Number]))
(ann clojure.core/* (Fn [AnyInteger * -> AnyInteger]
                        [Number * -> Number]))
(ann clojure.core// [Number Number * -> Number])

(ann clojure.core/inc (Fn [AnyInteger -> AnyInteger]
                          [Number -> Number]))
(ann clojure.core/dec (Fn [AnyInteger -> AnyInteger]
                          [Number -> Number]))

(ann clojure.core/even? [AnyInteger -> boolean])
(ann clojure.core/odd? [AnyInteger -> boolean])

(ann clojure.core/take
     (All [x]
       [AnyInteger (Seqable x) -> (LazySeq x)]))

(ann clojure.core/cons
     (All [x]
       [x (Option (Seqable x)) -> (ASeq x)]))

(ann clojure.core/reverse
     (All [x]
       [(Option (Seqable x)) -> (Seqable x)]))

;casts
(ann clojure.core/bigdec [Any -> BigDecimal])
(ann clojure.core/bigint [Any -> clojure.lang.BigInt])
(ann clojure.core/boolean [Any -> boolean])
(ann clojure.core/byte [Any -> byte])
(ann clojure.core/char [Any -> char])
(ann clojure.core/double [Any -> double])
(ann clojure.core/float [Any -> float])
(ann clojure.core/int [Any -> int])
(ann clojure.core/long [Any -> long])
(ann clojure.core/num [Any -> Number])
(ann clojure.core/short [Any -> short])

(override-method clojure.lang.RT/get (All [y] (Fn [(IPersistentMap Any y) Any -> (Option y)])))

(override-method clojure.lang.Numbers/add (Fn [AnyInteger AnyInteger -> AnyInteger]
                                              [Number Number -> Number]))
(override-method clojure.lang.Numbers/inc (Fn [AnyInteger -> AnyInteger]
                                              [Number -> Number]))
(override-method clojure.lang.Numbers/dec (Fn [AnyInteger -> AnyInteger]
                                              [Number -> Number]))
(override-method clojure.lang.Numbers/minus (Fn [AnyInteger AnyInteger -> AnyInteger]
                                                [Number Number -> Number]))
(override-method clojure.lang.Numbers/multiply (Fn [AnyInteger AnyInteger -> AnyInteger]
                                                   [Number Number -> Number]))
(override-method clojure.lang.Numbers/divide [Number Number -> Number])

(override-method clojure.lang.Numbers/lt [Number Number -> boolean])
(override-method clojure.lang.Numbers/lte [Number Number -> boolean])
(override-method clojure.lang.Numbers/gt [Number Number -> boolean])
(override-method clojure.lang.Numbers/gte [Number Number -> boolean])

(override-constructor clojure.lang.LazySeq 
                      (All [x]
                        [[-> (Option (Seqable x))] -> (LazySeq x)]))

(let [t (make-FnIntersection
          (make-Function 
            [(Un -nil (RClass-of Seqable [-any]))]
            (parse-type 'AnyInteger)
            nil nil
            :object (->Path [(->CountPE)] 0)))]
  (add-var-type 'clojure.core/count t)
  (add-method-override 'clojure.lang.RT/count t))

(non-nil-return java.lang.Object/getClass #{0})
(override-method clojure.lang.RT/nth
                 (All [x y]
                   (Fn [(Seqable x) AnyInteger -> x]
                       [(Seqable x) AnyInteger y -> (U x y)])))
