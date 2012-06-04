
;; Some sigs

(+T clojure.core/seq
    (All [[x :variance :invariant]]
      (Fn [(Inst Seqable x) -> x]
          [CharSequence -> (U StringSeq Nil)]
          ;array -> (U ArraySeq Nil)
          [Nil -> Nil]
          [(U Map Iterable) -> (U IteratorSeq Nil)])))

(+T clojure.core/conj
    (All [[x :variance :invariant]   ;object to cons
          [c :variance :invariant]]  ;cons result
      (Fn [(Inst IPersistentCollection x c _ _ _) x & x * -> c]
          [Nil & x * -> (Inst PersistentList x)])))

(+T clojure.core/into
    (All [[o :variance :invariant] ;first arg
          [tx :variance :invariant]
          [tc :variance :invariant]
          [t :variance :invariant] ;transient
          [pt :variance :invariant]
          [x :variance :invariant]   ;object to cons
          [c :variance :invariant]]  ;cons result
      (Fn [(Inst IEditableCollection t)
           (Inst ConstantCollection tx ... tx) ->
           pt :in (Inst ITransientCollection t tx _)
           :init t
           :next tx
           :for tx
           :finally (Inst ITransientCollection _ _ pt)]
          [(Inst IPersistentCollection x c _ _ _) -> c]
          [Nil & x * -> (Inst PersistentList x)])))

(+T clojure.core/first
    (All [[f :variance :invariant]]
      (Fn [(Inst IPersistentCollection _ _ _ f _ _) -> f]
          [(Inst Seqable (Inst IPersistentCollection _ _ _ f _ _)) -> f]
          [CharSequence -> (U StringSeq Nil)]
          ;array -> (U Any Nil)
          [Iterable -> (U IteratorSeq Nil)]
          [Nil -> Nil])))

(+T clojure.core/rest
    (All [[r :variance :invariant]]  ;rest
      (Fn [(Inst IPersistentCollection _ _ _ _ r _) -> r]
          [(Inst Seqable (Inst IPersistentCollection _ _ _ _ r _)) -> r]
          [CharSequence -> (U StringSeq PersistentList$EmptyList)]
          [Nil -> PersistentList$EmptyList]
          ;array -> (U ArraySeq PersistentList$EmptyList)
          [(U Map Iterable) -> (U IteratorSeq PersistentList$EmptyList)])))

(+T clojure.core/next
    (All [[n :variance :invariant]]  ;rest
      (Fn [(Inst IPersistentCollection _ _ _ _ _ n) -> n]
          [(Inst Seqable (Inst IPersistentCollection _ _ _ _ _ n)) -> n]
          [CharSequence -> (U StringSeq Nil)]
          [Nil -> Nil]
          ;array -> (U ArraySeq Nil)
          [(U Map Iterable) -> (U IteratorSeq Nil)])))

(+T clojure.core/assoc
    (All [[a :variance :invariant] ;invariant
          [b :variance :invariant]
          [c :variance :invariant]]
         (Fn [(Inst Associative (Fn [a b -> c])) a b & [a b] * -> c] ;TODO "keyword" rest args
             [Nil a b & [a b] * -> (U (Inst PersistentArrayMap a b)
                                      (Inst PersistentHashMap a b))] ;sufficient return type? seems hacky - Ambrose
             )))

(defrecord A [])


(dissoc {:a :b, :c :d, :e :f}
        :a   ;{:c :d, :e :f}
        :c   ;{:e :f}
        :e   ;{}
        :a   ;{}
        )

(+T clojure.core/dissoc
    (All [[a :< (Inst IPersistentMap _)
           :variance :invariant]
          [b :variance :invariant]
          [c :pre-type true]
          [d :recursive-placeholder true]]
         (Fn [a & c ... c -> d :in (IPersistentCollection [c -> d]) ... c :next d]
             [Nil & Any * -> Nil])))

(+T clojure.core/merge
    (All [[a :< (Inst IPersistentCollection _)]]
         (Fn [& m ... m -> r])))

(+T clojure.core/count
    (All [[a :variance :invariant]]
         (Fn [(Inst Counted a) -> a]
             [(Inst Seqable a) -> Integer])))

[1 2] :- (I (Counted (Value 2))
            (IPersistentVector [1 2]))

(+T clojure.core/nth
    (All [[a :< (Inst Value _)
           :variance :invariant]
          [b :< (Inst Value _)
           :variance :invariant]]
         (Fn [(Inst Counted a) (Inst Value b) -> (assert (< a b))])))

(conj {:a :b} (could-return-nil) (if (pred 1)
                                   [:a :c]
                                   [:b :c]))

[& b ... b]
(C a b) ... a :recursive-point b

(C [a0 ->
   (C [a1 ->
      (C [a2 ->
          ...
          (C [aN ->
              r])])])])

(Inst IPersistentMap a 
      (Inst IPersistentMap b (Inst IPersistentMap )))

(map + [1 2] [1])

(+T clojure.core/map
    (All [[a :variance :invariant]
          [b :pre-type true]
          [c :variance :invariant]]
         (Fn [(Fn [a & b ... b -> c])
              (Inst Seqable (Inst ISeq a _ _))
              & (Inst Seqable (Inst ISeq b _ _))
              ... b
              -> (LazySeq c)])))

(+T clojure.core/nth
    (All [[a :variance :invariant]
          [b :pre-type true]
          [c :variance :invariant]]
         (I (Fn [(Inst Indexed a b) a -> b]
                [Nil Any -> Nil]
                [CharSequence Integer -> (U Nil Character)]
                [(U Matcher RandomAccess) Integer -> Any]
                [Map.Entry Integer -> Any]
                [Sequential Integer -> Any])
            (Fn [(Inst Indexed a b) a c -> (U b c)]
                [Nil Any c -> c]
                [CharSequence Integer c -> (U c Character)]
                [(U Matcher RandomAccess) Integer c -> (U Any c)]
                [Map.Entry Integer c -> (U Any c)]
                [Sequential Integer -> Any]))))

;(U Integer (Value 1)) <: (Inst Value _)

(get {:a 1} :a) => 1
(get {:a 1} :b) => nil

(+T clojure.core/get
    (All [[vk :< (Inst Value _)
           :variance :invariant]
          [c :variance :invariant]
          [d :variance :invariant]
          [e :variance :invariant]]
         (Fn [(ILookup (Fn [vk -> d])) vk -> d]  ;try matching on Values
             [(ILookup (Fn [c -> d])) e -> (U d Nil)] ;fall back on Classes
             [Map Any -> Any]
             [(IPersistentSet vk) vk -> vk] ;try matching on Values
             [(IPersistentSet vk) vk -> Nil] ;try matching on Values
             [(IPersistentSet c) e -> (U vk Nil)] ;fall back on Classes
             )))

; just playing with syntax 
(comment

  ;(+T-pprotocol ProtocolName tvars & methods)
  (+T-pprotocol ISeq [[a :variance :invariant]]
    (+T first (Fn [(Inst IPersistentCollection Nothing)
                   -> Nil]
                  [(Inst IPersistentCollection a)
                   -> (U (ASeq a) Nil)]
                  [String -> (U Nil (ASeq Character))]
                  [(U Iterable Map Seqable) -> (U Nil (ASeq Any))])))

  ;(+T-ptype TypeName tvars fields & impls)

  (+T-ptype VectorNode [[x :variance :invariant]] 
    [[arr (Inst Array x)]])

  (+T-ptype PersistentVector [[x :variance :covariant]] 
    [[cnt :- Long]
     [root :- (Inst VectorNode Any)]
     [tail :- (Inst ListNode x)]]
                )
  )
