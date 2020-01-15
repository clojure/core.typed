(ns clojure.core.typed.test.conj-experiment
  (:refer-clojure :exclude [conj into with-meta])
  (:require [clojure.core.typed :as t])
  (:import (clojure.lang IMapEntry)))

(comment
; # ICollection
;  
; This interface defines the extension point for `conj`, the polymorphic conjoin function.
;
; conj's contract says it always returns another ICollection.
;
; conj is interesting from a typing perspective because it is polymorphic in both its
; argument and return type.
;
; conjing a vector can accept anything as arguments
;
;   (conj [] 1 nil)
;   ;=> [1 nil]
;
; Maps are more restrictive; only an `(IMapEntry a b)` and (by convention) `nil`
; can be passed to conj. Only the former case actually changes the result.
;
; ` (conj {} [1 2] nil)
;   ;=> {1 2}
;
; This polymorphic behaviour over parameters is crucial when considering code like
;
;   (defn conj-empty 
;     "Takes a collection and conj's `arg` onto the result of `(empty coll)`"
;     [coll arg]
;     (conj (empty coll) a))
;
; Here we need to check: for all collections `coll`, `arg` is legal
; to conj onto the empty type of `coll`.
;
; The annotation for conj-empty is
;
;   (ann conj-empty 
;     (All [max-arg
;           [a :< max-arg]
;           [res :variance :covariant
;            :< (Rec [m]
;                 (TFn [[arg :variance :covariant]] 
;                   (Rec [c]
;                     (IColl max-arg m c))))]]
;       [(ICollection max-arg res) a -> (res a)]))
;
; # Example instantiations
;
; This is hard to grok at a glance; let's see some common instantiations.
;
; This is a valid instantiation if we pass a Vector and a Number to conj-empty:
;
;   [(ICollection Any Vector) Number -> (Vector Number)]
;
; or a List and a Long:
;
;   [(ICollection Any List) Long -> (List Long)]
;
; The interesting case of passing a `(Map Boolean Boolean)` and an `(IMapEntry Number Number)`:
;
;   [(ICollection (U nil (IMapEntry Any Any))
;                 (TFn [[x :variance :covariant
;                       :< (U nil (IMapEntry Any Any))]]
;                   (Map (U Boolean Number) (U Boolean Number))))
;    (U nil (IMapEntry Number Number))
;    -> (Map (U Boolean Number) (U Boolean Number))]
;
; # ICollection's type parameters
;
; Now we will step through each of ICollection's type parameters.
;
; ## max-arg
;
; The first parameter `max-arg` captures the intuition that there is maximum type that can
; be conjed onto a particular ICollection. For Vectors and Lists `max-arg` is `Any`, because
; they accept any parameter to `conj`.
;
; For Maps, `max-arg` is `(U nil (IMapEntry Any Any))`: only the types `nil` and `(IMapEntry Any Any)`
; can be legally conjed onto a map.
;
; `max-arg` is used as an upper bound. For example, here is the type for the -conj method:
;
;    -conj
;    (All [[a :< max-arg]] 
;      [(IColl max-arg res) a a * -> (res a)])
;
; `a` is restricted to be smaller than `max-arg`, which guarantees any instantiation of `-conj`
; will only accept type correct arguments.
;
; Review the instantiations above and confirm that the instantiations for `max-arg` make sense
; for each conj target type.
;
; ## res
;
; `res` generates the result type of a conj operation. 
;
; `res` is much like conj at the value level, except one level "up". 
;
; It is a type function that accepts a parameter (a type) to conj and returns a type that
; represents the result of the `conj` operation.
;
; For conjing a `(Vector t)`, `res` is simply `Vector`, or equivalently `(TFn [[x :variance :covariant]] (Vector x))`.
; 
; As you can see from ICollection's ann-protocol below, `res` has an upper bound: 
;
;  [res :variance :covariant
;   :< (Rec [m]
;        (TFn [[arg :variance :covariant
;               :< max-arg]] 
;          (ICollection max-arg m)))]
;
; The upper bound constrains `res` to be a type function that takes a `conj` argument (bounded
; by `max-arg`) and returns an ICollection.

(t/ann-protocol [[max-arg :variance :covariant]
                 [res :variance :covariant
                  :> (TFn [[arg :variance :covariant
                            :< max-arg]] 
                       Nothing)
                  :< (Rec [m]
                       (TFn [[arg :variance :covariant
                              :< max-arg]] 
                         (IColl max-arg m)))
                  ]]
                IColl
                -conj 
                (All [[a :< max-arg]]
                  [(IColl max-arg res) a a * -> (res a)])
                )

; # IEmptyableCollection
;
; This is part of IPersistentCollection in Clojure.
;
; Simply returns an empty collection of the same type as the target
; collection. -empty just returns an IColl.

(t/ann-protocol [[emp :variance :covariant
                  :> Nothing
                  :< (Rec [m]
                       (IColl Any 
                              (TFn [[arg :variance :covariant]]
                                m)))]]
                IEmptyableCollection
                -empty 
                [(IEmptyableCollection emp) -> emp]
                )

; # IMeta
;
; Reading mutable or immutable metadata.
;
; -meta returns the current metadata of the target.

(t/ann-protocol [[mmap :variance :covariant
                  :< (U nil (t/Map Any Any))]]
                IMeta

                -meta
                [(IMeta mmap) -> mmap]
                )

(t/defprotocol IMeta
  (-meta [this]))

; # IWithMeta
;
; IObj in Clojure.
; 
; Writing immutable metadata.
(t/ann-protocol [[metafn :variance :covariant
                  :> (TFn [[new-meta :variance :covariant
                            :< (U nil (t/Map Any Any))]]
                          Nothing)
                  :< (Rec [m]
                       (TFn [[new-meta :variance :covariant
                              :< (U nil (t/Map Any Any))]]
                         (I (IWithMeta m)
                            ; seems useful to want to also read metadata abstractly
                            (IMeta new-meta))))]]

                IWithMeta
                -with-meta
                (All [[mmap :< (U nil (t/Map Any Any))]]
                  [(IWithMeta metafn) mmap -> (metafn mmap)])
                )

(t/defprotocol IWithMeta
  (-with-meta [this m]))

(t/ann-datatype [[m :variance :covariant
                  :< (U nil (t/Map Any Any))]]
                Symbol
                [name :- String
                 meta-map :- m]
                :extends
                [(IMeta m)
                 (IWithMeta Symbol)])
(deftype Symbol [name meta-map]
  IWithMeta
  ; (All [mmap]
  ;   [(IWithMeta metafn) mmap -> (metafn mmap)])
  (-with-meta [this m]
    (Symbol. name m))


  IMeta
  (-meta [this] meta-map))

(t/ann with-meta 
       (All [[metafn
              :> (TFn [[new-meta :variance :covariant
                        :< (U nil (t/Map Any Any))]]
                      Nothing)
              :< (Rec [m]
                      (TFn [[new-meta :variance :covariant
                             :< (U nil (t/Map Any Any))]]
                           (I (IWithMeta m)
                              (IMeta new-meta))))]
             [m :< (U nil (t/Map Any Any))]]
          [(IWithMeta metafn) m -> (metafn m)]))
(defn with-meta [a m]
  ((t/inst -with-meta metafn m) a m))

; # ISeqable
;
; Immutable representation of the target collection.
;
; -seq returns the nonempty persistent collection or nil. Its
; return type allows the target collection full flexibility for
; the type of collection it returns.
(t/ann-protocol [[s :variance :covariant
                  :< (U nil (t/NonEmptySeq Any))]]
                ISeqable

                -seq
                [(ISeqable s) -> s]
                )

; update metadata
; update map entries
; update seqable length
; update 

; Who should take the brunt of the work for updating types?
;
; We could rely on predicates like every? to generate better types.
; eg. (every? number? myvec) could infer (is (Vec Number) myvec) rather than
;     (is (Seqable Number) myvec). This would be simple to update myvec with.
;
; Or we could make the intersection simplification/combination process
; smarter or more extensible.
;
; eg. combining (is (Vec Any) myvec) and (is (Seqable Number) myvec) somehow
;     infers (is (Vec Number) myvec)

; # ISeq
(t/ann-protocol [[s :variance :covariant]
                 [l :variance :covariant
                  :< (CountRange 0)]]
                ISeq
                -first
                (Fn [(ISeq s (CountRange 1)) -> s]
                    [(ISeq s l) -> (U nil s)])
                -rest
                [(ISeq s l) -> (ISeq s (CountRange 0))]
                )

(t/ann-protocol [[s :variance :covariant]]
                INext
                -next
                [(INext s) -> (U nil (t/NonEmptySeq s))]
                )

(t/defprotocol IColl)
(t/defprotocol ISeqable)
(t/defprotocol IEmptyableCollection)
(t/defprotocol ISeq)
(t/defprotocol INext)

(t/ann every?
     (All [x [y :< x] p]
       [[y -> Any
         :filters {:then (is p 0)}]
        -> (U nil (ISeqable (ISeq x)))]))

(t/ann first (All [x]
               (Fn [(ISeqable (t/NonEmptySeq x)) -> x]
                   [(U nil (ISeqable (U nil (t/NonEmptySeq x)))) -> (U nil x)])))

; A type function takes a number of types and returns a type
;
; A function takes a number of values and returns a value
;
; A function intersection describes 
; 

(t/defalias MapLike
  (TFn [[kv :variance :covariant]
        [meta :variance :covariant]]
    (I (IColl (U nil '[Any Any] (IMapEntry Any Any))
              (TFn 
                [[x :variance :covariant
                  :< (U nil '[Any Any] (IMapEntry Any Any))]]
                (MapLike (U kv x) meta))))
       #_(IMeta meta)
       #_(IWithMeta (TFn [[new-meta :variance :covariant
                         :< (U nil (t/Map Any Any))]]
                    (MapLike kv new-meta)))))

;; How can we make occurrence typing updating extensible?
;; eg. (HashMap a b) + (IMap c d) = (HashMap (I a c) (I b d))
;;
;; We might want to express things like:
;;  (when (IMap c d)
;;    (HashMap (I a c) (I b d)))

(t/ann ^:no-check 
     conj 
     (All [max-arg
           [a :< max-arg]
           [res :< (Rec [m]
                     (TFn [[x :variance :covariant]]
                       (IColl max-arg m)))]]
          (Fn [(IColl max-arg res) a a * -> (res a)])))
(defn conj [target arg & args]
  (apply clojure.core/conj target arg args))

#_(t/cf 
  (fn []
  (t/inst conj 
          (U nil '[Any Any] (IMapEntry Any Any))
          (U nil '[Number Number] (IMapEntry Number Number))
          (TFn [[x :variance :covariant]]
             (MapLike (IMapEntry Number Number) (U nil (t/Map Any Any)))))))

; the above appears to choke on computing the following subtyping relationship:

;(All
;  [Poly-fresh-sym98115 Poly-fresh-sym98117]
;  (TFn
;    [[fresh-sym98258
;      :variance
;      :covariant
;      :<
;      (U
;       (clojure.lang.IMapEntry Poly-fresh-sym98115 Poly-fresh-sym98117)
;       nil
;       (HVec [Poly-fresh-sym98115 Poly-fresh-sym98117]))]]
;    (clojure.core.typed.test.conj-experiment/MapLike
;      (U Poly-fresh-sym98115 java.lang.Number)
;      (U java.lang.Number Poly-fresh-sym98117)
;      (U (clojure.core.typed/Map Any Any) nil))))
;(Rec
;  [G__98082]
;  (TFn
;    [[fresh-sym98261 :variance :covariant]]
;    (clojure.core.typed.test.conj-experiment/IColl
;      (U (HVec [Any Any]) nil (clojure.lang.IMapEntry Any Any))
;      G__98082)))


;(ann ^:no-check
;     into
;     (All [e
;           [Res :< (Rec [c]
;                     (TFn [[x :variance :covariant]]
;                       (IColl Any c)))]]
;          (Fn [(IColl arg Res) (U nil (Seqable arg)) -> (Res arg)])
;(defn into [target arg]
;  (clojure.core/into target arg))
;
;(ann make-map (All [x y]
;                   [x y -> (IColl (IMapEntry x y)
;                                  (TFn [[arg :variance :covariant
;                                         :< (U nil '[x y])]]
;                                       arg)
;                                  (Rec [res]
;                                       (All [x1 x2]
;                                          (TFn [[arg :variance :covariant
;                                                 :< (U nil (IMapEntry x y))]]
;                                               (IColl (IMapEntry x1 y1)
;                                                      (TFn [[arg :variance :covariant
;                                                             :< (U nil (IMapEntry x1 y1))]]
;                                                           arg)
;                                                      res)))))]))
;(defn make-map [& kvs] (apply hash-map kvs))
;
;conj-map
;
;(fn []
;  (conj 

;
;(defalias Map 
;  (TFn [[a :variance :covariant]
;        [b :variance :covariant]]
;    (I (IPersistentCollection (U nil (IMapEntry a b))
;                              (All [k v]
;                                (TFn [[x :varance :covariant
;                                       :< (U nil (IMapEntry k v))]]
;                                  (Map (U a k) (U b v)))))
;       (IPersistentMap a b)
;       (Seqable (Seq (IMapEntry a b)))
;       (IFn (All [d]
;                 (Fn [Any -> (U nil b)]
;                     [Any d -> (U b d)])))
;       (ILookup a b)
;       (Associative a b))))

(t/ann-protocol [[x :variance :covariant]]
                Dummy)
(t/defprotocol Dummy)

(t/defalias DummyAlias
  (TFn [[k :variance :covariant]]
       (All [a]
            [-> (DummyAlias (U k a))])))
)

;; WithMeta
;; 
;; Updating metadata
;;
;; Metadata seems like it might be useful to update with Assoc,
;; but it's a "special" abstraction in Clojure that is backed by
;; a "hidden" field __meta or _meta.
;;
;; (WithMeta [m]
;;   (PersistentHashMap x y z)
;;   (PersistentHashMap x y m))
;;
;; (add-WithMeta-case 
;;   [x]
;;   [] 
;;   (Symbol (U nil (Map Any Any)))
;;   (Symbol x))
;;
;; (add-WithMeta-case 
;;   [m]
;;   [k v]
;;   (IPersistentMap k v (U nil (Map Any Any)))
;;   (IPersistentMap k v m))
