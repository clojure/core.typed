(ns clojure.core.typed.test.conj-experiment
  (:refer-clojure :exclude [conj into])
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
;          (Rec [c]
;            (ICollection max-arg m c))))]
;
; The upper bound constrains `res` to be a type function that takes a `conj` argument (bounded
; by `max-arg`) and returns an ICollection.

(t/ann-protocol [[max-arg :variance :covariant]
                 [res :variance :covariant
                  :< (Rec [m]
                       (TFn [[arg :variance :covariant
                              :< max-arg]] 
                         (Rec [c]
                           (IColl Any m c))))]]
                IColl
                -conj 
                (All [[a :< max-arg]] 
                  [(IColl max-arg res) a a * -> (res a)])
                )

(t/ann-protocol [[emp :variance :covariant
                  :< (Rec [m]
                       (IColl Any 
                              (TFn [[arg :variance :covariant]]
                                m)
                              m))]]
                IEmptyableCollection
                ;empty 
                ;[(IEmptyableCollection emp) -> emp]
                )

(t/ann-protocol [[metafn :variance :covariant
                  :< (Rec [m]
                       (TFn [[new-meta :variance :covariant
                              :< (U nil (Map Any Any))]]
                         (I (IMeta new-meta)
                            (IWithMeta m))))]]
                IWithMeta
                ;with-meta
                ;(All [mmap]
                ; [(IWithMeta metafn) mmap -> (metafn mmap)])
                )

(t/ann-protocol [[mmap :variance :covariant
                  :< (U nil (Map Any Any))]]
                IMeta
                ;meta
                ;[(IMeta mmap) -> mmap]
                )

(t/ann-protocol [[s :variance :covariant
                  :< (U nil (NonEmptySeq Any))]]
                ISeqable
                ;seq
                ;[(ISeqable s) -> s]
                )

(t/ann-protocol [[s :variance :covariant]]
                ISeq
                ;first
                ;[(ISeq s) -> (U nil s)]
                ;rest
                ;[(ISeq s) -> (Seq s)]
                )

(t/ann-protocol [[s :variance :covariant]]
                INext
                ;next
                ;[(INext s) -> (U nil (Seq x))]
                )

(t/ann first (All [x]
               (Fn [(ISeqable (NonEmptySeq x)) -> x]
                   [(U nil (ISeqable (U nil (NonEmptySeq x)))) -> (U nil x)])))

(t/defprotocol> IColl)
(t/defprotocol> IWithMeta)
(t/defprotocol> IMeta)

(t/def-alias MapLike
  (TFn [[k :variance :covariant]
        [v :variance :covariant]
        [meta :variance :covariant]]
    (I (IColl (U nil '[Any Any] (IMapEntry Any Any))
              (All [a b]
                (TFn 
                  [[x :variance :covariant
                    :< (U nil '[a b] (IMapEntry a b))]]
                  (MapLike (U k a) (U v b) meta)))
              (MapLike Nothing Nothing nil))
       (IMeta meta
              ; if we want to update this type with (IMeta a),
              ; we can pass `a` to this TFn to perform this update.
              ; Extensible type updating!
              :update
              (TFn [[new-meta :variance :covariant]]
                (MapLike k v new-meta)))
       (IWithMeta (TFn [[new-meta :variance :covariant
                         :< (U nil (Map Any Any))]]
                    (MapLike k v new-meta))))))

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
                       (Rec [c]
                         (IColl max-arg m c))))]]
          (Fn [(Rec [m] (IColl max-arg res m)) a a * -> (res a)])))
(defn conj [target arg & args]
  (apply clojure.core/conj target arg args))

(fn []
  (t/inst conj 
          (U nil '[Any Any] (IMapEntry Any Any))
          (U nil '[Number Number] (IMapEntry Number Number))
          Any
          #_(TFn [[x :variance :covariant
                 :< (U nil '[Number Number] (IMapEntry Number Number))]]
             (MapLike Number Number))))

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
;(def-alias Map 
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
)
