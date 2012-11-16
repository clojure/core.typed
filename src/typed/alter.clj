
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Altered Classes

;; TODO fix metadata representation
;; TODO remove redundant ancestors, add tests to ensure they are preserved.

(alter-class Seqable [[a :variance :covariant]])

(alter-class IMeta [[a :variance :covariant]])

(alter-class IPersistentCollection [[a :variance :covariant]]
             :replace
             {Seqable (Seqable a)})

(alter-class ISeq [[a :variance :covariant]]
             :replace
             {Seqable (Seqable a)
              IPersistentCollection (IPersistentCollection a)})

(alter-class ILookup [[a :variance :covariant]
                      [b :variance :covariant]])

(alter-class IPersistentSet [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)})

(alter-class APersistentSet [[a :variance :covariant]]
             :replace
             {Seqable (Seqable a)
              IFn [Any -> (U a nil)]
              AFn [Any -> (U a nil)]
              IPersistentCollection (IPersistentCollection a)
              IPersistentSet (IPersistentSet a)})

(alter-class PersistentHashSet [[a :variance :covariant]]
             :replace
             {Seqable (Seqable a)
              APersistentSet (APersistentSet a)
              IFn [Any -> (U a nil)]
              AFn [Any -> (U a nil)]
              IPersistentSet (IPersistentSet a)
              IPersistentCollection (IPersistentCollection a)
              IMeta (IMeta Any)})

(alter-class Associative [[a :variance :covariant]
                          [b :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection Any)
              Seqable (Seqable Any)
              ILookup (ILookup a b)})

(alter-class IMapEntry [[a :variance :covariant]
                        [b :variance :covariant]])

(alter-class IPersistentMap [[a :variance :covariant]
                             [b :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection (IMapEntry a b))
              Seqable (Seqable (IMapEntry a b))
              ILookup (ILookup a b)
              Associative (Associative a b)})

(alter-class ASeq [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)
              ISeq (ISeq a)
              IMeta (IMeta Any)})

(alter-class IPersistentStack [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)})

(alter-class IPersistentVector [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)
              IPersistentStack (IPersistentStack a)
              ILookup (ILookup Number a)
              Associative (Associative Number a)})

(alter-class APersistentMap [[a :variance :covariant] [b :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection (IMapEntry a b))
              IPersistentMap (IPersistentMap a b)
              Seqable (Seqable (IMapEntry a b))
              IFn (All [d]
                    (Fn [Any -> (U nil b)]
                        [Any d -> (U b d)]))
              ILookup (ILookup a b)
              Associative (Associative Number a)})

(alter-class APersistentVector [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)
              IPersistentVector (IPersistentVector a)
              IFn [Number -> a]
              IPersistentStack (IPersistentStack a)
              ILookup (ILookup Number a)
              Associative (Associative Number a)})

(alter-class PersistentVector [[a :variance :covariant]]
             :replace
             {APersistentVector (APersistentVector a)
              IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)
              IPersistentVector (IPersistentVector a)
              IFn [Number -> a]
              IPersistentStack (IPersistentStack a)
              ILookup (ILookup Number a)
              IMeta (IMeta Any)
              Associative (Associative Number a)})

(alter-class Cons [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              ASeq (ASeq a)
              Seqable (Seqable a)
              ISeq (ISeq a)
              IMeta (IMeta Any)})

(alter-class IPersistentList [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)
              IPersistentStack (IPersistentStack a)})

(alter-class PersistentList [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              ASeq (ASeq a)
              Seqable (Seqable a)
              IPersistentList (IPersistentList a)
              ISeq (ISeq a)
              IPersistentStack (IPersistentStack a)
              IMeta (IMeta Any)})

(alter-class Symbol []
             :replace
             {IMeta (IMeta Any)})

(alter-class IDeref [[r :variance :covariant]])


(alter-class IRef [[w :variance :contravariant]
                   [r :variance :covariant]]
             :replace
             {IDeref (IDeref r)})

(alter-class IReference [[w :variance :contravariant]
                         [r :variance :covariant]]
       :replace
       {IMeta (IMeta Any)})

(alter-class AReference [[w :variance :contravariant]
                         [r :variance :covariant]]
             :replace
             {IMeta (IMeta Any)
              IReference (IReference w r)})

(alter-class ARef [[w :variance :contravariant]
                   [r :variance :covariant]]
             :replace
             {IRef (IRef w r)
              IMeta (IMeta Any)
              AReference (AReference w r)
              IDeref (IDeref r)
              IReference (IReference w r)})

(alter-class Atom [[w :variance :contravariant]
                   [r :variance :covariant]]
             :replace
             {IRef (IRef w r)
              IMeta (IMeta Any)
              AReference (AReference w r)
              ARef (ARef w r)
              IDeref (IDeref r)
              IReference (IReference w r)})

(alter-class LazySeq [[a :variance :covariant]]
             :replace
             {Seqable (Seqable a)
              ISeq (ISeq a)
              IMeta (IMeta Any)
              IPersistentCollection (IPersistentCollection a)})

