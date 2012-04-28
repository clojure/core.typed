
(annotate-class Seqable [a])
(annotate-class IPersistentCollection [a]
  :extends [(Seqable a)])

(annotate-class IPersistentStack [a]
  :extends [(IPersistentCollection a)])

(annotate-class IPersistentList [a]
  :extends [Sequential
            (IPersistentStack a)])

(annotate-class IPersistentVector [a]
  :extends [(Associative Long a)
            Sequential
            (IPersistentStack a)
            Reversible
            Indexed])

(annotate-class APersistentVector [a]
  :extends [AFn
            (IPersistentVector a)
            Iterable
            List
            RandomAccess
            Comparable
            Serializable
            IHashEq])

(annotate-class PersistentVector [a]
  :extends [(APersistentVector a)
            (IObj IPersistentMap)
            IEditableCollection])

(annotate-class IMapEntry [a b])

(annotate-class ILookup [a b])

(annotate-class Associative [a b]
  :extends [(IPersistentCollection (IMapEntry a b))
            (ILookup a b)])

(annotate-class IPersistentMap [a b]
  :extends [Iterable
            (Associative a b)
            Counted])


(annotate-class IDeref [a])

(annotate-class IMeta [(a <! IPersistentMap)])
(annotate-class IObj [(a <! IPersistentMap)]
  :extends [(IMeta a)])

(annotate-class IRef [a]
  :extends [(IDeref a)])

(annotate-class IReference [(a <! IPersistentMap)]
  :extends [(IMeta a)])

(annotate-class AReference [(a <! IPersistentMap)]
  :extends [(IReference a)])

(annotate-class ARef [a]
  :extends [(AReference IPersistentMap)
            (IRef a)])

(annotate-class Atom [a]
  :extends [(ARef a)])

(annotate-class Ref [a]
  :extends [(Comparable Ref)
            IFn
            (ARef a)
            (IRef a)])

