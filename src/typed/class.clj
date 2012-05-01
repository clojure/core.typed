
(annotate-class (Seqable a))
(annotate-class (IPersistentCollection a)
  :extends [(Seqable a)])

(annotate-class (IPersistentStack a)
  :extends [(IPersistentCollection a)])

(annotate-class (IPersistentList a)
  :extends [(IPersistentStack a)])

(annotate-class (IPersistentVector a)
  :extends [(Associative Long a)
            (IPersistentStack a)])

(annotate-class (APersistentVector a)
  :extends [(IPersistentVector a)])

(annotate-class (PersistentVector a)
  :extends [(APersistentVector a)
            (IObj Any)])

(annotate-class (IMapEntry a b))

(annotate-class (AMapEntry a b)
  :extends [(IMapEntry a b)
            (APersistentVector* a b)]) ;; TODO * extends to constant class?

(annotate-class (MapEntry a b)
  :extends [(AMapEntry a b)])

(annotate-class (ILookup a b))

(annotate-class (Associative a b)
  :extends [(IPersistentCollection (IMapEntry a b))
            (ILookup a b)])

(annotate-class (IPersistentMap a b)
  :extends [(Associative a b)])

(annotate-class (IDeref a))

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
            (ARef a)
            (IRef a)])

