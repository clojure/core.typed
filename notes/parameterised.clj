; nil is an ISeq
; seq returns an ISeq (includes nil)
; nil <: (Seqable Unit)
; nil <: (ISeq Unit)

(+T clojure.core/seq [(U nil (Seqable A)) -> (ISeq A)])

Nil - 

Nothing - bottom value

# Advantages of Nil <: Null

- Distinguish between nil and null
- null is not an ISeq (but nil is ...)

# Advantages of just Nil

- nil is still null
- no special cases for void
- hides details of null, void in type system
  when they aren't needed

nil <!: java.lang.Void
nil <!: java.lang.Void/TYPE
nil <!: Null
nil <: Seq[Nothing]
nil <: IMeta
nil <: IObj
nil <: Counted
nil <: ILookup
nil <: Associative

(+T clojure.core/first [(U (Seqable A) nil) -> A])

; # Should we type methods of clojure.lang.* interfaces?
; 
; - We could type check definitions of clojure.core functions
; - But it seems advantageous to abstract over clojure.core
; - eg. pretend clojure.lang.ISeq/seq is the same as clojure.core/seq
; - but there is a mismatch
; - eg. (iseq? nil) => false
; - but the type system says otherwise
; 
; # Should we abstract over ISeq?
; 
; - (seq x) is really [(U Seqable nil Iterable ...) -> (U nil ISeq)]
; - Would rather [Seqable -> ISeq]
; - We could introduce a new type
; - (def-type-alias SeqableI (A) (U (Seqable A) (Iterable A) (Array A) ...))
; - (def-type-alias SeqableOrNil (A) (U nil (SeqableI A)))
; - (def-type-alias SeqOrNil (A) (U nil (ISeq A)))
; - seq :- [(SeqableOrNil A) -> (SeqOrNil A)]
; - count :- [(SeqableOrNil A) -> (SeqOrNil A)]

(def-typed-interface Seqable (+A)
  :methods
  [(seq [this])])

(def-typed-interface IPersistentCollection (+A)
  :extends-interface
  [(Seqable A)]
  :methods
  [(count [this])
   (cons [this o])
   (empty [this])
   (equiv [this o])])

(def-typed-interface ISeq (+A)
  :extends-interface
  [(IPersistentCollection A)]
  :methods
  [(first [this])])

(def-typed-interface IPersistentVector (+A)
  :extends-interface
  [(Associative Long A)
   (Sequential A)
   (IPersistentStack A)
   Reversible
   Indexed]
  :methods
  [(assocN [this i va])
   (cons [this o])])

(def-typed-abstract APersistentVector (+A)
  :extends-abstract
  [(AFn ...)] ;TODO
  :implements-interface
  [(IPersistentVector A)
   (Iterable A)
   (List A)
   RandomAccess
   Comparable
   Seriablizable
   IHashEq])

(def-typed-class PersistentVector (+A)
  :extends-abstract
  [(APersistentVector A)]
  :implements-interface
  [IObj
   IEditableCollection])

(def-typed-interface IMeta ()
  :methods
  [(meta [this])]
  )

(def-typed-interface IObj ()
  :extends-interface
  [IMeta]
  :methods
  [(with-meta [this meta])]
  )

(def-typed-interface IEditableInterface ()
  :methods
  [(asTransient [this])])

(def-typed-interface IHashEq ()
  :methods
  [(hashEq [this])])

(def-typed-interface MapEntry 
                     )

(defprotocol ISeq (+A)
  (-first [this] ...))

(definterface Iterable ()
  )

(defprotocol IMeta ()
  )

PersistentVector[X] extends APersistentVector[X] implements IObj[X], IEditableCollection[X]
APersistentVector[X] extends AFn[Long -> X] implements IPersistentVector[X], Iterable[X]
                                                       List[X], RandomAccess[X], Comparable[X],
                                                       Serializable[X], IHashEq[X]

; Fn types? have infinite domain..
; See scala's function1..function20..function?
AFn

(defn symbol? [a :- Any]
  :filters
  [a :- Symbol
   a !:- Symbol])

(defn identity (A) [a :- A] A
  :filters
  [a !:- (U nil false)
   a :- (U nil false)]
  a)

(defn every? (A) 
  [f :- [A -> B]
   coll :- (Seqable A)]
  boolean
  :filter
  [coll :- (Seqable (then-filter f))
   coll :- (Seqable (U (then-filter f)
                       (else-filter f)))]
  )

(let [x (long-or-nil-seq)] ; (Seq (U Long nil))
  (when (every? identity x)
    ; x :- (Seqable (U Long nil))
    ; with proposition x :- (Seqable !:- (U nil false))
    ; update (Seqable (U Long nil)
    ;        (Seqable !:- (U nil false))
    ; => x :- (Seqable Long)
    (apply + x)))

(let [x (some-seq)] ; (Seq Any)
  (if (symbol? (first x))
    ; x :- (Seq Any)
    ; and (first x) :- Symbol
    (str (namespace (first x)) (name (first x)))))
