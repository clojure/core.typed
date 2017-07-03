(ns clojure.core.typed.base-env-clj-rclass
  (:import (clojure.lang Named IMapEntry AMapEntry Seqable
                         LazySeq PersistentHashSet PersistentTreeSet PersistentTreeMap PersistentList APersistentVector
                         APersistentSet IPersistentSet IPersistentMap IPersistentVector
                         APersistentMap IDeref ISeq IPersistentCollection
                         ILookup Indexed Associative IPersistentStack PersistentVector Cons
                         IPersistentList IRef ARef Reversible
                         ITransientCollection ITransientSet ITransientAssociative ITransientMap
                         ITransientVector PersistentHashMap Reduced)
           (java.util Collection RandomAccess))
  (:require [clojure.core.typed.base-env-helper :as h]
            [clojure.core.typed.base-env-common :refer [delay-and-cache-env]]
            [clojure.core.typed.fold-default]
            [clojure.core.typed.rclass-env :as rcls]
            [clojure.core.typed :refer [Any Nothing TFn Rec
                                        Pred U I All IFn
                                        HVec HSequential Keyword]
             :as t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Altered Classes

;; TODO remove redundant ancestors, add tests to ensure they are preserved.

(delay-and-cache-env init-altered-env
                     (assert (class? Seqable))
  (h/alters

Seqable [[[a :variance :covariant]]
         ]

Reversible [[[a :variance :covariant]]
            ]

IPersistentCollection [[[a :variance :covariant]]
                       :replace
                       {Seqable (Seqable a)}]

ISeq [[[a :variance :covariant]]
      :replace
      {Seqable (Seqable a)
       IPersistentCollection (IPersistentCollection a)}]

clojure.lang.ChunkBuffer [[[a :variance :invariant]]]

clojure.lang.IChunkedSeq [[[a :variance :covariant]]
                          :replace
                          {Seqable (Seqable a)
                           IPersistentCollection (IPersistentCollection a)
                           ISeq (ISeq a)}]

clojure.lang.Indexed [[[a :variance :covariant]]]

clojure.lang.IChunk [[[a :variance :covariant]]
                     :replace
                     {clojure.lang.Indexed (clojure.lang.Indexed a)}]

ILookup [[[a :variance :covariant]
          [b :variance :covariant]]]

IPersistentSet [[[a :variance :covariant]]
                :replace
                {IPersistentCollection (IPersistentCollection a)
                 Seqable (Seqable a)}
                :unchecked-ancestors
                #{[Any -> (U a nil)]}] ;; not a real ancestor

APersistentSet [[[a :variance :covariant]]
                :replace
                {Seqable (Seqable a)
                 java.util.Set (java.util.Set a)
                 Collection (Collection a)
                 Iterable (Iterable a)
                 IPersistentCollection (IPersistentCollection a)
                 IPersistentSet (IPersistentSet a)}
                :unchecked-ancestors
                #{[Any -> (U a nil)]}
                ]

PersistentHashSet [[[a :variance :covariant]]
                   :replace
                   {Seqable (Seqable a)
                    java.util.Set (java.util.Set a)
                    Iterable (Iterable a)
                    Collection (Collection a)
                    APersistentSet (APersistentSet a)
                    IPersistentSet (IPersistentSet a)
                    IPersistentCollection (IPersistentCollection a)}
                   :unchecked-ancestors
                   #{[Any -> (U a nil)]}]

PersistentTreeSet [[[a :variance :covariant]]
                   :replace
                   {Seqable (Seqable a)
                    java.util.Set (java.util.Set a)
                    Iterable (Iterable a)
                    Collection (Collection a)
                    Reversible (Reversible a)
                    APersistentSet (APersistentSet a)
                    IPersistentSet (IPersistentSet a)
                    IPersistentCollection (IPersistentCollection a)}
                    :unchecked-ancestors
                    #{[Any -> (U a nil)]}]

Associative [[[a :variance :covariant]
              [b :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection Any)
              Seqable (Seqable Any)
              ILookup (ILookup a b)}]

;ITransientCollection [[[w :variance :contravariant]
;                       [r :variance :covariant]]]
;
;ITransientSet [[[w :variance :contravariant]
;                [r :variance :covariant]]
;               :replace
;               {ITransientCollection (ITransientCollection w r)}]
;
;ITransientAssociative [[[wkey :variance :contravariant]
;                        [wval :variance :contravariant]
;                        [rkey :variance :covariant]
;                        [rval :variance :covariant]]
;                       :replace
;                       {ILookup (ILookup rkey rval)
;                        ITransientCollection (ITransientCollection (IMapEntry wkey wval)
;                                                                   (IMapEntry rkey rval))}]
;
;ITransientMap [[[wkey :variance :contravariant]
;                [wval :variance :contravariant]
;                [rkey :variance :covariant]
;                [rval :variance :covariant]]
;               :replace
;               {ILookup (ILookup rkey rval)
;                ITransientAssociative (ITransientAssociative wkey wval rkey rval)
;                ITransientCollection (ITransientCollection (IMapEntry wkey wval)
;                                                           (IMapEntry rkey rval))}]
;
;ATransientMap [[[wkey :variance :contravariant]
;                [wval :variance :contravariant]
;                [rkey :variance :covariant]
;                [rval :variance :covariant]]
;               {;TODO override AFn
;                ILookup (ILookup rkey rval)
;                ITransientAssociative (ITransientAssociative wkey wval rkey rval)
;                ITransientCollection (ITransientCollection (IMapEntry wkey wval)
;                                                           (IMapEntry rkey rval))}]
;
;ITransientVector [[[w :variance :contravariant]
;                   [r :variance :covariant]]
;                  :replace
;                  {ITransientAssociative (ITransientAssociative Number wval Number rval)
;                   ITransientCollection (ITransientCollection w r)
;                   Indexed (Indexed r)}]
;
;IEditableCollection [[c :variance :covariant]]

IPersistentStack [[[a :variance :covariant]]
                  :replace
                  {IPersistentCollection (IPersistentCollection a)
                   Seqable (Seqable a)}]


;define vectors before maps, as HVector is part of map ancestors
IPersistentVector [[[a :variance :covariant]]
                   :replace
                   {IPersistentCollection (IPersistentCollection a)
                    Seqable (Seqable a)
                    Reversible (Reversible a)
                    IPersistentStack (IPersistentStack a)
                    ILookup (ILookup Number a)
                    Associative (Associative Number a)
                    Indexed (Indexed a)}
                   :unchecked-ancestors
                   #{[Number -> a]}] ;; not a real ancestor, but very useful

APersistentVector [[[a :variance :covariant]]
                   :replace
                   {IPersistentCollection (IPersistentCollection a)
                    Seqable (Seqable a)
                    Iterable (Iterable a)
                    Collection (Collection a)
                    java.util.List (java.util.List a)
                    RandomAccess (RandomAccess a)
                    IPersistentVector (IPersistentVector a)
                    Reversible (Reversible a)
                    IPersistentStack (IPersistentStack a)
                    ILookup (ILookup Number a)
                    Associative (Associative Number a)
                    Indexed (Indexed a)}
                   :unchecked-ancestors
                   #{[Number -> a]}]

PersistentVector [[[a :variance :covariant]]
                  :replace
                  {APersistentVector (APersistentVector a)
                   IPersistentCollection (IPersistentCollection a)
                   Iterable (Iterable a)
                   Collection (Collection a)
                   java.util.List (java.util.List a)
                   RandomAccess (RandomAccess a)
                   Seqable (Seqable a)
                   IPersistentVector (IPersistentVector a)
                   Reversible (Reversible a)
                   IPersistentStack (IPersistentStack a)
                   ILookup (ILookup Number a)
                   Associative (Associative Number a)
                   Indexed (Indexed a)
                   #_IEditableCollection #_(IEditableCollection (ITransientVector a))}
                  :unchecked-ancestors
                  #{[Number -> a]}]

IMapEntry [[[a :variance :covariant]
            [b :variance :covariant]]]

clojure.lang.AMapEntry 
          [[[a :variance :covariant]
            [b :variance :covariant]]
           :replace
           {IMapEntry (IMapEntry a b)
            Iterable (Iterable (U a b))
            RandomAccess (RandomAccess (U a b))
            IPersistentCollection (IPersistentCollection 
                                    (U a b))
            java.util.List (java.util.List (U a b))
            Collection (Collection (U a b))
            Seqable (Seqable (U a b))
            IPersistentVector (IPersistentVector (U a b))
            Reversible (Reversible (U a b))
            IPersistentStack (IPersistentStack (U a b))
            ILookup (ILookup Number (U a b))
            Associative (Associative Number (U a b))
            Indexed (Indexed (U a b))
            APersistentVector (APersistentVector (U a b))}
           :unchecked-ancestors
           #{'[a b]
             [Number -> (U a b)]}]

clojure.lang.MapEntry
          [[[a :variance :covariant]
            [b :variance :covariant]]
           :replace
           {IMapEntry (IMapEntry a b)
            Iterable (Iterable (U a b))
            RandomAccess (RandomAccess (U a b))
            java.util.List (java.util.List (U a b))
            Collection (Collection (U a b))
            AMapEntry (AMapEntry a b)
            IPersistentCollection (IPersistentCollection (U a b))
            Seqable (Seqable (U a b))
            IPersistentVector (IPersistentVector (U a b))
            Reversible (Reversible (U a b))
            IPersistentStack (IPersistentStack (U a b))
            ILookup (ILookup Number (U a b))
            Associative (Associative Number (U a b))
            Indexed (Indexed (U a b))
            APersistentVector (APersistentVector (U a b))}
           :unchecked-ancestors
           #{'[a b]
             [Number -> (U a b)]}]

IPersistentMap [[[a :variance :covariant]
                 [b :variance :covariant]]
                :replace
                {IPersistentCollection (IPersistentCollection (AMapEntry a b))
                 Iterable (Iterable (AMapEntry a b))
                 Seqable (Seqable (AMapEntry a b))
                 ILookup (ILookup a b)
                 Associative (Associative a b)}]

clojure.lang.ASeq [[[a :variance :covariant]]
      :replace
      {IPersistentCollection (IPersistentCollection a)
       Iterable (Iterable a)
       Collection (Collection a)
       java.util.List (Collection a)
       Seqable (Seqable a)
       ISeq (ISeq a)
       }]

APersistentMap [[[a :variance :covariant] 
                 [b :variance :covariant]]
                :replace
                {IPersistentCollection (IPersistentCollection (AMapEntry a b))
                 Iterable (Iterable (AMapEntry a b))
                 IPersistentMap (IPersistentMap a b)
                 Seqable (Seqable (AMapEntry a b))
                 ILookup (ILookup a b)
                 Associative (Associative a b)}
                :unchecked-ancestors
                #{(All [d]
                          (IFn [Any -> (U nil b)]
                              [Any d -> (U b d)]))}]


PersistentTreeMap [[[a :variance :covariant] 
                    [b :variance :covariant]]
                   :replace
                   {IPersistentCollection (IPersistentCollection (AMapEntry a b))
                    Iterable (Iterable (AMapEntry a b))
                    IPersistentMap (IPersistentMap a b)
                    APersistentMap (APersistentMap a b)
                    Seqable (Seqable (AMapEntry a b))
                    ILookup (ILookup a b)
                    Associative (Associative a b)
                    Reversible (Reversible (AMapEntry a b))
                    #_IEditableCollection #_(IEditableCollection (ITransientMap a b a b))}
                   :unchecked-ancestors
                   #{(All [d]
                             (IFn [Any -> (U nil b)]
                                 [Any d -> (U b d)]))}]

PersistentHashMap [[[a :variance :covariant] 
                    [b :variance :covariant]]
                   :replace
                   {IPersistentCollection (IPersistentCollection (AMapEntry a b))
                    Iterable (Iterable (AMapEntry a b))
                    IPersistentMap (IPersistentMap a b)
                    APersistentMap (APersistentMap a b)
                    Seqable (Seqable (AMapEntry a b))
                    ILookup (ILookup a b)
                    Associative (Associative a b)
                    #_IEditableCollection #_(IEditableCollection (ITransientMap a b a b))}
                   :unchecked-ancestors
                   #{(All [d]
                             (IFn [Any -> (U nil b)]
                                 [Any d -> (U b d)]))}]

Cons [[[a :variance :covariant]]
      :replace
      {IPersistentCollection (IPersistentCollection a)
       Iterable (Iterable a)
       Collection (Collection a)
       java.util.List (java.util.List a)
       clojure.lang.ASeq (clojure.lang.ASeq a)
       Seqable (Seqable a)
       ISeq (ISeq a)
       }]

IPersistentList [[[a :variance :covariant]]
                 :replace
                 {IPersistentCollection (IPersistentCollection a)
                  Seqable (Seqable a)
                  IPersistentStack (IPersistentStack a)}]

PersistentList [[[a :variance :covariant]]
                :replace
                {IPersistentCollection (IPersistentCollection a)
                 Iterable (Iterable a)
                 Collection (Collection a)
                 java.util.List (java.util.List a)
                 clojure.lang.ASeq (clojure.lang.ASeq a)
                 Seqable (Seqable a)
                 IPersistentList (IPersistentList a)
                 ISeq (ISeq a)
                 IPersistentStack (IPersistentStack a)
                 }]

clojure.lang.Keyword [[]
         :unchecked-ancestors
         #{(All [x] 
                (IFn [(U nil (IPersistentMap Any x)) -> (U nil x)]
                    [Any -> Any]))}]

IDeref [[[r :variance :covariant]]]
clojure.lang.IBlockingDeref [[[r :variance :covariant]]]


IRef [[[w :variance :contravariant]
       [r :variance :covariant]]
      :replace
      {IDeref (IDeref r)}]

ARef [[[w :variance :contravariant]
       [r :variance :covariant]]
      :replace
      {IRef (IRef w r)
       IDeref (IDeref r)}]

clojure.lang.Ref 
     [[[w :variance :contravariant]
       [r :variance :covariant]]
      :replace
      {IRef (IRef w r)
       ARef (ARef w r)
       IDeref (IDeref r)}]

clojure.lang.Agent 
      [[[w :variance :contravariant]
        [r :variance :covariant]]
       :replace
       {IRef (IRef w r)
        IDeref (IDeref r)
        }]


clojure.lang.Delay [[[r :variance :covariant]]
                    :replace
                    {IDeref (IDeref r)}]

;invoking Var as IFn is a special case in the checker
clojure.lang.Var 
    [[[w :variance :contravariant]
      [r :variance :covariant]]
     :replace
     {IRef (IRef w r)
      IDeref (IDeref r)
      ARef (ARef w r)}]

clojure.lang.Atom 
     [[[w :variance :contravariant]
       [r :variance :covariant]]
      :replace
      {ARef (ARef w r)
       IRef (IRef w r)
       IDeref (IDeref r)}]

LazySeq [[[a :variance :covariant]]
         :replace
         {Seqable (Seqable a)
          Collection (Collection a)
          java.util.List (Collection a)
          Iterable (Iterable a)
          ISeq (ISeq a)
          IPersistentCollection (IPersistentCollection a)}]

Reduced [[[a :variance :covariant]]
         :replace
         {IDeref (IDeref a)}]

;;; We override the internal Java classes that clojure.lang.* classes use
;;; and simulate some of them extending Clojure interfaces as if they were protocols

; Hack for Seqable things. Not needed if Seqable was a protocol.

java.lang.CharSequence [[]
                        :unchecked-ancestors
                        #{(Seqable Character)
                          (Indexed Character)}]

;FIXME Need to correctly check ancestors, this shouldn't be necessary because String is a CharSequence
; CTYP-15
java.lang.String [[]
                  :unchecked-ancestors
                  #{(Seqable Character)
                    (Indexed Character)}]

java.lang.Iterable [[[a :variance :covariant]]
                    :unchecked-ancestors
                    #{(Seqable a)}]

java.util.Set [[[a :variance :covariant]]
               :replace
               {Iterable (Iterable a)
                Collection (Collection a)}
               :unchecked-ancestors
               #{(Seqable a)}]


java.util.List [[[a :variance :covariant]]
                :replace
                {Iterable (Iterable a)
                 Collection (Collection a)}
                :unchecked-ancestors
                #{(Seqable a)}]

java.util.Collection [[[a :variance :covariant]]
                      :replace
                      {Iterable (Iterable a)}
                      :unchecked-ancestors
                      #{(Seqable a)}]

java.util.RandomAccess [[[a :variance :covariant]]
                        :unchecked-ancestors
                        #{(Indexed a)}]

))

(defn reset-rclass-env! []
  (rcls/reset-rclass-env! (init-altered-env)))
