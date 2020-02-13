;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.jvm.base-env-clj-rclass
  (:import (clojure.lang Named IMapEntry AMapEntry Seqable
                         LazySeq PersistentHashSet PersistentTreeSet PersistentTreeMap PersistentList APersistentVector
                         APersistentSet IPersistentSet IPersistentMap IPersistentVector
                         APersistentMap IDeref ISeq IPersistentCollection
                         ILookup Indexed Associative IPersistentStack PersistentVector Cons
                         IPersistentList IRef ARef Reversible
                         ITransientCollection ITransientSet ITransientAssociative ITransientMap
                         ITransientVector PersistentHashMap Reduced MultiFn)
           (java.util Collection RandomAccess))
  (:require [clojure.core.typed.checker.base-env-helper :as h]
            [clojure.core.typed.checker.base-env-common :refer [delay-and-cache-env]]
            [clojure.core.typed.checker.fold-default]
            [clojure.core.typed.checker.jvm.rclass-env :as rcls]
            [clojure.core.typed :refer [Any Nothing TFn Rec
                                        Pred U I All IFn
                                        HVec HSequential]
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

Associative [[[m :variance :covariant]
              [k :variance :covariant]
              [v :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection m)
              Seqable (Seqable m)
              ILookup (ILookup k v)}]

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
                    Associative (Associative a Number a)
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
                    Associative (Associative a Number a)
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
                   Associative (Associative a Number a)
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
            Associative (Associative (U a b) Number (U a b))
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
            Associative (Associative (U a b) Number (U a b))
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
                 Associative (Associative (AMapEntry a b) a b)}]

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
                 Associative (Associative (AMapEntry a b) a b)}
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
                    Associative (Associative (AMapEntry a b) a b)
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
                    Associative (Associative (AMapEntry a b) a b)
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

; 1. what is the variance of the dispatch function? covariant?
;
;   (ann f (MultiFn [t/Int -> t/Int] [t/Int -> Bool]))
;   (defmulti f even?)
;
;   ; make return types less specific -- seems ok. just means
;   ; we can infer less in defmethod's.
;   (ann-form f (MultiFn [t/Int -> Any] [t/Int -> Any]))
;
;   (ann g (MultiFn [t/Int -> t/Int] [t/Int -> Bool]))
;   (defmulti g odd?)
;
; 2. what bound do we want on `f` and `d`?
;   ; say if we don't have `d`'s lower bound as EveryIFn..
;   (ann f (MultiFn .. ':a))
;   (defmulti f :a)
;
;   (ann g (MultiFn .. (ToIFn ':a)))
;   (defmulti g (fn [a] (:a a)))
;
;   ; is this desirable? let's use the lower bound, since we can always
;   ; make the type more permissive later.
;   (ann-form f (TypeOf g)) ;ok
;   (ann-form g (TypeOf f)) ;fails

#_#_
MultiFn [[[f :variance :covariant :> EveryIFn :< AnyIFn]
          [d :variance :covariant :> EveryIFn :< AnyIFn]
          ;; only support :default default for now, and no hierarchy support
          #_#_
          :named
          {default [:variance :covariant,
                    :between [EveryIFn AnyValue],
                    :default ':default]
           hierarchy [:variance :covariant
                      :< (Var Hierarchy)
                      :default GlobalHierarchy]}]
         :replace
         {clojure.lang.IFn f}]

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
