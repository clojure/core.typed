;#; Base Types

(alter-poly-interface Seqable [[a :< (U (Inst ISeq _ _ _) Nil) :variance :covariant] ;result of (seq this)
                               ])

;cons, count, empty, equiv
(alter-poly-interface IPersistentCollection [[a :variance :covariant]  ;object to cons
                                             [b :< (Inst IPersistentCollection _ _ _)
                                              :variance :covariant]  ;cons result
                                             [c :< (Inst IPersistentCollection _ _ _)
                                              :variance :covariant]  ;empty
                                             ])

(alter-poly-interface ISeq [[a :variance :covariant] ;first
                            [b :< (Inst ISeq _)
                             :variance :covariant] ;rest output
                            [c :< (U Nil (Inst ISeq _))
                             :variance :covariant] ;next output
                            ])

;(get (ILookup [a -> b]) a) => b
(alter-poly-interface ILookup [[a :< (Fn [_ -> _])    ;just return Nothing for not found? then fns using this can union to get not found
                                :variance :covariant]])

(alter-poly-interface IPersistentSet [[a :variance :covariant] ;contents of set
                                      ])

;(assoc (Associative [a b -> c]) a b) => c
(alter-poly-interface Associative [[a :< (Fn [_ _ -> _]) ;key value -> assoc result
                                    :variance :covariant]
                                   ]
                      )

;(dissoc (IPersistentMap [c -> d]) c) => d
(alter-poly-interface IPersistentMap [[c :< (Fn [_ -> _])   ;dissockey -> dissocresult
                                       :variance :covariant]]
                      )

(alter-poly-interface IPersistentStack [[a :variance :covariant] ;peek result
                                        [b :< (Inst IPersistentStack _ _)
                                         :variance :covariant] ;pop result
                                        ])

(alter-poly-interface IPersistentVector [[a :variance :covariant]  ;key
                                         [b :variance :covariant]]);value

(alter-poly-interface Counted [[a :variance :covariant]  ;count
                              ])


(alter-poly-class Cons [[a :variance :covariant]
                        [b :< (U Nil (Inst ISeq a)) :variance :covariant]]
                  :replace
                  {Seqable (Inst Seqable (Inst Cons a b))
                   IPersistentColection (Inst IPersistentColection 
                                              a                             ;object to cons
                                              (Inst Cons a (Inst Cons a b)) ;conj
                                              PersistentList$EmptyList      ;empty
                                              a                             ;first
                                              b)                            ;rest
                   ISeq (Inst ISeq a)})

(alter-poly-class PersistentList [[a :variance :covariant]]
                  :replace
                  {Seqable (Inst Seqable (Inst PersistentList a))
                   IPersistentStack (Inst IPersistentStack a (U (Inst PersistentList a)
                                                                PersistentList$EmptyList))
                   IPersistentColection (Inst IPersistentColection 
                                              a                          ;cons obj
                                              (Inst PersistentList a)    ;cons result
                                              PersistentList$EmptyList   ;empty
                                              a                          ;first
                                              (U (Inst PersistentList a)
                                                 (Inst PersistentList$EmptyList)) ;rest
                                              (Inst PersistentList a)    ;next
                                              )
                   })

(alter-poly-class PersistentList$EmptyList [[a :variance :covariant]]
                  :replace
                  {Seqable (Inst Seqable Nil)
                   IPersistentStack (Inst IPersistentStack a Nothing) ;cannot pop
                   IPersistentCollection (Inst IPersistentCollection
                                               a                       ;cons obj
                                               (Inst PersistentList a) ;cons result
                                               PersistentList$EmptyList  ;empty
                                               Nil                   ;first
                                               IPersistentCollection$EmptyList ;rest
                                               Nil) ;next
                   })

(alter-poly-interface IMapEntry [[a :variance :covariant] ;key
                                 [b :variance :covariant] ;value
                                 ]
                      )

;(alter-poly-class MapEntry [[a :variance :covariant] ;key
;                            [b :variance :covariant] ;value
;                            ]
;                  :replace
;                  {IMapEntry (Inst IMapEntry a b)
;                   Associative (Inst Associative 
;                                     (Fn [
;                                          a b -> (Inst PersistentVector b)

(alter-poly-class PersistentHashMap [[a :variance :invariant] ;key
                                     [b :variance :invariant]]
                  :replace
                  {Seqable (Inst Seqable (Inst ASeq (MapEntry a b)))
                   IPersistentMap (Inst IPersistentMap 
                                        (Fn [a -> (Inst IPersistentMap a b)]))  ;dissockey -> dissocresult
                   })

(deftype ConstantPersistentHashMap [keyvals])

(alter-poly-class PersistentVector [[a :variance :covariant]]
                  :replace
                  {Seqable (Inst Seqable (U Nil (Inst PersistentVector$ChunkedSeq a)))
                   IPersistentCollection (Inst IPersistentCollection 
                                               a             ;type to cons
                                               (Inst PersistentVector a) ;cons result
                                               (Inst PersistentVector Nothing) ;empty
                                               a             ;first
                                               (Inst PersistentVector a) ;rest
                                               (Inst PersistentVector a) ;next
                                               )
                   Associative (Inst Associative (Fn [Long a -> (PersistentVector a)])) ;TODO clojure integer type
                   IPersistentStack (Inst IPersistentStack a)
                   IFn (Fn [Long -> a])})

