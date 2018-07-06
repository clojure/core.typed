(ns ^:skip-wiki clojure.core.typed.bootstrap-cljs
  (:require [clojure.set :as set]))

(def -base-aliases
  '#{AnyInteger Integer Int Seqable NonEmptySeq Number String Boolean Seq Str
     EmptySeqable NonEmptySeqable Option Coll NonEmptyColl NonEmptyASeq NonEmptyAVec
     EmptyCount NonEmptyCount Vec Nilable AVec NilableNonEmptyASeq PersistentList Collection Set Stack Reversible
     IPersistentSet IPersistentVector IPersistentMap APersistentMap Associative Map Atom1 Atom2 Sequential
     Num})

(def -specials 
  '#{All U Any Pred ReadOnlyArray
     Array IFn TFn I HSequential HSeq HSet HMap Val Value CountRange
     ExactCount Difference Rec Assoc Get HVec JSUndefined JSNull
     Nothing JSNumber JSBoolean JSString JSSymbol JSObject CLJSInteger
     JSObj})

(let [i (set/intersection -base-aliases -specials)]
  (assert (empty? i) (pr-str i)))

(defmacro base-aliases 
  "Define base aliases"
  []
  `(do ~@(map #(list 'def %) (concat -base-aliases -specials))))
