(ns ^:skip-wiki clojure.core.typed.bootstrap-cljs
  (:require [clojure.set :as set]))

(alter-meta! *ns* assoc :skip-wiki true)

(def -base-aliases
  '#{AnyInteger Seqable NonEmptySeq Number
     EmptySeqable NonEmptySeqable Option Coll NonEmptyColl NonEmptyASeq})

(def -specials
  '#{All U Any Pred Int Bool Num Str Object ReadOnlyArray
     Array IFn TFn I HSequential})

(assert (empty? (set/intersection -base-aliases -specials)))

(defmacro base-aliases
  "Define base aliases"
  []
  `(do ~@(map #(list 'def %) (concat -base-aliases -specials))))
