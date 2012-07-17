(ns typed.test.core-logic
  (:refer-clojure :exclude [==])
  (:use [clojure.walk :only [postwalk]])
  (:require [clojure.set :as set])
  (:import [java.io Writer]
           [clojure.lang IPersistentSet Symbol IPersistentMap Seqable
            IPersistentVector])
  (:require [typed.core :refer [ann-protocol ann tc-ignore declare-datatypes def-alias
                                declare-protocols ann-datatype]]))

(ann *occurs-check* (U true false))
(ann *locals* (IPersistentSet Symbol))

(def ^{:dynamic true} *occurs-check* true)
(def ^{:dynamic true} *locals*)

(def-alias Fail false)

(declare-datatypes ISubstitution)
(declare-protocols IUnifyTerms 
                   IUnifyWithNil
                   IUnifyWithObject
                   IUnifyWithLVar
                   IUnifyWithSequential
                   IUnifyWithMap
                   IUnifyWithSet
                   IReifyTerm
                   IWalkTerm
                   IOccursCheckTerm
                   IBuildTerm
                   IBind
                   IMPlus
                   ITake)

(def-alias Term (I IUnifyTerms 
                   IUnifyWithNil
                   IUnifyWithObject
                   IUnifyWithLVar
                   IUnifyWithSequential
                   IUnifyWithMap
                   IUnifyWithSet
                   IReifyTerm
                   IWalkTerm
                   IOccursCheckTerm
                   IBuildTerm
                   IBind
                   IMPlus
                   ITake))

(ann-protocol IUnifyTerms
              :methods
              {unify-terms [Term Term ISubstitution -> (U ISubstitution Fail)]})

(ann-protocol IBind
              :methods
              {bind [Term [ISubstitution -> Any] -> Any]})

(ann-protocol IMPlus
              :methods
              {mplus [Term Term -> Any]})

(ann-protocol ITake
              :methods
              {take* [Term -> Any]})

(ann-protocol IBuildTerm
              :methods
              {build-term [Term ISubstitution -> Any]})

(tc-ignore
(defprotocol IUnifyTerms
  (unify-terms [u v s]))

(defprotocol IUnifyWithNil
  (unify-with-nil [v u s]))

(defprotocol IUnifyWithObject
  (unify-with-object [v u s]))

(defprotocol IUnifyWithLVar
  (unify-with-lvar [v u s]))

(defprotocol IUnifyWithLSeq
  (unify-with-lseq [v u s]))

(defprotocol IUnifyWithSequential
  (unify-with-seq [v u s]))

(defprotocol IUnifyWithMap
  (unify-with-map [v u s]))

(defprotocol IUnifyWithSet
  (unify-with-set [v u s]))

(defprotocol IReifyTerm
  (reify-term [v s]))

(defprotocol IWalkTerm
  (walk-term [v s]))

(defprotocol IOccursCheckTerm
  (occurs-check-term [v x s]))

(defprotocol IBuildTerm
  (build-term [u s]))

(defprotocol IBind
  (bind [this g]))

(defprotocol IMPlus
  (mplus [a f]))

(defprotocol ITake
  (take* [a]))
)

;; =============================================================================
;; Pair

(ann-protocol IPair
              :methods
              {lhs [IPair -> Any]
               rhs [IPair -> Any]})

(tc-ignore
(defprotocol IPair
  (lhs [this])
  (rhs [this]))
)

(ann-datatype Pair [[lhs :- Term]
                    [rhs :- Term]])

(deftype Pair [lhs rhs]
  clojure.lang.Counted
  (count [_] 2)
  clojure.lang.Indexed
  (nth [_ i] (case i
                   0 lhs
                   1 rhs
                   (throw (IndexOutOfBoundsException.))))
  (nth [_ i not-found] (case i
                             0 lhs
                             1 rhs
                             not-found))
  IPair
  (lhs [_] lhs)
  (rhs [_] rhs)
  java.util.Map$Entry
  (getKey [_] lhs)
  (getValue [_] rhs)
  Object
  (toString [_]
    (str "(" lhs " . " rhs ")")))

(ann pair [Term Term -> Pair])
(defn- ^Pair pair [lhs rhs]
  (Pair. lhs rhs))

;; =============================================================================
;; Substitutions

(ann-protocol ISubstitutions
              :methods
              {length [ISubstitutions -> Number]
               occurs-check [ISubstitutions Term Term -> (U true false)]
               ext [ISubstitutions Term Term -> (U nil ISubstitutions)]
               ext-no-check [ISubstitutions Term Term -> ISubstitutions]
               walk [ISubstitutions Term -> Term]
               walk* [ISubstitutions Term -> Term]
               unify [ISubstitutions Term Term -> (U ISubstitutions Fail)]
               update [ISubstitutions Term Term -> Term] ;return?
               reify-lvar-name [ISubstitutions -> Symbol]
               -reify* [ISubstitutions Term -> Symbol]
               -reify [ISubstitutions Term -> Any]
               build [ISubstitutions Term -> Any]})

(tc-ignore
(defprotocol ISubstitutions
  (length [this])
  (occurs-check [this u v])
  (ext [this u v])
  (ext-no-check [this u v])
  (walk [this v])
  (walk* [this v])
  (unify [this u v])
  (update [this x v])
  (reify-lvar-name [_])
  (-reify* [this v])
  (-reify [this v])
  (build [this u]))
)

(declare empty-s)
(declare choice)
(declare lvar)
(declare lvar?)
(declare pair)
(declare lcons)
(declare run-constraints)

(ann-datatype Substitutions [[s :- (IPersistentMap Term Term)]
                             [l :- (Seqable Any)]
                             [c :- (IPersistentVector Any)]]
              :ancestors
              #{ISubstitutions IBind IMPlus ITake})

(deftype Substitutions [s l c]
  Object
  (equals [this o]
    (or (identical? this o)
        (and (.. this getClass (isInstance o))
             (= s ^clojure.lang.PersistentHashMap (.s ^Substitutions o)))))

  ISubstitutions
  (length [this] (count s))

  (occurs-check [this u v]
    (let [v (walk this v)]
      (occurs-check-term v u this)))
  
  (ext [this u v]
    (if (and *occurs-check* (occurs-check this u v))
      nil
      (ext-no-check this u v)))

  (ext-no-check [this u v]
    (Substitutions. (assoc s u v)
                    (cons (pair u v) l)
                    c))

  (walk [this v]
    (loop [lv v [v vp] (find s v)]
      (cond
       (nil? v) lv
       (not (lvar? vp)) vp
       :else (recur vp (find s vp)))))
  
  (walk* [this v]
    (let [v (walk this v)]
      (walk-term v this)))

  (unify [this u v]
    (if (identical? u v)
      this
      (let [u (walk this u)
            v (walk this v)]
        (if (identical? u v)
          this
          (unify-terms u v this)))))

  (update [this x v]
    (let [sp (ext this x v)]
      ((run-constraints (if (var? v) #{x v} #{x}))
       (Substitutions. sp l c))))

  (reify-lvar-name [this]
    (symbol (str "_." (count s))))

  (-reify* [this v]
    (let [v (walk this v)]
      (reify-term v this)))

  (-reify [this v]
    (let [v (walk* this v)]
      (walk* (-reify* empty-s v) v)))

  (build [this u]
    (build-term u this))

  IBind
  (bind [this g]
    (g this))
  IMPlus
  (mplus [this f]
    (choice this f))
  ITake
  (take* [this] this))

(ann make-s (Fn [-> Substitutions]
                [(IPersistentMap Term Term) -> Substitutions]
                [(IPersistentMap Term Term) (Seqable Any) -> Substitutions]
                [(IPersistentMap Term Term) (Seqable Any) (IPersistentVector Any) -> Substitutions]))
(defn- ^Substitutions make-s
  ([] (Substitutions. {} () []))
  ([m] (Substitutions. m () []))
  ([m l] (Substitutions. m l []))
  ([m l c] (Substitutions. m l c)))

(ann empty-s Substitutions)
(def ^Substitutions empty-s (make-s))

(ann empty-f [-> nil])
(def empty-f (fn []))

;TODO filters
(ann subst? [Any -> (U true false)])
(defn- subst? [x]
  (instance? Substitutions x))

(defn ^Substitutions to-s [v]
  (let [s (reduce (fn [m [k v]] (assoc m k v)) {} v)
        l (reduce (fn [l [k v]] (cons (Pair. k v) l)) '() v)]
    (make-s s l [])))
