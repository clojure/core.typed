(ns typed.test.core-logic
  (:refer-clojure :exclude [==])
  (:use [clojure.walk :only [postwalk]])
  (:import [java.io Writer]
           [clojure.lang IPersistentSet Symbol IPersistentMap Seqable
            IPersistentVector IPersistentList Sequential])
  (:require [clojure.set :as set]
            [clojure.repl :refer [pst]]
            [typed.core :refer [ann-protocol ann tc-ignore def-alias
                                declare-protocols declare-datatypes
                                ann-datatype loop> check-ns non-nil-return
                                tc-pr-env]]
            [analyze.core :refer [ast]]))

(ann *occurs-check* (U true false))
(ann *reify-vars* (U true false))
(ann *locals* (IPersistentSet Symbol))

(def ^{:dynamic true} *occurs-check* true)
(def ^{:dynamic true} *reify-vars* true)
(def ^{:dynamic true} *locals*)

(def-alias Fail false)

(declare-protocols ISubstitutions
                   IUnifyTerms 
                   IUnifyWithNil
                   IUnifyWithObject
                   IUnifyWithLVar
                   IUnifyWithSequential
                   IUnifyWithMap
                   IUnifyWithSet
                   IReifyTerm
                   IWalkTerm
                   IOccursCheckTerm
                   IBuildTerm)

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
                   IBuildTerm))

(ann-protocol IUnifyTerms
              :methods
              {unify-terms [Term Term ISubstitutions -> (U ISubstitutions Fail)]})

(ann-protocol IUnifyWithNil
              :methods
              {unify-with-nil [Term nil ISubstitutions -> (U ISubstitutions Fail)]})

(ann-protocol IUnifyWithObject
              :methods
              {unify-with-object [Term Object ISubstitutions -> (U ISubstitutions Fail)]})

(declare-protocols LVar)

(ann-protocol IUnifyWithLVar
              :methods
              {unify-with-lvar [Term LVar ISubstitutions -> (U ISubstitutions Fail)]})

(declare-protocols LConsSeq)

(ann-protocol IUnifyWithLSeq
              :methods
              {unify-with-lseq [Term LConsSeq ISubstitutions -> (U ISubstitutions Fail)]})

(ann-protocol IUnifyWithSequential
              :methods
              {unify-with-seq [Term Sequential ISubstitutions -> (U ISubstitutions Fail)]})

(ann-protocol IUnifyWithMap
              :methods
              {unify-with-map [Term IPersistentMap ISubstitutions -> (U ISubstitutions Fail)]})

(ann-protocol IUnifyWithSet
              :methods
              {unify-with-Set [Term IPersistentSet ISubstitutions -> (U ISubstitutions Fail)]})

(ann-protocol IReifyTerm
              :methods
              {reify-term [Term ISubstitutions -> ISubstitutions]})

(ann-protocol IWalkTerm
              :methods
              {walk-term [Term ISubstitutions -> Term]}) ;TODO ?

(ann-protocol IOccursCheckTerm
              :methods
              {occurs-check-term [Term Term Term -> ISubstitutions]}) ;TODO ?

(ann-protocol IBuildTerm
              :methods
              {build-term [Term ISubstitutions -> Any]})

(ann-protocol IBind
              :methods
              {bind [Term [ISubstitutions -> Any] -> Any]})

(ann-protocol IMPlus
              :methods
              {mplus [Term Term -> Any]})

(ann-protocol ITake
              :methods
              {take* [Term -> Any]})

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

(ann-datatype Unbound [])
(deftype Unbound [])

(ann unbound Unbound)
(def ^Unbound unbound (Unbound.))

(ann Unbound? (predicate Unbound))
(tc-ignore
(defn Unbound? [a]
  (identical? a unbound))
)

(ann-protocol ILVar
              :methods
              {constraints [ILVar -> Any]
               add-constraint [ILVar Any -> Any]
               add-constraints [ILVar Any -> Any]
               remove-constraint [ILVar Any -> Any]
               remove-constraints [ILVar -> Any]})

(tc-ignore
(defprotocol ILVar
  (constraints [this])
  (add-constraint [this c])
  (add-constraints [this ds])
  (remove-constraint [this c])
  (remove-constraints [this]))
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
               walk-var [ISubstitutions Term -> Term]
               walk [ISubstitutions Term -> Term]
               walk* [ISubstitutions Term -> Term]
               unify [ISubstitutions Term Term -> (U ISubstitutions Fail)]
               update [ISubstitutions Term Term -> Term] ;return?
               reify-lvar-name [ISubstitutions -> Symbol]
               -reify* [ISubstitutions Term -> ISubstitutions]
               -reify [ISubstitutions Term -> ISubstitutions]
               build [ISubstitutions Term -> ISubstitutions]})

(tc-ignore
(defprotocol ISubstitutions
  (length [this])
  (occurs-check [this u v])
  (ext [this u v])
  (ext-no-check [this u v])
  (swap [this cu])
  (constrain [this u c])
  (get-var [this v])
  (use-verify [this f])
  (walk [this v])
  (walk-var [this v])
  (walk* [this v])
  (unify [this u v])
  (reify-lvar-name [_])
  (-reify* [this v])
  (-reify [this v])
  (build [this u]))
)

(declare-datatypes Substitutions)

(ann empty-s Substitutions)
(declare empty-s)

(declare-datatypes Choice)

;TODO
(ann choice [Any [Any -> Any] -> Choice])
(declare choice)

(declare-datatypes LVar)

(ann lvar (Fn [-> LVar]
              [Symbol -> LVar]
              [Symbol Any -> LVar])) ;TODO second arg is a cs
(declare lvar)

;TODO filters
(ann lvar? (Fn [Any -> (U false true)]))
(declare lvar?)

(declare pair)
(declare lcons)

(ann-datatype Substitutions [[s :- (IPersistentMap ILVar (U Unbound Term))]
                             [l :- (IPersistentList Pair)] ;[l :- (IPersistentList (Pair LVar Term))]
                             [verify :- [ISubstitutions Term Term -> ISubstitutions]]
                             [cs :- Any]] ;TODO constraint store
              )

(deftype Substitutions [s l verify cs]
  Object
  (equals [this o]
    (or (identical? this o)
        (and (.. this getClass (isInstance o))
             (= s ^clojure.lang.PersistentHashMap (.s ^Substitutions o)))))
  (toString [_]
    (prn-str [s l verify cs]))

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
    (verify this u v))

  (swap [this cu]
    (if (contains? s cu)
      (let [v (s cu)]
        (Substitutions. (-> s (dissoc cu) (assoc cu v)) l verify cs))
      (Substitutions. (assoc s cu unbound) l verify cs)))

  (constrain [this u c]
    (let [u (walk this u)]
      (swap this (add-constraint u c))))

  (get-var [this v]
    (first (find s v)))

  (use-verify [this f]
    (Substitutions. s l f cs))
  
  ;Need equality filters for this to type check.
;  (walk [this v]
;    (loop> [[lv :- Term] v
;            [[v vp] :- (U nil (Vector* ILVar (U Unbound Term)))] (find s v)]
;      (cond
;       (nil? v) lv
;       ;created predicate for singleton type
;       (Unbound? vp) v
;       (not (lvar? vp)) vp
;       :else (recur vp (find s vp)))))
  (walk [this v]
    (loop> [[lv :- Term] v
            [fr :- (U nil (Vector* ILVar (U Unbound Term)))] (find s v)]
      (let [v (nth fr 0)
            vp (nth fr 1)]
        (cond
          (nil? v) lv
          ;created predicate for singleton type
          (Unbound? vp) v
          (not (lvar? vp)) vp
          :else (recur vp (find s vp))))))
  
  ;walk-var same as walk above...
  (walk-var [this v]
    (tc-pr-env "before loop")
    (loop> [[lv :- Term] v 
            [fr :- (U nil (Vector* ILVar (U Unbound Term)))] (find s v)]
      (tc-pr-env "before nths")
      (let [v (nth fr 0)
            vp (nth fr 1)]
        (tc-pr-env "after nths")
        (cond
          (nil? v) lv
          (Unbound? vp) (do (tc-pr-env "unbound branch")
                          v)
          (not (lvar? vp)) v
          :else (recur vp (find s vp))))))
  
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

(defn- ^Substitutions pass-verify [^Substitutions s u v]
  (Substitutions. (assoc (.s s) u v)
                  (cons (pair u v) (.l s))
                  (.verify s)
                  (.cs s)))

(ann make-s (Fn [-> Substitutions]
                [(IPersistentMap Term Term) -> Substitutions]
                [(IPersistentMap Term Term) (Seqable Any) -> Substitutions]
                [(IPersistentMap Term Term) (Seqable Any) (IPersistentVector Any) -> Substitutions]))
(defn- ^Substitutions make-s
  ([m l] (Substitutions. m l pass-verify nil))
  ([m l f] (Substitutions. m l f nil))
  ([m l f cs] (Substitutions. m l f cs)))

(def ^Substitutions empty-s (make-s {} '()))

(defn- subst? [x]
  (instance? Substitutions x))

(defn ^Substitutions to-s [v]
  (let [s (reduce (fn [m [k v]] (assoc m k v)) {} v)
        l (reduce (fn [l [k v]] (cons (Pair. k v) l)) '() v)]
    (make-s s l)))
