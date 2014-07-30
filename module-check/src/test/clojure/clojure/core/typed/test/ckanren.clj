(ns clojure.core.typed.test.ckanren
  (:refer-clojure :exclude [==])
  (:use [clojure.walk :only [postwalk]])
  (:require [clojure.set :as set]
            [clojure.core.typed :refer [ann defalias declare-protocols ann-protocol
                                        declare-datatypes
                                        ann-form
                                        tc-ignore check-ns ann-datatype cf
                                        fn> AnyInteger
                                        print-env defprotocol>]
             :as t]
            [clojure.repl :refer [pst]])
  (:import [java.io Writer]
           [clojure.lang Symbol Sequential IPersistentMap APersistentSet Sorted]))

(comment

(ann *occurs-check* boolean)
(ann *reify-vars* boolean)
(ann *locals* (APersistentSet Symbol))
(ann *expand-doms* boolean)

(def ^{:dynamic true} *occurs-check* true)
(def ^{:dynamic true} *reify-vars* true)
(def ^{:dynamic true} *locals*)
(def ^{:dynamic true} *expand-doms*)

;; =============================================================================
;; Core Protocols

;; -----------------------------------------------------------------------------
;; miniKanren Protocols

(defalias Fail false)

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

(defalias Term (t/I IUnifyTerms 
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
              unify-terms [Term Term ISubstitutions -> (U ISubstitutions Fail)])

(ann-protocol IUnifyWithNil
              unify-with-nil [Term nil ISubstitutions -> (U ISubstitutions Fail)])

(ann-protocol IUnifyWithObject
              unify-with-object [Term Object ISubstitutions -> (U ISubstitutions Fail)])

(declare-protocols ILVar)

(ann-protocol IUnifyWithLVar
              unify-with-lvar [Term ILVar ISubstitutions -> (U ISubstitutions Fail)])

(declare-protocols LConsSeq)

(ann-protocol IUnifyWithLSeq
              unify-with-lseq [Term LConsSeq ISubstitutions -> (U ISubstitutions Fail)])

(ann-protocol IUnifyWithSequential
              unify-with-seq [Term Sequential ISubstitutions -> (U ISubstitutions Fail)])

(ann-protocol IUnifyWithMap
              unify-with-map [Term (IPersistentMap Any Any) ISubstitutions -> (U ISubstitutions Fail)])

(ann-protocol IUnifyWithSet
              unify-with-Set [Term (APersistentSet Any) ISubstitutions -> (U ISubstitutions Fail)])

(ann-protocol IReifyTerm
              reify-term [Term ISubstitutions -> ISubstitutions])

(ann-protocol IWalkTerm
              walk-term [Term ISubstitutions -> Term]) ;TODO ?

(ann-protocol IOccursCheckTerm
              occurs-check-term [Term Term Term -> ISubstitutions]) ;TODO ?

(ann-protocol IBuildTerm
              build-term [Term ISubstitutions -> Any])

(ann-protocol IBind
              bind [Term [ISubstitutions -> Any] -> Any])

(ann-protocol IMPlus
              mplus [Term Term -> Any])

(ann-protocol ITake
              take* [Term -> Any])

;TODO ext-check
(ann-protocol ISubstitutions
              walk [ISubstitutions Term -> Term])

(defprotocol> IUnifyTerms
  (unify-terms [u v s]))

(defprotocol> IUnifyWithNil
  (unify-with-nil [v u s]))

(defprotocol> IUnifyWithObject
  (unify-with-object [v u s]))

(defprotocol> IUnifyWithLVar
  (unify-with-lvar [v u s]))

(defprotocol> IUnifyWithLSeq
  (unify-with-lseq [v u s]))

(defprotocol> IUnifyWithSequential
  (unify-with-seq [v u s]))

(defprotocol> IUnifyWithMap
  (unify-with-map [v u s]))

(defprotocol> IUnifyWithSet
  (unify-with-set [v u s]))

(defprotocol> IReifyTerm
  (reify-term [v s]))

(defprotocol> IWalkTerm
  (walk-term [v s]))

(defprotocol> IOccursCheckTerm
  (occurs-check-term [v x s]))

(defprotocol> IBuildTerm
  (build-term [u s]))

(defprotocol> IBind
  (bind [this g]))

(defprotocol> IMPlus
  (mplus [a f]))

(defprotocol> ITake
  (take* [a]))

(defprotocol> ISubstitutions
  (ext-no-check [this x v])
  (walk [this x] [this x wrap?]))

(defprotocol> ISubstitutionsCLP
  (update [this x v]))

;; -----------------------------------------------------------------------------
;; cKanren protocols

(ann-protocol IRefinable
              refinable? [IRefinable -> boolean])

(declare-datatypes FiniteDomain)

(ann-protocol IRefine
              refine [IRefine (U nil FiniteDomain Number) -> (U nil FiniteDomain Number)])

(defprotocol> IUnifyWithRefinable
  (unify-with-refinable [v u s]))

(defprotocol> IUnifyWithInteger
  (unify-with-integer [v u s]))

(defprotocol> IUnifyWithIntervalFD
  (unify-with-interval [v u s]))

(defprotocol> IUnifyWithMultiIntervalFD
  (unify-with-multi-interval [v u s]))

(defprotocol> IRunnable
  (runnable? [c s]))

(defprotocol> IRefinable
  (refinable? [x]))

(defprotocol> IRefine
  (refine [x v]))

(extend-type Object
  IRefinable
  (refinable? [_] false))

;; TODO: think more about update-proc, works for now

(ann-protocol IWithConstraintId
              with-id [IWithConstraintId Any -> Any]) ;FIXME Fn Type?

(defprotocol> IConstraintStore
  (addc [this c])
  (updatec [this c])
  (checkc [this c s])
  (remc [this c])
  (runc [this c])
  (constraints [this x])
  (update-proc [this id proc]))

(defprotocol> IWithConstraintId
  (with-id [this id]))

(extend-type Object
  IWithConstraintId
  (with-id [this id] this))

(ann-protocol IConstraintId
              id [IConstraintId -> Any])

(defprotocol> IConstraintId
  (id [this]))

(extend-type Object
  IConstraintId
  (id [this] nil))

(defprotocol> IStorableConstraint
  (with-proc [this proc])
  (proc [this]))

(defprotocol> IConstraintOp
  (rator [this])
  (rands [this]))

(defprotocol> IRelevant
  (relevant? [this s] [this x s]))

;; TODO: add IRelevantVar ?

(ann-protocol IReifiableConstraint
              reifiable? [IReifiableConstraint -> boolean])

(defprotocol> IReifiableConstraint
  (reifiable? [this])
  (reifyc [this v r]))

(extend-type Object
  IReifiableConstraint
  (reifiable? [this] false))

(ann-protocol IEnforceableConstraint
              enforceable? [IEnforceableConstraint -> boolean])

(defprotocol> IEnforceableConstraint
  (enforceable? [c]))

(extend-type Object
  IEnforceableConstraint
  (enforceable? [x] false))

(ann-protocol INeedsStore
              needs-store? [INeedsStore -> boolean])

(defprotocol> INeedsStore
  (needs-store? [this]))

(extend-type Object
  INeedsStore
  (needs-store? [_] false))

;; TODO: ICLPSet, half the below could be moved into this

(declare-protocols IPair)
(declare-datatypes Pair)

(ann-protocol IInterval
              lb [IInterval -> Number]
              ub [IInterval -> Number]
              bounds [IInterval -> (Pair Number Number)])

(ann-protocol ISortedDomain
              drop-one [ISortedDomain -> (U nil Number FiniteDomain)]
              drop-before [ISortedDomain Number -> FiniteDomain]
              keep-before [ISortedDomain Number -> FiniteDomain])

(defprotocol> IInterval
  (lb [this])
  (ub [this])
  (bounds [this]))

(defprotocol> IIntervals
  (intervals [this]))

(defprotocol> ISortedDomain
  (drop-one [this])
  (drop-before [this n])
  (keep-before [this n]))

(ann-protocol IFiniteDomain
              domain? [IFiniteDomain -> boolean]
              member? [IFiniteDomain Any -> boolean]
              disjoint? [IFiniteDomain Any -> boolean]
              intersects? [IFiniteDomain Any -> boolean]
              subsumes [IFiniteDomain Any -> boolean])

(ann-protocol IIntersection
              intersection [IIntersection (U Number nil FiniteDomain) -> (U Number nil FiniteDomain)])

(defprotocol> IFiniteDomain
  (domain? [this])
  (member? [this that])
  (disjoint? [this that])
  (intersects? [this that])
  (subsumes? [this that]))

(defprotocol> IIntersection
  (intersection [this that]))

(defprotocol> IDifference
  (difference [this that]))

(extend-type Object
  IFiniteDomain
  (domain? [x] false))

(defprotocol> IForceAnswerTerm
  (-force-ans [v x]))

;; =============================================================================
;; Pair

(ann-protocol [[a :variance :covariant] 
                [b :variance :covariant]]
               IPair 
               lhs [(IPair a b) -> a]
               rhs [(IPair a b) -> b])

(defprotocol> IPair
  (lhs [this])
  (rhs [this]))

(ann-datatype [[a :variance :covariant]
               [b :variance :covariant]]
              Pair 
              [[lhs :- a]
               [rhs :- b]]
              :unchecked-ancestors #{(IPair a b) (ExactCount 2)})
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
    (str "(" lhs " . " rhs ")"))
  (equals [_ o]
    (if (instance? Pair o)
      (let [^Pair o o]
        (and (= lhs (.-lhs o))
             (= rhs (.-rhs o))))
      false)))

(ann pair (All [a b] [a b -> (Pair a b)]))
(defn- ^Pair pair [lhs rhs]
  (Pair. lhs rhs))

(tc-ignore
(defmethod print-method Pair [x ^Writer writer]
  (let [^Pair x x]
    (.write writer (str "(" (.lhs x) " . " (.rhs x) ")"))))
  )

;; =============================================================================
;; Constraint Store

(ann-datatype LVar [name :- Symbol
                    hash :- AnyInteger
                    cs :- Any
                    meta :- Any]
              :unchecked-ancestors #{Term})

(ann lvar? (predicate LVar))

(declare lvar? interval multi-interval)

(ann interval-< [IInterval IInterval -> boolean])
(defn interval-< [i j]
  (< (ub i) (lb j)))

(ann interval-> [IInterval IInterval -> boolean])
(defn interval-> [i j]
  (> (lb i) (ub j)))

(declare domain)

(ann domain [Number * -> (U nil Number FiniteDomain)])

(ann-datatype FiniteDomain [s :- (t/I Sorted (APersistentSet Number))
                            min :- Number
                            max :- Number])

(deftype FiniteDomain [s min max]
  IInterval
  (lb [_] min)
  (ub [_] max)
  (bounds [_] (pair min max))
  ISortedDomain
  (drop-one [_]
    (let [s (disj s min)
          c (count s)]
      (cond
       (= c 1) (first s)
       (> c 1) (FiniteDomain. s (first s) max))
       :else nil))
  (drop-before [_ n]
    (apply domain (drop-while (fn> [s* :- Number]
                                 (< s* n))
                              s)))
  (keep-before [this n]
    (apply domain (take-while (fn> [s* :- Number]
                                 (< s* n))
                              s)))
  IRefinable
  (refinable? [_] true)
  IRefine
  (refine [this other] 
    (print-env "refine:")
    (intersection this other))
  IFiniteDomain
  (domain? [_] true)
  (member? [this that]
    (if (integer? that)
      (if (s that)
        true
        false)
      (member? s that)))
  (disjoint? [this that]
    (if (integer? that)
      (not (member? this that))
      (disjoint? this that)))
  IIntersection
  (intersection [this that]
    (if (integer? that)
      (when (member? this that) that)
      (intersection this that)))
  IDifference
  (difference [this that]
    (if (integer? that)
      (domain (disj s that))
      (difference that this)))
  IIntervals
  (intervals [_] (seq s)))

(defn domain [& args]
  (when (seq args)
    (let [s (into (sorted-set) args)]
      (if (> (count s) 1)
        (FiniteDomain. s (first s) (first (rseq s)))
        (first s)))))

(defn sorted-set->dom [s]
  (when (seq s)
    (FiniteDomain. s (first s) (first (rseq s)))))

(declare interval? difference* intersection*)

(defmacro extend-to-fd [t]
  `(extend-type ~t
     IInterval
     (~'lb [this#] this#)
     (~'ub [this#] this#)
     (~'bounds [this#] (pair this# this#))
     ISortedDomain
     (~'drop-one [this#]
       nil)
     (~'drop-before [this# n#]
       (when (>= this# n#)
         this#))
     (~'keep-before [this# n#]
       (when (< this# n#)
         this#))
     IFiniteDomain
     (~'domain? [this#] true)
     (~'member? [this# that#]
       (if (integer? that#)
         (= this# that#)
         (member? that# this#)))
     (~'disjoint? [this# that#]
       (if (integer? that#)
         (not= this# that#)
         (disjoint? that# this#)))
     IIntersection
     (~'intersection [this# that#]
       (cond
        (integer? that#) (when (= this# that#)
                           this#)
        (interval? that#) (intersection that# this#)
        :else (intersection* this# that#)))
     IDifference
     (~'difference [this# that#]
       (cond
        (integer? that#) (if (= this# that#)
                           nil
                           this#)
        (interval? that#) (difference that# this#)
        :else (difference* this# that#)))
     IIntervals
     (~'intervals [this#]
       (list this#))))

(extend-to-fd java.lang.Byte)
(extend-to-fd java.lang.Short)
(extend-to-fd java.lang.Integer)
(extend-to-fd java.lang.Long)
(extend-to-fd java.math.BigInteger)
(extend-to-fd clojure.lang.BigInt)

(declare interval interval?)

(deftype IntervalFD [_lb _ub]
  Object
  (equals [_ o]
    (if (instance? IntervalFD o)
      (let [^IntervalFD o o]
        (and (= _lb (._lb o))
             (= _ub (._ub o))))
      false))
  (toString [this]
    (pr-str this))
  IRefinable
  (refinable? [_] true)
  IRefine
  (refine [this other] (intersection this other))
  IInterval
  (lb [_] _lb)
  (ub [_] _ub)
  (bounds [_] (pair _lb _ub))
  ISortedDomain
  (drop-one [_]
    (let [nlb (inc _lb)]
      (when (<= nlb _ub)
        (interval nlb _ub))))
  (drop-before [this n]
    (cond
     (= n _ub) n
     (< n _lb) this
     (> n _ub) nil
     :else (interval n _ub)))
  (keep-before [this n]
    (cond
     (<= n _lb) nil
     (> n _ub) this
     :else (interval _lb (dec n))))
  IFiniteDomain
  (domain? [_] true)
  (member? [this that]
    (cond
     (integer? that)
     (and (>= that _lb) (<= that _ub))
     
     (interval? that)
     (let [i this
           j that
           [imin imax] (bounds i)
           [jmin jmax] (bounds j)]
       (and (>= jmin imin)
            (<= jmax imax)))
     
     :else (member? that this)))
  (disjoint? [this that]
    (cond
     (integer? that)
     (not (member? this that))

     (interval? that)
     (let [i this
           j that
           [imin imax] (bounds i)
           [jmin jmax] (bounds j)]
       (or (> imin jmax)
           (< imax jmin)))

     :else (disjoint? that this)))
  IIntersection
  (intersection [this that]
    (cond
     (integer? that)
     (if (member? this that)
       that
       nil)

     (interval? that)
     (let [i this j that
           imin (lb i) imax (ub i)
           jmin (lb j) jmax (ub j)]
       (cond
        (< imax jmin) nil
        (< jmax imin) nil
        (and (<= imin jmin)
             (>= imax jmax)) j
        (and (<= jmin imin)
             (>= jmax imax)) i
        (and (<= imin jmin)
             (<= imax jmax)) (interval jmin imax)
        (and (<= jmin imin)
             (<= jmax imax)) (interval imin jmax)
        :else (throw (Error. (str "Interval intersection not defined " i " " j)))))

     :else (intersection* this that)))
  IDifference
  (difference [this that]
    (cond
     (integer? that)
     (cond
      (= _lb that) (interval (inc _lb) _ub)
      (= _ub that) (interval _lb (dec _ub))
      :else (if (member? this that)
              (multi-interval (interval _lb (dec that))
                              (interval (inc that) _ub))
              this))
     
     (interval? that)
     (let [i this j that
           imin (lb i) imax (ub i)
           jmin (lb j) jmax (ub j)]
       (cond
        (> jmin imax) i
        (and (<= jmin imin)
             (>= jmax imax)) nil
        (and (< imin jmin)
             (> imax jmax)) (multi-interval (interval imin (dec jmin))
             (interval (inc jmax) imax))
        (and (< imin jmin)
             (<= jmin imax)) (interval imin (dec jmin))
        (and (> imax jmax)
             (<= jmin imin)) (interval (inc jmax) imax)
        :else (throw (Error. (str "Interval difference not defined " i " " j)))))
     
     :else (difference* this that)))
  IIntervals
  (intervals [this]
    (list this)))

(defn interval? [x]
  (instance? IntervalFD x))

(defmethod print-method IntervalFD [x ^Writer writer]
  (.write writer (str "<interval:" (lb x) ".." (ub x) ">")))

(defn ^IntervalFD interval
  ([ub] (IntervalFD. 0 ub))
  ([lb ub]
     (if (zero? (- ub lb))
       ub
       (IntervalFD. lb ub))))

(defn intersection* [is js]
  (loop [is (seq (intervals is)) js (seq (intervals js)) r []]
    (if (and is js)
      (let [i (first is)
            j (first js)]
        (cond
         (interval-< i j) (recur (next is) js r)
         (interval-> i j) (recur is (next js) r)
         :else
         (let [[imin imax] (bounds i)
               [jmin jmax] (bounds j)]
           (cond
            (<= imin jmin)
            (cond
             (< imax jmax)
             (recur (next is)
                    (cons (interval (inc imax) jmax) (next js))
                    (conj r (interval jmin imax)))
             (> imax jmax)
             (recur (cons (interval (inc jmax) imax) (next is))
                    (next js)
                    (conj r j))
             :else
             (recur (next is) (next js)
                    (conj r (interval jmin jmax))))
            (> imin jmin)
            (cond
             (> imax jmax)
             (recur (cons (interval (inc jmax) imax) (next is))
                    (next js)
                    (conj r (interval imin jmax)))
             (< imax jmax)
             (recur is (cons (interval (inc imax) jmax) (next js))
                    (conj r i))
             :else
             (recur (next is) (next js)
                    (conj r (interval imin imax))))))))
      (apply multi-interval r))))

(defn difference* [is js]
    (loop [is (seq (intervals is)) js (seq (intervals js)) r []]
      (if is
        (if js
          (let [i (first is)
                j (first js)]
            (cond
             (interval-< i j) (recur (next is) js (conj r i))
             (interval-> i j) (recur is (next js) r)
             :else
             (let [[imin imax] (bounds i)
                   [jmin jmax] (bounds j)]
               (cond
                (< imin jmin)
                (cond
                 (< jmax imax)
                 (recur (cons (interval (inc jmax) imax) (next is))
                        (next js)
                        (conj r (interval imin (dec jmin))))
                 (> jmax imax)
                 (recur (next is)
                        (cons (interval (inc imax) jmax) (next js))
                        (conj r (interval imin (dec jmin))))
                 :else
                 (recur (next is) (next js)
                        (conj r (interval imin (dec jmin)))))
                (>= imin jmin)
                (cond
                 (< imax jmax)
                 (recur (next is)
                        (cons (interval (inc imax) jmax) (next js))
                        r)
                 (> imax jmax)
                 (recur (cons (interval (inc jmax) imax) (next is))
                        (next js)
                        r)
                 :else (recur (next is) (next js)
                              r))))))
          (apply multi-interval (into r is)))
        (apply multi-interval r))))

(declare normalize-intervals singleton-dom? multi-interval)

(deftype MultiIntervalFD [min max is]
  Object
  (equals [this j]
    (if (instance? MultiIntervalFD j)
      (let [i this
            [jmin jmax] (bounds j)]
        (if (and (= min jmin) (= max jmax))
          (let [is (normalize-intervals is)
                js (normalize-intervals (intervals j))]
            (= is js))
          false))
      false))
  IRefinable
  (refinable? [_] true)
  IRefine
  (refine [this other] (intersection this other))
  IInterval
  (lb [_] min)
  (ub [_] max)
  (bounds [_] (pair min max))
  ISortedDomain
  (drop-one [_]
    (let [i (first is)]
      (if (singleton-dom? i)
        (let [nis (rest is)]
          (MultiIntervalFD. (lb (first nis)) max nis))
        (let [ni (drop-one i)]
          (MultiIntervalFD. (lb ni) max (cons ni (rest is)))))))
  (drop-before [_ n]
    (let [is (seq is)]
      (loop [is is r []]
        (if is
          (let [i (drop-before (first is) n)]
            (if i
              (recur (next is) (conj r i))
              (recur (next is) r)))
          (when (pos? (count r))
            (apply multi-interval r))))))
  (keep-before [_ n]
    (let [is (seq is)]
      (loop [is is r []]
        (if is
          (let [i (keep-before (first is) n)]
            (if i
              (recur (next is) (conj r i))
              (recur (next is) r)))
          (when (pos? (count r))
            (apply multi-interval r))))))
  IFiniteDomain
  (domain? [_] true)
  (member? [this that]
    (if (disjoint? (interval (lb this) (ub this))
                   (interval (lb that) (ub that)))
    false
    (let [d0 (intervals this)
          d1 (intervals that)]
      (loop [d0 d0 d1 d1 r []]
        (if (nil? d0)
          true)))))
  (disjoint? [this that]
    (if (disjoint? (interval (lb this) (ub this))
                   (interval (lb that) (ub that)))
      true
      (let [d0 (intervals this)
            d1 (intervals that)]
        (loop [d0 d0 d1 d1]
          (if (nil? d0)
            true
            (let [i (first d0)
                  j (first d1)]
              (cond
               (or (interval-< i j) (disjoint? i j)) (recur (next d0) d1)
               (interval-> i j) (recur d0 (next d1))
               :else false)))))))
  IIntersection
  (intersection [this that]
    (intersection* this that))
  IDifference
  (difference [this that]
    (difference* this that))
  IIntervals
  (intervals [this]
    (seq is)))

;; union where possible
(defn normalize-intervals [is]
  (reduce (fn [r i]
            (if (zero? (count r))
              (conj r i)
              (let [j (peek r)
                    jmax (ub j)
                    imin (lb i)]
                (if (<= (dec imin) jmax)
                  (conj (pop r) (interval (lb j) (ub i)))
                  (conj r i)))))
          [] is))

(defn multi-interval
  ([] nil)
  ([i0] i0)
  ([i0 i1]
     (let [is [i0 i1]]
       (MultiIntervalFD. (reduce min (map lb is)) (reduce max (map ub is)) is)))
  ([i0 i1 & ir]
     (let [is (into [] (concat (list i0 i1) ir))]
       (MultiIntervalFD. (reduce min (map lb is)) (reduce max (map ub is)) is))))

(defmethod print-method MultiIntervalFD [x ^Writer writer]
  (.write writer (str "<intervals:" (apply pr-str (.is x)) ">")))

(defn var-rands [c]
  (into [] (filter lvar? (flatten (rands c)))))

(defn vars-to-remove [c vs s]
  (let [purge (atom true)
        vs (doall
            (filter (fn [x]
                      (when (lvar? x)
                        (let [remove? (not (relevant? c x s))]
                          (when-not remove?
                            (reset! purge false))
                          remove?)))
                    (flatten vs)))]
    (pair @purge vs)))

;; TODO: map interface stinks, but a collection might be OK?
;; conj, disj?

(deftype ConstraintStore [km cm cid running]
  IConstraintStore
  (addc [this c]
    (let [vars (var-rands c)
          c (with-id c cid)
          ^ConstraintStore cs (reduce (fn [cs v] (assoc cs v c)) this vars)]
      (ConstraintStore. (.km cs) (.cm cs) (inc cid) running)))
  (updatec [this c]
    (ConstraintStore. km (assoc cm (id c) c) cid running))
  (checkc [this c s]
    (let [ocid (id c)
          oc (get cm ocid)]
      ;; the constraint may no longer be in the store
      (if oc
        (let [[purge? vs] (vars-to-remove oc (rands c) s)
              nkm (reduce (fn [m v]
                            (let [kcs (disj (get m v) ocid)]
                              (if (empty? kcs)
                                (dissoc m v)
                                (assoc m v kcs))))
                          km vs)
              ncm (if purge?
                    (dissoc cm ocid)
                    cm)]
          (ConstraintStore. nkm ncm cid (disj running ocid)))
        (ConstraintStore. km cm cid (disj running ocid)))))
  (remc [this c]
    (let [vs (var-rands c)
          ocid (id c)
          nkm (reduce (fn [km v]
                        (let [vcs (disj (get km v) ocid)]
                          (if (empty? vcs)
                            (dissoc km v)
                            (assoc km v vcs))))
                      km vs)
          ncm (dissoc cm ocid)]
      (ConstraintStore. nkm ncm cid running)))
  (runc [this c]
    (ConstraintStore. km cm cid (conj running (id c))))
  (constraints [this x]
    (map cm (get km x)))
  (update-proc [this id proc]
    (let [ncm (assoc cm id (with-proc (get cm id) proc))]
      (ConstraintStore. km ncm cid running)))
  clojure.lang.Counted
  (count [this]
    (count cm))
  ;; TODO: do no expose a map interface
  clojure.lang.Associative
  (assoc [this k v]
    (when-not (lvar? k)
      (throw (Error. (str "constraint store assoc expected logic var key: " k))))
    (let [nkm (update-in km [k] (fnil (fn [s] (conj s cid)) #{}))
          ncm (assoc cm cid v)]
      (ConstraintStore. nkm ncm cid running)))
  clojure.lang.ILookup
  (valAt [this k]
    (when-let [ids (get km k)]
      (map cm (remove running ids))))
  (valAt [this k not-found]
    (if-let [v (.valAt this k)]
      v
      not-found)))

(defn ^ConstraintStore make-cs []
  (ConstraintStore.
   clojure.lang.PersistentHashMap/EMPTY
   clojure.lang.PersistentHashMap/EMPTY 0
   #{}))










) ;comment


(comment
  (cf (ann-form (pair 1 2) (Pair Number Number)))

  (cf (first [1]))
  (clojure.core.typed/cs-gen '#{} {'G__65606 no-bounds} '{} 
                             (->HeterogeneousVector [(-val 1)])
                             (In (RClass-of Seqable [(make-F 'G__65606)]) (make-CountRange 1)))
  )
