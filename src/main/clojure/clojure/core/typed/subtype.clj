(ns ^:skip-wiki clojure.core.typed.subtype
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.type-protocols :as tp]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.filter-rep :as fr]
            [clojure.core.typed.filter-ops :as fops]
            [clojure.core.typed.object-rep :as orep]
            [clojure.core.typed.frees :as frees]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.datatype-ancestor-env :as ancest]
            [clojure.core.typed.analyze-cljs :as cljs-util]
            [clojure.set :as set]
            [clojure.repl :as repl])
  (:import (clojure.core.typed.type_rep Poly TApp Union Intersection Value Function
                                        Result Protocol TypeFn Name F Bounds
                                        PrimitiveArray DataType RClass HeterogeneousMap
                                        HeterogeneousList HeterogeneousSeq CountRange KwArgs
                                        Extends)
           (clojure.core.typed.filter_rep FilterSet)
           (clojure.lang APersistentMap APersistentVector PersistentList ASeq Seqable)))

(alter-meta! *ns* assoc :skip-wiki true)

;(def-alias Seen Any)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtype

(defmacro handle-failure [& body]
  `(u/handle-subtype-failure ~@body))

;[Type Type -> Nothing]
(defn fail! [s t]
  (throw u/subtype-exn))

;keeps track of currently seen subtype relations for recursive types.
;(Set [Type Type])
(defonce ^:dynamic *sub-current-seen* #{})

(defn currently-subtyping? []
  (boolean (seq *sub-current-seen*)))

(declare subtypes*-varargs)

;[(Seqable Type) (Seqable Type) Type -> Boolean]
(defn subtypes-varargs?
  "True if argtys are under dom"
  [argtys dom rst]
  (handle-failure
    (subtypes*-varargs #{} argtys dom rst)
    true))

;subtype and subtype? use *sub-current-seen* for remembering types (for Rec)
;subtypeA* takes an extra argument (the current-seen subtypes), called by subtype
;
; In short, only call subtype (or subtype?)

(declare subtype)

(defonce subtype-cache (atom {}))

; (ann reset-subtype-cache [-> nil])
(defn reset-subtype-cache []
  (reset! subtype-cache {})
  nil)

;[Type Type -> Boolean]
(defn subtype? [s t]
  {:post [(u/boolean? %)]}
  (letfn [(do-subtype []
            (u/p :subtype/subtype?
               (boolean
                 (handle-failure
                   (subtype s t)))))]
    (if-let [[_ res] (u/p :subtype-cache-lookup (find @subtype-cache [(hash s) (hash t)]))]
      (u/p :subtype-cache-hit 
       res)
      (let [_ (u/p :subtype-cache-miss)
            res (do-subtype)]
        (when-not (currently-subtyping?)
          (swap! subtype-cache assoc [(hash s) (hash t)] res))
        res))))

(declare subtypeA*)

;[(IPersistentSet '[Type Type]) Type Type -> Boolean]
(defn subtypeA*? [A s t]
  (handle-failure
    (subtypeA* A s t)))

(declare supertype-of-one-arr)

(defn infer-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.cs-gen) 'infer)]
    (assert (var? v) "infer unbound")
    v))

;[(IPersistentMap Symbol Bounds) (Seqable Type) (Seqable Type)
;  -> Boolean]
(defn unify [X S T]
  (u/p :subtype/unify
  (let [infer @(infer-var)]
    (boolean 
      (u/handle-cs-gen-failure
        (infer X {} S T r/-any))))))

(declare subtype-TApp? protocol-extenders
         subtype-datatypes-or-records subtype-Result subtype-PrimitiveArray
         subtype-CountRange subtype-TypeFn subtype-RClass
         subtype-datatype-and-protocol subtype-rclass-protocol
         boxed-primitives subtype-datatype-rclass)

(defn simplify-In [t]
  {:pre [(r/Intersection? t)]}
  (let [mi (apply c/In (:types t))]
    (if (r/Intersection? mi)
      (:types mi)
      [mi])))

;TODO replace hardcoding cases for unfolding Mu? etc. with a single case for unresolved types.
;[(IPersistentSet '[Type Type]) Type Type -> (IPersistentSet '[Type Type])]
(defn subtypeA* [A s t]
  {:pre [(r/AnyType? s)
         (r/AnyType? t)]
   :post [(set? %)]}
  (if (or (u/p :subtype/query-current-seen
            (contains? A [s t]))
          (= s t)
          ; FIXME TypeFn's probably are not between Top/Bottom
          (r/Top? t)
          (r/Bottom? s)
          ;TCError is top and bottom
          (some r/TCError? [s t]))
    A
    (binding [*sub-current-seen* (u/p :subtype/extend-current-seen (conj A [s t]))]
      (cond
        (or (r/TCResult? s)
            (r/TCResult? t))
        (assert nil "Cannot give TCResult to subtype")

        (and (r/Value? s)
             (r/Value? t))
        ;already (not= s t)
        (fail! s t)

        (r/Name? s)
        (subtypeA* *sub-current-seen* (c/resolve-Name s) t)

        (r/Name? t)
        (subtypeA* *sub-current-seen* s (c/resolve-Name t))

        (and (r/Poly? s)
             (r/Poly? t)
             (= (:nbound s) (:nbound t)))
        (let [;instantiate both sides with the same fresh variables
              names (repeatedly (:nbound s) gensym)
              bbnds1 (c/Poly-bbnds* names s)
              bbnds2 (c/Poly-bbnds* names t)
              b1 (c/Poly-body* names s)
              b2 (c/Poly-body* names t)]
          (if (and (= bbnds1 bbnds2)
                   (free-ops/with-bounded-frees (zipmap (map r/F-maker names) bbnds1)
                     (subtype? b1 b2)))
            *sub-current-seen*
            (fail! s t)))

        ;use unification to see if we can use the Poly type here
        (and (r/Poly? s)
             (let [names (c/Poly-fresh-symbols* s)
                   bnds (c/Poly-bbnds* names s)
                   b1 (c/Poly-body* names s)]
               (unify (zipmap names bnds) [b1] [t])))
        (let [names (c/Poly-fresh-symbols* s)
              bnds (c/Poly-bbnds* names s)
              b1 (c/Poly-body* names s)]
          (if (unify (zipmap names bnds) [b1] [t])
            *sub-current-seen*
            (fail! s t)))

        (and (r/Poly? t)
             (let [names (c/Poly-fresh-symbols* t)
                   b (c/Poly-body* names t)]
               (empty? (frees/fv t))))
        (let [names (c/Poly-fresh-symbols* t)
              b (c/Poly-body* names t)]
          (if (subtype? s b)
            *sub-current-seen*
            (fail! s t)))

;        (and (r/TApp? s)
;             (r/TApp? t))
;        (if (subtypeA*? (fully-resolve-type s) (fully-resolve-type t))
;          *sub-current-seen*
;          (fail! s t))

        (r/TApp? s)
        (let [{:keys [rands]} s
              rator (c/fully-resolve-type (:rator s))]
          (cond
            (r/F? rator) (fail! s t)

            (r/TypeFn? rator)
            (let [names (c/TypeFn-fresh-symbols* rator)
                  bbnds (c/TypeFn-bbnds* names rator)
                  res (c/instantiate-typefn rator rands :names names)]
              (if (subtypeA*? (conj *sub-current-seen* [s t]) res t)
                *sub-current-seen*
                (fail! s t)))

            :else (u/int-error (str "First argument to TApp must be TFn, actual: " rator))))

        (r/TApp? t)
        (let [{:keys [rands]} t
              rator (c/fully-resolve-type (:rator t))]
          (cond
            (r/F? rator) (fail! s t)

            (r/TypeFn? rator)
            (let [names (c/TypeFn-fresh-symbols* rator)
                  res (c/instantiate-typefn rator rands :names names)]
              (if (subtypeA*? (conj *sub-current-seen* [s t]) s res)
                *sub-current-seen*
                (fail! s t)))

            :else (u/int-error (str "First argument to TApp must be TFn, actual: " rator))))

        (r/App? s)
        (subtypeA* *sub-current-seen* (c/resolve-App s) t)

        (r/App? t)
        (subtypeA* *sub-current-seen* s (c/resolve-App t))

        (r/Bottom? t)
        (fail! s t)

        (r/Union? s)
        ;use subtypeA*, throws error
        (u/p :subtype-union-l
        (if (every? (fn union-left [s] (subtypeA* *sub-current-seen* s t)) (:types s))
          *sub-current-seen*
          (fail! s t))
           )

        ;use subtypeA*?, boolean result
        (r/Union? t)
        (u/p :subtype-union-r
        (if (some (fn union-right [t] (subtypeA*? *sub-current-seen* s t)) (:types t))
          *sub-current-seen*
          (fail! s t))
           )

        (and (r/FnIntersection? s)
             (r/FnIntersection? t))
        (loop [A* *sub-current-seen*
               arr2 (:types t)]
          (let [arr1 (:types s)]
            (if (empty? arr2) 
              A*
              (if-let [A (supertype-of-one-arr A* (first arr2) arr1)]
                (recur A (next arr2))
                (fail! s t)))))

        (and (r/Intersection? s)
             (r/Intersection? t))
        (let [ss (simplify-In s)
              ts (simplify-In t)]
          (if (every? (fn intersection-both [s*]
                        (some (fn intersection-both-inner [t*] (subtype? s* t*)) ts))
                      ss)
            *sub-current-seen*
            (fail! s t)))

        (r/Intersection? s)
        (let [ss (simplify-In s)]
          (if (some #(subtype? % t) ss)
            *sub-current-seen*
            (fail! s t)))

        (r/Intersection? t)
        (let [ts (simplify-In t)]
          (if (every? #(subtype? s %) ts)
            *sub-current-seen*
            (fail! s t)))

        (and (r/Extends? s)
             (r/Extends? t))
        (if (and ;all positive information matches.
                 ; Each t should occur in at least one s.
                 (every? (fn extends-t [t*]
                           (some #(subtype? % t*) (:extends s)))
                         (:extends t))
                 ;lhs does not explicitly implement any forbidden types.
                 ; No negative t should be a supertype of a positive s
                 (not-any? (fn extends-not-t [not-t*]
                             (some #(subtype? % not-t*) (:extends s)))
                           (:without t))
                 ;lhs explicitly disallows same types as rhs
                 ; Each negative t should be a supertype of some negative s
                 (every? (fn extends-without-t[not-t*]
                           (some #(subtype? % not-t*) (:without s)))
                         (:without t)))
          *sub-current-seen*
          (fail! s t))

        (r/Extends? s)
        (let [^Extends s s]
          (if (and (some #(subtype? % t) (.extends s))
                   (not-any? #(subtype? % t) (.without s)))
            *sub-current-seen*
            (fail! s t)))

        (r/Extends? t)
        (let [^Extends t t]
          (if (and (every? identity 
                           (doall
                             (for [e (.extends t)]
                               (subtype? s e))))
                   (not-any? #(subtype? s %) (.without t)))
            *sub-current-seen*
            (fail! s t)))

        (r/Mu? s)
        (subtype (c/unfold s) t)

        (r/Mu? t)
        (subtype s (c/unfold t))

        (and (r/TopFunction? t)
             (r/FnIntersection? s))
        *sub-current-seen*

        ;       B <: A
        ;_______________________________
        ; (Not A) <: (Not B)
        (every? r/NotType? [s t])
        (if (subtype? (:type t) (:type s))
          *sub-current-seen*
          (fail! s t))

        ;  A <!: B  A is not free  B is not free
        ;________________________________________
        ; A <: (Not B)
;   Should this also require (fv s) U (fv t) to be empty?
        (r/NotType? t)
        (if (and (not-any? (some-fn r/B? r/F?) [s (:type t)])
                 (not (subtype? s (:type t))))
          *sub-current-seen*
          (fail! s t))

        (and (r/F? s)
             (let [{:keys [upper-bound lower-bound] :as bnd} (free-ops/free-with-name-bnds (:name s))]
               (if-not bnd 
                 (do #_(u/int-error (str "No bounds for " (:name s)))
                     nil)
                 (and (subtype? upper-bound t)
                      (subtype? lower-bound t)))))
        *sub-current-seen*

        (and (r/F? t)
             (let [{:keys [upper-bound lower-bound] :as bnd} (free-ops/free-with-name-bnds (:name t))]
               (if-not bnd 
                 (do #_(u/int-error (str "No bounds for " (:name t)))
                     nil)
                 (and (subtype? s upper-bound)
                      (subtype? s lower-bound)))))
        *sub-current-seen*

        (and (r/AssocType? s)
             (r/AssocType? t)
             (r/F? (:target s))
             (r/F? (:target t))
             (not-any? :dentries [s t]))
        (if (and (= (:target s) (:target t))
                 (subtype? (apply c/assoc-pairs-noret (c/-complete-hmap {}) (:entries s))
                           (apply c/assoc-pairs-noret (c/-complete-hmap {}) (:entries t))))
          *sub-current-seen*
          (fail! s t))

        (and (r/AssocType? s)
             (r/F? (:target s))
             (not (r/AssocType? t)))
        (let [bnds (free-ops/free-with-name-bnds (-> s :target :name))
              _ (assert bnds
                        (str "Bounds not found for free variable: " (-> s :target :name)))]
          (if (and (subtype? (:upper-bound bnds) t)
                   (subtype? (apply c/assoc-pairs-noret (c/-complete-hmap {}) (:entries s))
                             t))
            *sub-current-seen*
            (fail! s t)))
      
        ; avoids infinite expansion because associng an F is a fixed point
        (and (r/AssocType? s)
             (not (r/F? (:target s))))
        (let [s-or-n (apply c/assoc-pairs-noret (:target s) (:entries s))]
          (if (and s-or-n (subtype? s-or-n t))
            *sub-current-seen*
            (fail! s t)))

        ; avoids infinite expansion because associng an F is a fixed point
        (and (r/AssocType? t)
             (not (r/F? (:target t))))
        (let [t-or-n (apply c/assoc-pairs-noret (:target t) (:entries t))]
          (if (and t-or-n (subtype? s t-or-n))
            *sub-current-seen*
            (fail! s t)))

        (and (r/HeterogeneousVector? s)
             (r/HeterogeneousVector? t))
        (if (and (cond 
                   ; simple case, no rest types
                   (and (not-any? :rest [s t])
                        (not-any? :drest [s t]))
                   (let []
                     (and (= (count (:types s))
                             (count (:types t)))
                          (every? identity (map subtype? (:types s) (:types t)))))

                   ; rest on right
                   (and (:rest t)
                        (not (:drest s)))
                   (and (>= (count (:types s))
                            (count (:types t)))
                        (if (:rest s)
                          (subtype? (:rest s) (:rest t))
                          true)
                        ;pad t to the right
                        (every? identity (map subtype? 
                                              (:types s)
                                              (concat (:types t) 
                                                      (repeat (- (count (:types s)) (count (:types t)))
                                                              (:rest t))))))

                   ;TODO other cases
                   :else nil
                   )
                 ; ignore interesting results
                 (every? (fn hvec1 [[f1 f2]] (or (= (fops/-FS fr/-top fr/-top) f2)
                                                 (= f1 f2)))
                         (map vector (:fs s) (:fs t)))
                 ; ignore interesting results
                 (every? (fn hvec2 [[o1 o2]] (or (orep/EmptyObject? o2)
                                                 (= o1 o2)))
                         (map vector (:objects s) (:objects t))))
          *sub-current-seen*
          (fail! s t))

        (and (r/HeterogeneousList? s)
             (r/HeterogeneousList? t))
        (if (= (count (:types s))
               (count (:types t)))
          (or (last (map subtype (:types s) (:types t)))
              #{})
          (fail! s t))

        (and (r/HeterogeneousSeq? s)
             (r/HeterogeneousSeq? t))
        (if (= (count (:types s))
               (count (:types t)))
          (or (last (map subtype (:types s) (:types t)))
              #{})
          (fail! s t))

        ;every rtype entry must be in ltypes
        ;eg. {:a 1, :b 2, :c 3} <: {:a 1, :b 2}
        (and (r/HeterogeneousMap? s)
             (r/HeterogeneousMap? t))
        (let [; convention: prefix things on left with l, right with r
              {ltypes :types labsent :absent-keys :as s} s
              {rtypes :types rabsent :absent-keys :as t} t]
          (if (and ; if t is complete, s must be complete with the same keys
                   (if (c/complete-hmap? t)
                     (if (c/complete-hmap? s)
                       (= (set (keys ltypes)) (set (keys rtypes)))
                       false)
                     true)
                   ; all absent keys in t should be absent in s
                   (every? identity
                           (for [rabsent-key rabsent]
                             ; Subtyping is good if rabsent-key is:
                             ; 1. Absent in s
                             ; 2. Not present in s, but s is complete
                             (or ((set labsent) rabsent-key)
                                 (when (c/complete-hmap? s)
                                   (not ((set (keys ltypes)) rabsent-key))))))
                   ; all present keys in t should be present in s
                   (every? identity
                           (map (fn [[k v]]
                                  (when-let [t (get ltypes k)]
                                    (subtype? t v)))
                                rtypes)))
            *sub-current-seen*
            (fail! s t)))

        (r/HeterogeneousMap? s)
        (let [^HeterogeneousMap s s]
          ; Partial HMaps do not record absence of fields, only subtype to (APersistentMap Any Any)
          (if (c/complete-hmap? s)
            (subtype (c/In (impl/impl-case
                             :clojure (c/RClass-of APersistentMap [(apply c/Un (keys (.types s)))
                                                                   (apply c/Un (vals (.types s)))])
                             :cljs (c/Protocol-of 'cljs.core/IMap [(apply c/Un (keys (.types s)))
                                                                   (apply c/Un (vals (.types s)))]))
                           (r/make-ExactCountRange (count (:types s))))
                     t)
            (subtype (c/In (impl/impl-case
                             :clojure (c/RClass-of APersistentMap [r/-any r/-any])
                             :cljs (c/Protocol-of 'cljs.core/IMap [r/-any r/-any]))
                           (r/make-CountRange (count (:types s))))
                     t)))

        (r/KwArgsSeq? s)
        (subtype (c/Un r/-nil 
                       (impl/impl-case
                         :clojure (c/RClass-of Seqable [r/-any])
                         :cljs (c/Protocol-of 'cljs.core/ISeqable [r/-any])))
                 t)

        (r/HeterogeneousVector? s)
        (let [ss (apply c/Un (:types s))]
          (subtype (c/In (impl/impl-case
                           :clojure (c/RClass-of APersistentVector [ss])
                           :cljs (c/Protocol-of 'cljs.core/IVector [ss]))
                         ((if (some #{:rest :drest} s) 
                            r/make-CountRange 
                            r/make-ExactCountRange)
                          (count (:types s))))
                   t))

        (r/HeterogeneousList? s)
        (let [ss (apply c/Un (:types s))]
          (subtype (c/In (impl/impl-case
                           :clojure (c/RClass-of PersistentList [ss])
                           :cljs (c/Protocol-of 'cljs.core/IList [ss]))
                         (r/make-ExactCountRange (count (:types s))))
                   t))

        (r/HeterogeneousSeq? s)
        (let [ss (apply c/Un (:types s))]
          (subtype (c/In (impl/impl-case
                           :clojure (c/RClass-of ASeq [ss])
                           :cljs (c/Protocol-of 'cljs.core/ISeq [ss]))
                         (r/make-ExactCountRange (count (:types s))))
                   t))

; The order of checking protocols and datatypes is subtle.
; It is easier to calculate the ancestors of a datatype than
; the descendants of a protocol, so Datatype <: Any comes 
; before Protocol <: Any.
        (and (r/Protocol? s)
             (r/Protocol? t))
        (let [{var1 :the-var variances* :variances poly1 :poly?} s
              {var2 :the-var poly2 :poly?} t]
          ;(prn "protocols subtype" s t)
          (if (and (= var1 var2)
                   (every? (fn prcol-variance [[v l r]]
                             (case v
                               :covariant (subtypeA* *sub-current-seen* l r)
                               :contravariant (subtypeA* *sub-current-seen* r l)
                               :invariant (and (subtypeA* *sub-current-seen* l r)
                                               (subtypeA* *sub-current-seen* r l))))
                           (map vector variances* poly1 poly2)))
            *sub-current-seen*
            (fail! s t)))

        (and (r/DataType? s)
             (r/DataType? t))
        (subtype-datatypes-or-records s t)

        (and (r/DataType? s)
             (r/Protocol? t))
        (if (subtype-datatype-and-protocol s t)
          *sub-current-seen*
          (fail! s t))

        (and (r/RClass? s)
             (r/Protocol? t))
        (subtype-rclass-protocol s t)

        (and (r/Nil? s)
             (r/Protocol? t)
             (impl/checking-clojure?))
        (if (contains? (c/Protocol-normal-extenders t) nil)
          *sub-current-seen*
          (fail! s t))

        ;values are subtypes of their classes
        (r/Value? s)
        (let [^Value s s
              sval (.val s)]
          (impl/impl-case
            :clojure (cond 
                       ; this is after the nil <: Protocol case, so we fail
                       (nil? sval) (fail! s t)
                       ; this is a faster path than the final case
                       (r/RClass? t) (let [cls (let [cls (u/symbol->Class (:the-class t))]
                                                 (or (boxed-primitives cls)
                                                     cls))]
                                       (cond
                                         (#{Integer Long} cls) (if (or (instance? Integer sval)
                                                                       (instance? Long sval))
                                                                 *sub-current-seen*
                                                                 (fail! s t))
                                         ;handle string-as-seqable
                                         (string? sval) (if (subtype? (c/RClass-of String) t)
                                                          *sub-current-seen*
                                                          (fail! s t))
                                         :else (if (instance? cls sval) 
                                                 *sub-current-seen*
                                                 (fail! s t))))
                       :else (subtype (apply c/In (c/RClass-of (class sval))
                                             (cond
                                               ;keyword values are functions
                                               (keyword? sval) [(c/keyword->Fn sval)]
                                               ;strings have a known length as a seqable
                                               (string? sval) [(r/make-ExactCountRange (count sval))]))
                                      t))
            :cljs (cond
                    (integer? (.val s)) (subtype (r/IntegerCLJS-maker) t)
                    (number? (.val s)) (subtype (r/NumberCLJS-maker) t)
                    (u/boolean? (.val s)) (subtype (r/BooleanCLJS-maker) t)
                    (symbol? (.val s)) (subtype (c/Protocol-of 'cljs.core/Symbol) t)
                    (keyword? (.val s)) (subtype (c/Protocol-of 'cljs.core/Keyword) t)
                    :else (fail! s t))))

        (and (r/Result? s)
             (r/Result? t))
        (subtype-Result s t)

        (and (r/PrimitiveArray? s)
             (r/PrimitiveArray? t))
        (subtype-PrimitiveArray s t)

        (r/PrimitiveArray? s)
        (subtype (r/PrimitiveArray-maker Object r/-any r/-any) t)
      
        (and (r/TypeFn? s)
             (r/TypeFn? t))
        (subtype-TypeFn s t)

        (and (r/RClass? s)
             (r/RClass? t))
        (u/p :subtype/RClass (subtype-RClass s t))

        (and (r/DataType? s)
             (r/RClass? t))
        (subtype-datatype-rclass s t)

        ; handles Var-as-function
        (and (r/RClass? s)
             (r/FnIntersection? t))
        (if (some #(when (r/FnIntersection? %)
                     (subtype? % t))
                  (map c/fully-resolve-type (c/RClass-supers* s)))
          *sub-current-seen*
          (fail! s t))

        (and (r/CountRange? s)
             (r/CountRange? t))
        (subtype-CountRange s t)

        ; CLJS special types
        (and (r/IntegerCLJS? s)
             (r/NumberCLJS? t))
        *sub-current-seen*

        :else (fail! s t)))))

(def base-type
  {'object (r/ObjectCLJS-maker)
   'string (r/StringCLJS-maker)
   'number (r/NumberCLJS-maker)
   'array  (r/ArrayCLJS-maker r/-any r/-any)
   'function (r/FunctionCLJS-maker)
   'boolean (r/BooleanCLJS-maker)
   ;'default "_"
   })


(defn resolve-JS-reference [sym]
  (impl/assert-cljs)
  (cond
    (= "js" (namespace sym)) (c/JSNominal-with-unknown-params sym)
    (= "default" sym) (assert nil "FIXME what is default?")
    (base-type sym) (base-type sym)
    :else (let [{{:keys [protocol-symbol name]} :info} (cljs-util/analyze-qualified-symbol sym)]
            (if protocol-symbol
              (c/Protocol-with-unknown-params name)
              (c/DataType-with-unknown-params name)))))


(defn protocol-extenders [^Protocol p]
  {:pre [(r/Protocol? p)]
   :post [(every? r/Type? %)]}
  (u/p :subtype/protocol-extenders
  (impl/impl-case
    :clojure (let [exts (c/Protocol-normal-extenders p)]
               (for [ext exts]
                 (cond
                   (class? ext) (c/RClass-of-with-unknown-params ext)
                   (nil? ext) r/-nil
                   :else (throw (Exception. (str "What is this?" ext))))))
    :cljs (let [exts (cljs-util/extenders (:the-var p))]
            (for [ext exts]
              (cond
                (symbol? ext) (resolve-JS-reference ext)
                (nil? ext) r/-nil
                :else (throw (Exception. (str "What is this?" ext))))))))
  )

;[Type Type -> (IPersistentSet '[Type Type])]
(defn- subtype [s t]
  {:post [(set? %)]}
  #_(prn "subtype")
;  (if-let [hit (@subtype-cache (set [s t]))]
;    (do #_(prn "subtype hit")
;        hit)
    (let [res (u/p :subtype-top-subtypeA* (subtypeA* *sub-current-seen* s t))]
      ;(swap! subtype-cache assoc (set [s t]) res)
      res))

;[(IPersistentSet '[Type Type]) (Seqable Type) (Seqable Type) (Option Type)
;  -> (IPersistentSet '[Type Type])]
(defn subtypes*-varargs [A0 argtys dom rst]
  (loop [dom dom
         argtys argtys
         A A0]
    (cond
      (and (empty? dom) (empty? argtys)) A
      (empty? argtys) (fail! argtys dom)
      (and (empty? dom) rst)
      (if-let [A (subtypeA* A (first argtys) rst)]
        (recur dom (next argtys) A)
        (fail! (first argtys) rst))

      (empty? dom) (fail! argtys dom)
      :else
      (if-let [A (subtypeA* A0 (first argtys) (first dom))]
        (recur (next dom) (next argtys) A)
        (fail! (first argtys) (first dom))))))

(defn subtype-kwargs* [^KwArgs s ^KwArgs t]
  {:pre [((some-fn r/KwArgs? nil?) s)
         ((some-fn r/KwArgs? nil?) t)]}
  (if (= s t)
    *sub-current-seen*
    (u/nyi-error "subtype kwargs")))

;; simple co/contra-variance for ->
;[(IPersistentSet '[Type Type]) Function Function -> (IPersistentSet '[Type Type])]
(defn arr-subtype [A0 ^Function s ^Function t]
  {:pre [(r/Function? s)
         (r/Function? t)]}
  ;; top for functions is above everything
  (cond
    ;; top for functions is above everything
    (r/TopFunction? t) A0
    ;; the really simple case
    (and (not ((some-fn :rest :drest :kws) s))
         (not ((some-fn :rest :drest :kws) t)))
    (do
      (when-not (= (count (.dom s))
                   (count (.dom t)))
        (fail! s t))
      (-> *sub-current-seen*
        ((fn [A0]
           (reduce (fn [A* [s t]]
                     (subtypeA* A* s t))
                   A0
                   (map vector (.dom t) (.dom s)))))
        (subtypeA* (.rng s) (.rng t))))

    ;kw args
    (and (.kws s)
         (.kws t))
    (do
      (mapv subtype (.dom t) (.dom s))
      (subtype (.rng s) (.rng t))
      (subtype-kwargs* (.kws t) (.kws s)))

    (and (:rest s)
         (not ((some-fn :rest :drest) t)))
    (-> *sub-current-seen*
      (subtypes*-varargs (.dom t) (.dom s) (.rest s))
      (subtypeA* (.rng s) (.rng t)))

    (and (not ((some-fn :rest :drest) s))
         (:rest t))
    (fail! s t)

    (and (.rest s)
         (.rest t))
    (-> *sub-current-seen*
      (subtypes*-varargs (:dom t) (:dom s) (:rest s))
      (subtypeA* (:rest t) (:rest s))
      (subtypeA* (:rng s) (:rng t)))

    ;; handle ... varargs when the bounds are the same
    (and (:drest s)
         (:drest t)
         (= (-> s :drest :name)
            (-> t :drest :name)))
    (-> *sub-current-seen*
      (subtypeA* (-> t :drest :pre-type) (-> s :drest :pre-type))
      ((fn [A0] 
         (reduce (fn [A* [s t]]
                   (subtypeA* A* s t))
                 A0 (map vector (:dom t) (:dom s)))))
      (subtypeA* (:rng s) (:rng t)))
    :else (fail! s t)))

;[(IPersistentSet '[Type Type]) Function (Seqable Function) -> (Option (IPersistentSet '[Type Type]))]
(defn supertype-of-one-arr [A s ts]
  (some #(handle-failure 
           (arr-subtype A % s))
        ts))

(defn fully-resolve-filter [fl]
  {:pre [(fr/Filter? fl)]
   :post [(fr/Filter? %)]}
  (u/p :subtype/fully-resolve-filter
  (cond
    (fr/TypeFilter? fl) (update-in fl [:type] c/fully-resolve-type)
    (fr/NotTypeFilter? fl) (update-in fl [:type] c/fully-resolve-type)
    (fr/AndFilter? fl) (update-in fl [:fs] #(set (map fully-resolve-filter %)))
    (fr/OrFilter? fl) (update-in fl [:fs] #(set (map fully-resolve-filter %)))
    (fr/ImpFilter? fl) (-> fl
                           (update-in [:a] fully-resolve-filter)
                           (update-in [:c] fully-resolve-filter))
    :else fl)))

(defn fully-resolve-flowset [flow]
  {:pre [(r/FlowSet? flow)]
   :post [(r/FlowSet? %)]}
  (-> flow
      (update-in [:normal] fully-resolve-filter)))

(defn fully-resolve-fs [fs]
  {:pre [(fr/FilterSet? fs)]
   :post [(fr/FilterSet? %)]}
  (-> fs
      (update-in [:then] fully-resolve-filter)
      (update-in [:else] fully-resolve-filter)))

(defn subtype-type-filter? [s t]
  {:pre [(fr/TypeFilter? s)
         (fr/TypeFilter? t)]}
  (and (= (:path s) (:path t))
       (= (:id s) (:id t))
       (subtype? (:type s) (:type t))))

(defn subtype-not-type-filter? [s t]
  {:pre [(fr/NotTypeFilter? s)
         (fr/NotTypeFilter? t)]}
  (and (= (:path s) (:path t))
       (= (:id s) (:id t))
       (subtype? (:type t) (:type s))))

(defn subtype-Result
  [{t1 :t ^FilterSet f1 :fl o1 :o flow1 :flow :as s}
   {t2 :t ^FilterSet f2 :fl o2 :o flow2 :flow :as t}]
  (cond
    ;trivial case
    (and (= (fully-resolve-fs f1) (fully-resolve-fs f2))
         (= o1 o2)
         (= (fully-resolve-flowset flow1) (fully-resolve-flowset flow2)))
    (subtype t1 t2)

    ;we can ignore some interesting results
    (and (orep/EmptyObject? o2)
         (or (= f2 (fops/-FS fr/-top fr/-top))
             ; check :then, :else is top
             #_(and (= (.else f2) fr/-top)
                  (= (.then f1) (.then f2)))
             ; check :else, :then is top
             #_(and (= (.then f2) fr/-top)
                  (= (.else f1) (.else f2))))
         (= flow2 (r/-flow fr/-top)))
    (subtype t1 t2)

    (and (orep/EmptyObject? o2)
         (= f1 f2)
         (= flow2 (r/-flow fr/-top)))
    (subtype t1 t2)

    ;special case for (& (is y sym) ...) <: (is y sym)
    (and (fr/AndFilter? (:then f1))
         (fr/TypeFilter? (:then f2))
         (every? fops/atomic-filter? (:fs (:then f1)))
         (= 1 (count (filter fr/TypeFilter? (:fs (:then f1)))))
         (= fr/-top (:else f2))
         (= flow1 flow2 (r/-flow fr/-top))
         (= o1 o2))
    (let [f1-tf (first (filter fr/TypeFilter? (:fs (:then f1))))]
      (if (= f1-tf (:then f2))
        (subtype t1 t2)
        (fail! t1 t2)))

    :else (fail! t1 t2)))

(defn subtype-TypeFn-app?
  [^TypeFn tfn ^TApp ltapp ^TApp rtapp]
  {:pre [(r/TypeFn? tfn)
         (r/TApp? ltapp)
         (r/TApp? rtapp)]}
  ;(prn "subtype-TApp")
  (every? (fn [[v l r]]
            (case v
              :covariant (subtypeA*? *sub-current-seen* l r)
              :contravariant (subtypeA*? *sub-current-seen* r l)
              :invariant (and (subtypeA*? *sub-current-seen* l r)
                              (subtypeA*? *sub-current-seen* r l))))
          (map vector (.variances tfn) (.rands ltapp) (.rands rtapp))))

(defmulti subtype-TApp? (fn [^TApp S ^TApp T]
                          {:pre [(r/TApp? S)
                                 (r/TApp? T)]}
                          [(class (.rator S)) (class (.rator T))
                           (= (.rator S) (.rator T))]))

(defmethod subtype-TApp? [TypeFn TypeFn false]
  [S T]
  (subtypeA*? (conj *sub-current-seen* [S T]) (c/resolve-TApp S) (c/resolve-TApp T)))

(defmethod subtype-TApp? [TypeFn TypeFn true]
  [^TApp S T]
  (binding [*sub-current-seen* (conj *sub-current-seen* [S T])]
    (subtype-TypeFn-app? (.rator S) S T)))

(defmethod subtype-TApp? [tp/TCAnyType Name false]
  [S T]
  (binding [*sub-current-seen* (conj *sub-current-seen* [S T])]
    (subtype-TApp? S (update-in T [:rator] c/resolve-Name))))

(defmethod subtype-TApp? [Name tp/TCAnyType false]
  [S T]
  (binding [*sub-current-seen* (conj *sub-current-seen* [S T])]
    (subtype-TApp? (update-in S [:rator] c/resolve-Name) T)))

(defmethod subtype-TApp? [Name Name false]
  [S T]
  (binding [*sub-current-seen* (conj *sub-current-seen* [S T])]
    (subtype-TApp? (update-in S [:rator] c/resolve-Name) 
                   (update-in T [:rator] c/resolve-Name))))

; for [Name Name false]
(prefer-method subtype-TApp? 
               [Name tp/TCAnyType false]
               [tp/TCAnyType Name false])

;same operator
(defmethod subtype-TApp? [Name Name true]
  [^TApp S T]
  (let [r (c/resolve-Name (.rator S))]
    (binding [*sub-current-seen* (conj *sub-current-seen* [S T])]
      (subtype-TApp? (assoc-in S [:rator] r)
                     (assoc-in T [:rator] r)))))

; only subtypes if applied to the same F
(defmethod subtype-TApp? [F F false] [S T] false)
;(defmethod subtype-TApp? [F F true]
;  [^TApp S T]
;  (let [tfn (some (fn [[_ {{:keys [name]} :F :keys [^Bounds bnds]}]] 
;                    (when (= name (.name ^F (.rator S)))
;                      (.higher-kind bnds)))
;                  free-ops/*free-scope*)]
;    (when tfn
;      (subtype-TypeFn-app? tfn S T))))

(defmethod subtype-TApp? :default [S T] false)

(defn subtype-TypeFn
  [^TypeFn S ^TypeFn T]
  (let [;instantiate both type functions with the same names
        names (repeatedly (.nbound S) gensym)
        sbnds (c/TypeFn-bbnds* names S)
        tbnds (c/TypeFn-bbnds* names T)
        sbody (c/TypeFn-body* names S)
        tbody (c/TypeFn-body* names T)]
    (if (and (= (.nbound S) (.nbound T))
             (= (.variances S) (.variances T))
             (every? identity
                     (map (fn [lbnd rbnd]
                            (and (subtype? (:upper-bound lbnd) (:upper-bound rbnd))
                                 (subtype? (:lower-bound rbnd) (:lower-bound lbnd))
                                 (subtype? (:lower-bound lbnd) (:upper-bound lbnd))
                                 (subtype? (:lower-bound rbnd) (:upper-bound rbnd))))
                          sbnds tbnds))
             (subtype? sbody tbody))
      *sub-current-seen*
      (fail! S T))))

(defn subtype-PrimitiveArray
  [^PrimitiveArray s 
   ^PrimitiveArray t]
  (if (and ;(= (.jtype s) (.jtype t))
           ;contravariant
           (subtype? (.input-type t)
                     (.input-type s))
           ;covariant
           (subtype? (.output-type s)
                     (.output-type t)))
    *sub-current-seen*
    (fail! s t)))

(defn datatype-ancestors 
  "Returns a set of Types which are ancestors of this datatype.
  Only useful when checking Clojure. This is because we need to query datatypes
  for their ancestors, as sometimes datatypes do not appear in `extenders`
  of a protocol (this happens when a protocol is extend directly in a deftype)."
  [{:keys [the-class] :as dt}]
  {:pre [(r/DataType? dt)]}
  (impl/assert-clojure)
  (u/p :subtype/datatype-ancestors
  (let [overidden-by (fn [sym o]
                       (cond
                         ((some-fn r/DataType? r/RClass?) o)
                         (when (= sym (:the-class o))
                           o)
                         (r/Protocol? o)
                         ; protocols are extended via their interface if they
                         ; show up in the ancestors of the datatype
                         (when (and (namespace sym)
                                    (= sym (c/Protocol-var->on-class (:the-var o))))
                           o)))
        overrides (ancest/get-datatype-ancestors dt)
        _ (assert (every? (some-fn r/DataType? r/RClass?) overrides)
                  "Overriding datatypes to things other than datatypes and classes NYI")
        ; the classes that this datatype extends.
        ; No vars should occur here because protocol are extend via
        ; their interface.
        normal-asyms (->> (ancestors (u/symbol->Class the-class))
                          (filter class?)
                          (map u/Class->symbol))
        post-override (set
                        (for [sym normal-asyms]
                          ; either we override this ancestor ...
                          (if-let [o (some #(overidden-by sym %) overrides)]
                            o
                            (let [protocol-varsym (c/Protocol-interface->on-var sym)]
                              (if (resolve protocol-varsym)
                                ;... or we make a protocol type from the varified interface ...
                                (c/Protocol-with-unknown-params protocol-varsym)
                                ;... or we make an RClass from the actual ancestor.
                                (c/RClass-of-with-unknown-params sym))))))]
    post-override))
  )

(defn ^:private subtype-rclass-protocol
  [s t]
  {:pre [(r/RClass? s)
         (r/Protocol? t)]}
  (impl/assert-clojure)
  (let [;first try and find the datatype in the protocol's extenders
        p-cls-extenders (map u/Class->symbol (filter class? (c/Protocol-normal-extenders t)))
        in-protocol-extenders? (some #{(:the-class s)} p-cls-extenders)
        relevant-rclass-ancestor (some (fn [p] 
                                         (when (and (r/Protocol? p)
                                                    (= (:the-var p) (:the-var t)))
                                           p))
                                       (c/RClass-supers* s))]
    (cond 
      ; the extension is via the protocol
      (or in-protocol-extenders?
          ; extension via the protocol's interface, or explicitly overriden
          relevant-rclass-ancestor)
      (let [relevant-protocol-extender (if relevant-rclass-ancestor
                                         relevant-rclass-ancestor
                                         (c/RClass-of-with-unknown-params (:the-class s)))]
        (if (subtype? s relevant-protocol-extender)
          *sub-current-seen*
          (fail! s t)))
      :else (fail! s t))))

;(t/ann subtype-datatype-rclass [DataType RClass -> Seen])
(defn ^:private subtype-datatype-rclass
  [s t]
  {:pre [(r/DataType? s)
         (r/RClass? t)]}
  (impl/assert-clojure)
  (let [relevant-datatype-ancestor (some (fn [p] 
                                           (when (and (r/RClass? p)
                                                      (= (:the-class p) (:the-class t)))
                                             p))
                                         (map c/fully-resolve-type (datatype-ancestors s)))]
    (if (and relevant-datatype-ancestor
             (subtype? s relevant-datatype-ancestor))
      *sub-current-seen*
      (fail! s t))))

;(t/ann subtype-datatype-and-protocol [DataType Protocol -> Seen])
(defn ^:private subtype-datatype-and-protocol
  [s t]
  {:pre [(r/DataType? s)
         (r/Protocol? t)]}
  (impl/impl-case
    :clojure (let [;first try and find the datatype in the protocol's extenders
                   p-cls-extenders (map u/Class->symbol (filter class? (c/Protocol-normal-extenders t)))
                   in-protocol-extenders? (some #{(:the-class s)} p-cls-extenders)
                   relevant-datatype-ancestor (some (fn [p] 
                                                      (when (and (r/Protocol? p)
                                                                 (= (:the-var p) (:the-var t)))
                                                        p))
                                                    (map c/fully-resolve-type (datatype-ancestors s)))]
               (cond 
                 ; the extension is via the protocol
                 (or in-protocol-extenders?
                     ; extension via the protocol's interface, or explicitly overriden
                     relevant-datatype-ancestor)
                 (let [relevant-protocol-extender (if relevant-datatype-ancestor
                                                    relevant-datatype-ancestor
                                                    (c/DataType-with-unknown-params (:the-class s)))]
                   (if (subtype? s relevant-protocol-extender)
                     *sub-current-seen*
                     (fail! s t)))
                 :else (fail! s t)))

    :cljs (assert nil "FIXME")))

(defn- subtype-datatypes-or-records
  [{cls1 :the-class poly1 :poly? :as s} 
   {cls2 :the-class poly2 :poly? :as t}]
  {:pre [(every? r/DataType? [s t])]}
  (if (and (= cls1 cls2)
           (every? (fn [[v l r]]
                     (case v
                       :covariant (subtypeA* *sub-current-seen* l r)
                       :contravariant (subtypeA* *sub-current-seen* r l)
                       :invariant (and (subtypeA* *sub-current-seen* l r)
                                       (subtypeA* *sub-current-seen* r l))))
                   (map vector (:variances s) poly1 poly2)))
    *sub-current-seen*
    (fail! s t)))

; does this really help?
(defn class-isa? 
  "A faster version of isa?, both parameters must be classes"
  [s ^Class t]
  (.isAssignableFrom t s))

; (Cons Integer) <: (Seqable Integer)
; (ancestors (Seqable Integer)

(defn- subtype-RClass-common-base 
  [{polyl? :poly? lcls-sym :the-class :as s}
   {polyr? :poly? rcls-sym :the-class :as t}]
  (impl/assert-clojure)
  (let [{variances :variances} s]
    (and (= lcls-sym rcls-sym)
         (or (and (empty? polyl?) (empty? polyr?))
             (and (seq polyl?)
                  (seq polyr?)
                  (every? identity
                          (doall (map #(case %1
                                         :covariant (subtype? %2 %3)
                                         :contravariant (subtype? %3 %2)
                                         (= %2 %3))
                                      variances
                                      polyl?
                                      polyr?))))))))

;(IPersistentMap Class Class)
(def boxed-primitives
  {Byte/TYPE Byte
   Short/TYPE Short
   Integer/TYPE Integer
   Long/TYPE Long
   Float/TYPE Float
   Double/TYPE Double
   Character/TYPE Character
   Boolean/TYPE Boolean})

;[RClass RClass -> Boolean]
(defn coerse-RClass-primitive
  [s t]
  (impl/assert-clojure)
  (cond
    ; (U Integer Long) <: (U int long)
    (and 
      (#{(c/RClass-of Integer) (c/RClass-of Long)} s)
      (#{(c/RClass-of 'int) (c/RClass-of 'long)} t))
    true

    :else
    (let [spcls (u/symbol->Class (:the-class s))
          tpcls (u/symbol->Class (:the-class t))
          scls (or (boxed-primitives spcls)
                   spcls)
          tcls (or (boxed-primitives tpcls)
                   tpcls)]
      (class-isa? scls tcls))))

(defn subtype-RClass
  [{polyl? :poly? :as s}
   {polyr? :poly? :as t}]
  (impl/assert-clojure)
  (let [scls (r/RClass->Class s)
        tcls (r/RClass->Class t)]
    ;(prn "subtype RClass" (prs/unparse-type s) (prs/unparse-type t))
    (cond
      (or
        ; use java subclassing
        (and (empty? polyl?)
             (empty? polyr?)
             (empty? (:replacements s))
             (empty? (:replacements t))
             (class-isa? scls tcls))

        ;same base class
        (and (= scls tcls)
             (subtype-RClass-common-base s t))

        ;one is a primitive, coerse
        (and (or (.isPrimitive scls)
                 (.isPrimitive tcls))
             (coerse-RClass-primitive s t))

        ;find a supertype of s that is the same base as t, and subtype of it
        (some #(when (r/RClass? %)
                 (and (= (:the-class t) (:the-class %))
                      (subtype-RClass-common-base % t)))
              (map c/fully-resolve-type (c/RClass-supers* s))))
      *sub-current-seen*

      ;try each ancestor

      :else (fail! s t))))

;subtype if t includes all of s. 
;tl <= sl, su <= tu
(defn subtype-CountRange
  [{supper :upper slower :lower :as s}
   {tupper :upper tlower :lower :as t}]
  (if (and (<= tlower slower)
           (if tupper
             (and supper (<= supper tupper))
             true))
    *sub-current-seen*
    (fail! s t)))

(defmacro sub-clj? [s t]
  `(impl/with-clojure-impl
     (subtype? (prs/parse-type '~s)
               (prs/parse-type '~t))))
