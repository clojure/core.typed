;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.jvm.subtype
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.core.typed.checker.filter-rep :as fr]
            [clojure.core.typed.checker.filter-ops :as fops]
            [clojure.core.typed.checker.object-rep :as orep]
            [clojure.core.typed.checker.frees :as frees]
            [clojure.core.typed.checker.free-ops :as free-ops]
            [clojure.core.typed.checker.datatype-ancestor-env :as ancest]
            [clojure.core.typed.checker.path-rep :as pth-rep]
            [clojure.core.typed.checker.indirect-ops :as ind]
            [clojure.core.typed.checker.indirect-utils :as ind-u]
            [clojure.core.typed.checker.jvm.assoc-utils :as assoc-u]
            [clojure.set :as set])
  (:import (clojure.lang ASeq)))

(defn ^:private gen-repeat [times repeated]
  (reduce (fn [acc cur]
            (concat acc cur))
          []
          (repeat times repeated)))

;(defalias Seen Any)

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

(declare subtypes*-varargs subtype?)

;[(t/Seqable Type) (t/Seqable Type) Type -> Boolean]
(defn subtypes-varargs?
  "True if argtys are under dom"
  [argtys dom rst kws]
  (handle-failure
    (subtypes*-varargs #{} argtys dom rst kws)
    true))

(defn subtypes-prest? [argtys dom prest]
  "True if argtys are under dom and prest"
  (let [dom-count (count dom)
        [argtys-dom argtys-rest] (split-at dom-count argtys)]
    (handle-failure
      (subtypes*-varargs #{} argtys-dom dom nil nil)
      (subtype? (r/-hvec (vec argtys-rest)) prest))))

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
  {:post [(boolean? %)]}
  (letfn [(do-subtype []
            (boolean
              (handle-failure
                (subtype s t))))]
    (if-let [[_ res] (find @subtype-cache [s t])]
      res
      (let [res (do-subtype)]
        (when-not (currently-subtyping?)
          (swap! subtype-cache assoc [s t] res))
        res))))

(declare subtypeA*)

;[(t/Set '[Type Type]) Type Type -> Boolean]
(defn subtypeA*? [A s t]
  (handle-failure
    (subtypeA* A s t)))

(declare supertype-of-one-arr)

;[(Map Symbol Bounds) (Map Symbol Bounds) (t/Seqable Type) (t/Seqable Type)
;  -> Boolean]
(defn unify [X Y S T R]
  (boolean 
    (u/handle-cs-gen-failure
      (ind/infer X Y S T R))))

(declare protocol-extenders subtype-TypeFn-rands?
         subtype-datatypes-or-records subtype-Result subtype-PrimitiveArray
         subtype-CountRange subtype-TypeFn subtype-RClass
         subtype-datatype-and-protocol subtype-rclass-protocol
         boxed-primitives subtype-datatype-rclass subtype-TApp)

(defn subtype-HSet [s t]
  {:pre [(r/HSet? s)
         (r/HSet? t)]}
  (if (and (= (:fixed s) (:fixed t))
           (:complete? s)
           (:complete? t))
    *sub-current-seen*
    (fail! s t)))

(defn simplify-In [t]
  {:pre [(r/Intersection? t)]}
  (let [mi (apply c/In (:types t))]
    (if (r/Intersection? mi)
      (:types mi)
      [mi])))

;TODO replace hardcoding cases for unfolding Mu? etc. with a single case for unresolved types.
;[(t/Set '[Type Type]) Type Type -> (t/Set '[Type Type])]
(defn subtypeA* [A s t]
  {:pre [(r/AnyType? s)
         (r/AnyType? t)]
   :post [(set? %)]}
  ;(prn "subtypeA*" s t)
  (if (or (contains? A [s t])
          (= s t)
          ; FIXME TypeFn's probably are not between Top/Bottom
          (r/Top? t)
          (r/Bottom? s)
          ;; Unchecked is both bottom and top
          (r/Unchecked? s)
          (r/Unchecked? t)
          ;TCError is top and bottom
          (some r/TCError? [s t]))
    A
    (binding [*sub-current-seen* (conj A [s t])]
      (cond
        (or (r/TCResult? s)
            (r/TCResult? t))
        (assert nil "Cannot give TCResult to subtype")

        ; use bounds to determine subtyping between frees and types
        ; 2 frees of the same name are handled in the (= s t) case.
        (and (r/F? s)
             (let [{:keys [upper-bound lower-bound] :as bnd} (free-ops/free-with-name-bnds (:name s))]
               (if-not bnd 
                 (do #_(err/int-error (str "No bounds for " (:name s)))
                     nil)
                 (and (subtype? upper-bound t)
                      (subtype? lower-bound t)))))
        *sub-current-seen*

        (and (r/F? t)
             (let [{:keys [upper-bound lower-bound] :as bnd} (free-ops/free-with-name-bnds (:name t))]
               (if-not bnd 
                 (do #_(err/int-error (str "No bounds for " (:name t)))
                     nil)
                 (and (subtype? s upper-bound)
                      (subtype? s lower-bound)))))
        *sub-current-seen*

        (r/TypeOf? s)
        (subtypeA* *sub-current-seen* (c/resolve-TypeOf s) t)

        (r/TypeOf? t)
        (subtypeA* *sub-current-seen* s (c/resolve-TypeOf t))

        (and (r/Value? s)
             (r/Value? t))
        ;already (not= s t)
        (fail! s t)

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
                   b1 (c/Poly-body* names s)
                   ;_ (prn "try unify on left")
                   u (unify (zipmap names bnds) {} [b1] [t] r/-any)]
               ;(prn "unified on left")
               u))
        *sub-current-seen*

        (and (r/PolyDots? s)
             (let [names (c/PolyDots-fresh-symbols* s)
                   bnds (c/PolyDots-bbnds* names s)
                   b1 (c/PolyDots-body* names s)
                   ;_ (prn "try PolyDots unify on left")
                   u (unify (zipmap (butlast names) (butlast bnds)) {(last names) (last bnds)} 
                            [b1] [t] r/-any)]
               ;(prn "unified on left" u)
               u))
        *sub-current-seen*

        (and (r/Poly? t)
             (let [names (c/Poly-fresh-symbols* t)
                   b (c/Poly-body* names t)]
               (empty? (frees/fv t))))
        (let [names (c/Poly-fresh-symbols* t)
              b (c/Poly-body* names t)]
          (if (subtype? s b)
            *sub-current-seen*
            (fail! s t)))

        (r/Name? s)
        (subtypeA* *sub-current-seen* (c/resolve-Name s) t)

        (r/Name? t)
        (subtypeA* *sub-current-seen* s (c/resolve-Name t))

        (r/Mu? s)
        (subtype (c/unfold s) t)

        (r/Mu? t)
        (subtype s (c/unfold t))

        (r/App? s)
        (subtypeA* *sub-current-seen* (c/resolve-App s) t)

        (r/App? t)
        (subtypeA* *sub-current-seen* s (c/resolve-App t))

        (r/Bottom? t)
        (fail! s t)

        (and (r/TApp? s)
             (r/TApp? t)
             (= (c/fully-resolve-type (:rator s))
                (c/fully-resolve-type (:rator t))))
        (subtype-TApp s t)

        (and (r/TApp? s)
             (r/TypeFn? (c/fully-resolve-type (:rator s))))
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

            :else (err/int-error (str "First argument to TApp must be TFn, actual: " (prs/unparse-type rator)))))

        (and (r/TApp? t)
             (r/TypeFn? (c/fully-resolve-type (:rator t))))
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

            :else (err/int-error (str "First argument to TApp must be TFn, actual: " (prs/unparse-type rator)))))

        (r/Union? s)
        ;use subtypeA*, throws error
        (if (every? (fn union-left [s] (subtypeA* *sub-current-seen* s t)) (:types s))
          *sub-current-seen*
          (fail! s t))

        ;use subtypeA*?, boolean result
        (r/Union? t)
        (if (some (fn union-right [t] (subtypeA*? *sub-current-seen* s t)) (:types t))
          *sub-current-seen*
          (fail! s t))

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

;does it matter what order the Intersection cases are?
        (r/Intersection? t)
        (let [ts (simplify-In t)]
          (if (every? #(subtype? s %) ts)
            *sub-current-seen*
            (fail! s t)))

        (r/Intersection? s)
        (let [ss (simplify-In s)]
          (if (some #(subtype? % t) ss)
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
                 (every? (fn extends-without-t [not-t*]
                           (some #(subtype? % not-t*) (:without s)))
                         (:without t)))
          *sub-current-seen*
          (fail! s t))

        (r/Extends? s)
        (if (and (some #(subtype? % t) (:extends s))
                 (not-any? #(subtype? % t) (:without s)))
          *sub-current-seen*
          (fail! s t))

        (r/Extends? t)
        (if (and (every? identity 
                         (doall
                           (for [e (:extends t)]
                             (subtype? s e))))
                 (not-any? #(subtype? s %) (:without t)))
          *sub-current-seen*
          (fail! s t))

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

; delegate to NotType
        (r/DifferenceType? s)
        (subtype (apply c/In (:type s) (map r/NotType-maker (:without s)))
                 t)

        (r/DifferenceType? t)
        (subtype s
                 (apply c/In (:type t) (map r/NotType-maker (:without t))))

        (and (r/GetType? s)
             (not (r/F? (:target s))))
        (subtype (c/-resolve s) t)

        (and (r/GetType? t)
             (not (r/F? (:target t))))
        (subtype s (c/-resolve t))

        (and (r/AssocType? s)
             (r/RClass? t)
             ; (Map xx yy)
             (= 'clojure.lang.IPersistentMap (:the-class t)))
        (let [{:keys [target entries dentries]} s
              {:keys [poly? the-class]} t
              ; _ (when-not (nil? dentries) (err/nyi-error (pr-str "NYI subtype of dentries AssocType " s)))
              ; we assume its all right
              entries-keys (map first entries)
              entries-vals (map second entries)]
          (if (and (subtype? target t)
                   (every? identity (map subtype? entries-keys (repeat (first poly?))))
                   (every? identity (map subtype? entries-vals (repeat (second poly?)))))
            *sub-current-seen*
            (fail! s t)))

        (and (r/AssocType? s)
             (r/AssocType? t)
             (r/F? (:target s))
             (r/F? (:target t))
             (not-any? :dentries [s t]))
        (if (and (= (:target s) (:target t))
                 (subtype? (apply assoc-u/assoc-pairs-noret (c/-complete-hmap {}) (:entries s))
                           (apply assoc-u/assoc-pairs-noret (c/-complete-hmap {}) (:entries t))))
          *sub-current-seen*
          (fail! s t))

        (and (r/AssocType? s)
             (r/F? (:target s))
             (not (r/AssocType? t)))
        (let [bnds (free-ops/free-with-name-bnds (-> s :target :name))
              _ (assert bnds
                        (str "Bounds not found for free variable: " (-> s :target :name)))]
          (if (and (subtype? (:upper-bound bnds) t)
                   (subtype? (apply assoc-u/assoc-pairs-noret (c/-complete-hmap {}) (:entries s))
                             t))
            *sub-current-seen*
            (fail! s t)))
      
        ; avoids infinite expansion because associng an F is a fixed point
        (and (r/AssocType? s)
             (not (r/F? (:target s))))
        (let [s-or-n (apply assoc-u/assoc-pairs-noret (:target s) (:entries s))]
          (if (and s-or-n (subtype? s-or-n t))
            *sub-current-seen*
            (fail! s t)))

        ; avoids infinite expansion because associng an F is a fixed point
        (and (r/AssocType? t)
             (not (r/F? (:target t))))
        (let [t-or-n (apply assoc-u/assoc-pairs-noret (:target t) (:entries t))]
          (if (and t-or-n (subtype? s t-or-n))
            *sub-current-seen*
            (fail! s t)))

        (and (r/HSequential? s)
             (r/HSequential? t)
             (r/compatible-HSequential-kind? (:kind s) (:kind t)))
        (if (and (cond
                   ; simple case, no rest, drest, repeat types
                   (and (not-any? :rest [s t])
                        (not-any? :drest [s t])
                        (not-any? :repeat [s t]))
                   (let []
                     (and (= (count (:types s))
                             (count (:types t)))
                          (every? identity (map subtype? (:types s) (:types t)))))

                   ; repeat on left
                   (and (:repeat s)
                        (not (:drest t)))
                   (let [s-types (:types s)
                         t-types (:types t)
                         s-types-count (count s-types)
                         t-types-count (count t-types)]
                     (cond
                       (:rest t)
                       (and (= 1 s-types-count)
                            (every? identity (map subtype?
                                                  (repeat (first s-types))
                                                  t-types))
                            (subtype? (first s-types) (:rest t)))

                       ; both s & t have :repeat
                       (:repeat t)
                       (if (and (<= t-types-count
                                    s-types-count)
                                (zero? (rem s-types-count
                                            t-types-count)))
                         (every? identity (map subtype?
                                               s-types
                                               (gen-repeat (/ (count s-types)
                                                              (count t-types)) t-types)))
                         false)

                       ; nothing on right
                       :else
                       false))

                   ; repeat on right
                   (and (:repeat t)
                        (not (:drest s)))
                   (let [s-types (:types s)
                         t-types (:types t)
                         s-types-count (count s-types)
                         t-types-count (count t-types)]
                     (if (:rest s)
                       (and (= 1 t-types-count)
                            (every? identity (map subtype?
                                                  s-types
                                                  (repeat (first t-types))))
                            (subtype? (:rest s) (first t-types)))

                       ; nothing on left
                       (and (or (zero? s-types-count)
                                (<= t-types-count
                                    s-types-count))
                            (if (zero? (rem s-types-count
                                            t-types-count))
                              (every? identity (map subtype?
                                                    s-types
                                                    (gen-repeat (/ s-types-count
                                                                   t-types-count) t-types)))
                              false))))

                   ; rest on right
                   (and (:rest t)
                        (not ((some-fn :drest :repeat) s)))
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

                   (and (:drest s)
                        (:rest t))
                   (and
                     (every? identity (map subtype?
                                           (:types s)
                                           (concat (:types t)
                                                   (repeat (- (count (:types s)) (count (:types t)))
                                                           (:rest t)))))
                     (r/Top? (:rest t)))

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

        ; repeat Heterogeneous* can always accept nil
        (and (r/Nil? s)
             (r/HSequential? t)
             (:repeat t))
          *sub-current-seen*

        ;every rtype entry must be in ltypes
        ;eg. {:a 1, :b 2, :c 3} <: {:a 1, :b 2}
        (and (r/HeterogeneousMap? s)
             (r/HeterogeneousMap? t))
        (let [; convention: prefix things on left with l, right with r
              {ltypes :types labsent :absent-keys :as s} s
              {rtypes :types rabsent :absent-keys :as t} t]
          (if (and ; if t is complete, s must be complete ..
                   (if (c/complete-hmap? t)
                     (if (c/complete-hmap? s)
                       ; mandatory keys on the right must appear as
                       ; mandatory on the left, but extra keys may appear
                       ; on the left
                       (and (let [right-mkeys (set (keys rtypes))
                                  left-mkeys (set (keys ltypes))]
                              (set/subset? right-mkeys
                                           left-mkeys))
                            ; extra mandatory keys on the left must appear
                            ; as optional on the right
                            (let [left-extra-mkeys (set/difference (set (keys ltypes))
                                                                       (set (keys rtypes)))
                                  right-optional-keys (set (keys (:optional t)))]
                              (set/subset? left-extra-mkeys
                                           right-optional-keys)))
                            ;Note:
                            ; optional key keys on t must be optional or mandatory or absent in s,
                            ; which is always the case so we don't need to check.
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
                                rtypes))
                   ; all optional keys in t should match optional/mandatory entries in s
                   (every? identity
                           (map (fn [[k v]]
                                  (let [matches-entry?
                                        (if-let [actual-v 
                                                 ((merge-with c/In
                                                    (:types s)
                                                    (:optional s))
                                                  k)]
                                          (subtype? actual-v v)
                                          (c/complete-hmap? s))]
                                  (cond
                                    (c/partial-hmap? s)
                                      (or (contains? (:absent-keys s) k)
                                          matches-entry?)
                                    :else matches-entry?)))
                                (:optional t)))
                   )
            *sub-current-seen*
            (fail! s t)))

        (r/HeterogeneousMap? s)
        (subtype (c/upcast-hmap s) t)

        ;; JSObj is covariant, taking after TypeScript & Google Closure. Obviously unsound.
        (and (r/JSObj? s)
             (r/JSObj? t))
        (let [; convention: prefix things on left with l, right with r
              {ltypes :types} s
              {rtypes :types} t]
          (if (every? (fn [[k rt]]
                        (let [lt (get ltypes k)]
                          (when lt
                            (subtype? lt rt))))
                      rtypes)
            *sub-current-seen*
            (fail! s t)))

        (and (r/HSet? s)
             (r/HSet? t))
        (subtype-HSet s t)

        (r/HSet? s)
        (subtype (c/upcast-hset s) t)

        (r/KwArgsSeq? s)
        (let [ss (if (:complete? s)
                   (apply c/Un
                          (concat
                            (apply concat (:mandatory s))
                            (apply concat (:optional s))))
                   r/-any)
              min-count (* 2 (count (:mandatory s)))
              max-count (when (:complete? s)
                          (+ min-count
                             (* 2 (count (:optional s)))))]
          (subtype (apply c/Un 
                          (concat
                            (when (and (:nilable-non-empty? s)
                                       (not (zero? min-count)))
                              [r/-nil])
                            [(c/In (r/make-CountRange 
                                     (max (if (:nilable-non-empty? s) 
                                            2 
                                            0)
                                          min-count) 
                                     max-count)
                                   (impl/impl-case
                                     :clojure (c/RClass-of ASeq [ss])
                                     :cljs (c/Protocol-of 'cljs.core/ISeq [ss])))]))
                   t))

        ; TODO add repeat support
        (r/HSequential? s)
        (subtype (c/upcast-HSequential s) t)

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

        ((some-fn r/JSNull? r/JSUndefined?) s)
        (subtypeA* *sub-current-seen* r/-nil t)

        ;values are subtypes of their classes
        (r/Value? s)
        (let [sval (:val s)]
          (impl/impl-case
            :clojure (cond 
                       ; this is after the nil <: Protocol case, so we fail
                       (nil? sval) (fail! s t)
                       ; this is a faster path than the final case
                       (r/RClass? t) (let [cls (let [cls (coerce/symbol->Class (:the-class t))]
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
                    (integer? sval) (subtype (r/CLJSInteger-maker) t)
                    (number? sval) (subtype (r/JSNumber-maker) t)
                    (string? sval) (subtype (r/JSString-maker) t)
                    (boolean? sval) (subtype (r/JSBoolean-maker) t)
                    (symbol? sval) (subtype (c/DataType-of 'cljs.core/Symbol) t)
                    (keyword? sval) (subtype (c/DataType-of 'cljs.core/Keyword) t)
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
        (subtype-RClass s t)

        (and (r/DataType? s)
             (r/RClass? t))
        (subtype-datatype-rclass s t)

        ; handles classes with FnIntersection ancestors
        (and (r/RClass? s)
             (r/FnIntersection? t))
        (cond
          ; Var doesn't actually have an FnIntersection ancestor,
          ; but this case simulates it.
          (#{'clojure.lang.Var} (:the-class s))
          (let [[_ read-type :as poly] (:poly? s)
                _ (when-not (#{2} (count (:poly? s)))
                    (err/int-error
                      (str "Assuming Var takes 2 arguments, "
                           "given " (count (:poly? s)))))]
            (if (subtype? read-type t)
              *sub-current-seen*
              (fail! s t)))

          :else (if (some #(when (r/FnIntersection? %)
                             (subtype? % t))
                          (map c/fully-resolve-type (c/RClass-supers* s)))
                  *sub-current-seen*
                  (fail! s t)))

        ; handles classes with heterogeneous vector ancestors (eg. IMapEntry)
        (and (r/RClass? s)
             (r/HSequential? t))
        (if (some #(when (r/HSequential? %)
                     (subtype? % t))
                  (map c/fully-resolve-type (c/RClass-supers* s)))
          *sub-current-seen*
          (fail! s t))

        ; hack for FnIntersection <: clojure.lang.IFn
        (when (r/FnIntersection? s)
          (subtype? (c/RClass-of clojure.lang.IFn) t))
        *sub-current-seen*

        (and (r/CountRange? s)
             (r/CountRange? t))
        (subtype-CountRange s t)

        ; CLJS special types
        (and (r/CLJSInteger? s)
             (r/JSNumber? t))
        *sub-current-seen*

        (and (r/PolyDots? s)
             (r/PolyDots? t)
             (= (:nbound s) (:nbound t)))
        (let [;instantiate both sides with the same fresh variables
              names (repeatedly (:nbound s) gensym)
              bbnds1 (c/PolyDots-bbnds* names s)
              bbnds2 (c/PolyDots-bbnds* names t)
              b1 (c/PolyDots-body* names s)
              b2 (c/PolyDots-body* names t)]
          (if (and (= bbnds1 bbnds2)
                   (free-ops/with-bounded-frees (zipmap (map r/F-maker names) bbnds1)
                     (subtype? b1 b2)))
            *sub-current-seen*
            (fail! s t)))

        ; TODO if s is (All [r x ...] [x ... x -> r]) and t is (All [r x] [x * -> r]) then we should say yes?

        :else (fail! s t)))))

(let [analyze-qualified-symbol (delay (impl/dynaload 'clojure.core.typed.checker.jvm.analyze-cljs/analyze-qualified-symbol))]
  (defn resolve-JS-reference [sym]
    (impl/assert-cljs)
    (cond
      (= "js" (namespace sym)) (c/JSNominal-with-unknown-params sym)
      :else (let [{{:keys [protocol-symbol name]} :info} (@analyze-qualified-symbol sym)]
              (if protocol-symbol
                (c/Protocol-with-unknown-params name)
                (c/DataType-with-unknown-params name))))))


(let [cljs-extenders (delay (impl/dynaload 'clojure.core.typed.checker.jvm.analyze-cljs/extenders))]
  (defn protocol-extenders [p]
    {:pre [(r/Protocol? p)]
     :post [(every? r/Type? %)]}
    (impl/impl-case
      :clojure (let [exts (c/Protocol-normal-extenders p)]
                 (for [ext exts]
                   (cond
                     (class? ext) (c/RClass-of-with-unknown-params ext)
                     (nil? ext) r/-nil
                     :else (throw (Exception. (str "What is this?" ext))))))
      :cljs (let [exts (@cljs-extenders (:the-var p))]
              (for [ext exts]
                (cond
                  (symbol? ext) (resolve-JS-reference ext)
                  (nil? ext) r/-nil
                  :else (throw (Exception. (str "What is this?" ext)))))))))

;[Type Type -> (IPersistentSet '[Type Type])]
(defn- subtype [s t]
  {:post [(set? %)]}
  #_(prn "subtype")
;  (if-let [hit (@subtype-cache (set [s t]))]
;    (do #_(prn "subtype hit")
;        hit)
    (let [res (subtypeA* *sub-current-seen* s t)]
      ;(swap! subtype-cache assoc (set [s t]) res)
      res))

;[(IPersistentSet '[Type Type]) (t/Seqable Type) (t/Seqable Type) (Option Type)
;  -> (IPersistentSet '[Type Type])]
(defn subtypes*-varargs [A0 argtys dom rst kws]
  {:pre [((some-fn nil? r/Type?) rst)
         ((some-fn nil? r/KwArgs?) kws)]}
  (letfn [(all-mandatory-kws? [found-kws]
            {:pre [(set? found-kws)]}
            (empty? (set/difference (set (keys (:mandatory kws)))
                                    found-kws)))]
    (loop [dom dom
           argtys argtys
           A A0
           found-kws #{}]
      (cond
        (and (empty? dom) (empty? argtys)) 
        (if (all-mandatory-kws? found-kws)
          A
          (fail! argtys dom))

        (empty? argtys) (fail! argtys dom)

        (and (empty? dom) rst)
        (if-let [A (subtypeA* A (first argtys) rst)]
          (recur dom (next argtys) A found-kws)
          (fail! (first argtys) rst))

        (and (empty? dom) (<= 2 (count argtys)) kws)
        (let [kw (c/fully-resolve-type (first argtys))
              val (second argtys)
              expected-val ((some-fn (:mandatory kws) (:optional kws))
                            kw)]
          (if (and expected-val (subtype? val expected-val))
            (recur dom (drop 2 argtys) A (conj found-kws kw))
            (fail! (take 2 argtys) kws)))

        (empty? dom) (fail! argtys dom)
        :else
        (if-let [A (subtypeA* A0 (first argtys) (first dom))]
          (recur (next dom) (next argtys) A found-kws)
          (fail! (first argtys) (first dom)))))))

;FIXME
(defn subtype-kwargs* [s t]
  {:pre [((some-fn r/KwArgs? nil?) s)
         ((some-fn r/KwArgs? nil?) t)]}
  (if (= s t)
    *sub-current-seen*
    (err/nyi-error "subtype kwargs")))

;; simple co/contra-variance for ->
;[(IPersistentSet '[Type Type]) Function Function -> (IPersistentSet '[Type Type])]
(defn arr-subtype [A0 s t]
  {:pre [(r/Function? s)
         (r/Function? t)]}
  ;; top for functions is above everything
  (cond
    ;; top for functions is above everything
    (r/TopFunction? t) A0
    ;; the really simple case
    (and (not ((some-fn :rest :drest :kws :prest) s))
         (not ((some-fn :rest :drest :kws :prest) t)))
    (do
      (when-not (= (count (:dom s))
                   (count (:dom t)))
        (fail! s t))
      (-> *sub-current-seen*
        ((fn [A0]
           (reduce (fn [A* [s t]]
                     (subtypeA* A* s t))
                   A0
                   (map vector (:dom t) (:dom s)))))
        (subtypeA* (:rng s) (:rng t))))

    (and (:prest s)
         (:prest t))
    (if (and (= (count (:dom s))
                (count (:dom t)))
             (-> *sub-current-seen*
               ((fn [A0]
                  (reduce (fn [A* [s t]]
                            (subtypeA* A* s t))
                          A0
                          (map vector (:dom t) (:dom s)))))
               (subtypeA* (:rng s) (:rng t)))
             (subtype (:prest s) (:prest t)))
      *sub-current-seen*
      (fail! s t))

    (and (:rest s)
         (:prest t))
    (let [_ (subtypeA* *sub-current-seen* (:rng s) (:rng t))
          subtype-list (fn [s t]
                         (for [s s
                               t t]
                           (subtypeA* *sub-current-seen* s t)))
          s-dom (:dom s)
          s-dom-count (count s-dom)
          t-dom (:dom t)
          t-dom-count (count t-dom)
          s-rest (:rest s)
          t-prest-types (-> t :prest :types)
          t-prest-types-count (count t-prest-types)
          _ (subtype-list (repeat t-prest-types-count s-rest) t-prest-types)]
      (if (> s-dom-count t-dom-count)
        ; hard mode
        (let [[s-dom-short s-dom-rest] (split-at t-dom-count s-dom)
              _ (subtype-list t-dom s-dom-short)
              remain-repeat-count (rem (count s-dom-rest) t-prest-types-count)
              ceiling (fn [up low]
                        {:pre [(every? integer? [up low])]}
                        (let [result (quot up low)]
                          (if (zero? (rem up low))
                            result
                            (inc result))))
              repeat-times (if (empty? s-dom-rest)
                             0
                             (ceiling (count s-dom-rest) t-prest-types-count))
              _ (subtype-list (concat s-dom-rest (repeat remain-repeat-count s-rest))
                              (gen-repeat repeat-times t-prest-types))]
          *sub-current-seen*)
        ; easy mode
        (let [[t-dom-short t-dom-rest] (split-at s-dom-count t-dom)
              _ (subtype-list t-dom-short s-dom)
              _ (subtype-list t-dom-rest (repeat (count t-dom-rest) s-rest))]
          *sub-current-seen*)))

    ;kw args
    (and (:kws s)
         (:kws t))
    (do
      (mapv subtype (:dom t) (:dom s))
      (subtype (:rng s) (:rng t))
      (subtype-kwargs* (:kws t) (:kws s)))

    (and (:rest s)
         (not ((some-fn :rest :drest :kws :prest) t)))
    (-> *sub-current-seen*
      (subtypes*-varargs (:dom t) (:dom s) (:rest s) nil)
      (subtypeA* (:rng s) (:rng t)))

    (and (not ((some-fn :rest :drest :kws :prest) s))
         (:rest t))
    (fail! s t)

    (and (:rest s)
         (:rest t))
    (-> *sub-current-seen*
      (subtypes*-varargs (:dom t) (:dom s) (:rest s) nil)
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

;[(IPersistentSet '[Type Type]) Function (t/Seqable Function) -> (Option (IPersistentSet '[Type Type]))]
(defn supertype-of-one-arr [A s ts]
  (some #(handle-failure 
           (arr-subtype A % s))
        ts))

(defn fully-resolve-filter [fl]
  {:pre [(fr/Filter? fl)]
   :post [(fr/Filter? %)]}
  (cond
    (fr/TypeFilter? fl) (update-in fl [:type] c/fully-resolve-type)
    (fr/NotTypeFilter? fl) (update-in fl [:type] c/fully-resolve-type)
    (fr/AndFilter? fl) (update-in fl [:fs] #(set (map fully-resolve-filter %)))
    (fr/OrFilter? fl) (update-in fl [:fs] #(set (map fully-resolve-filter %)))
    (fr/ImpFilter? fl) (-> fl
                           (update-in [:a] fully-resolve-filter)
                           (update-in [:c] fully-resolve-filter))
    :else fl))

(defn simplify-type-filter [f]
  {:pre [(fr/TypeFilter? f)]}
  (let [[fpth & rstpth] (:path f)]
    (cond 
      (empty? (:path f)) 
      f

      (pth-rep/KeyPE? fpth)
      (simplify-type-filter
        (fops/-filter 
          (c/make-HMap :mandatory {(r/-val (:val fpth)) (:type f)})
          (:id f)
          rstpth))
      :else f)))

(defn subtype-type-filter? [s t]
  {:pre [(fr/TypeFilter? s)
         (fr/TypeFilter? t)]}
  (let [s (simplify-type-filter s)
        t (simplify-type-filter t)]
    (and (fr/equal-paths? s t)
         (subtype? (:type s) (:type t)))))

(defn simplify-not-type-filter [f]
  {:pre [(fr/NotTypeFilter? f)]}
  (let [[fpth & rstpth] (:path f)]
    (cond 
      (empty? (:path f)) 
      f

      (pth-rep/KeyPE? fpth)
      (simplify-not-type-filter
        (fops/-not-filter 
          ; keys is optional
          (c/make-HMap 
            :optional {(r/-val (:val fpth)) (:type f)})
          (:id f)
          rstpth))
      :else f)))

(defn subtype-not-type-filter? [s t]
  {:pre [(fr/NotTypeFilter? s)
         (fr/NotTypeFilter? t)]}
  (let [s (simplify-not-type-filter s)
        t (simplify-not-type-filter t)]
    (and (fr/equal-paths? s t)
         (subtype? (:type t) (:type s)))))

(defn subtype-filter-set? [f1 f2]
  {:pre [(fr/FilterSet? f1)
         (fr/FilterSet? f2)]
   :post [(boolean? %)]}
  (boolean
    (or (= f2 (fops/-FS fr/-top fr/-top))
        (letfn [(sub-helper [f1 f2 pred field sub?]
                  (when (every? pred (map field [f1 f2]))
                    (sub? (field f1) (field f2))))]
          (or
            (and (sub-helper f1 f2 fr/TypeFilter? :then subtype-type-filter?)
                 (sub-helper f1 f2 fr/TypeFilter? :else subtype-not-type-filter?))
            (and (sub-helper f1 f2 fr/TypeFilter? :then subtype-type-filter?)
                 (sub-helper f1 f2 fr/NotTypeFilter? :else subtype-not-type-filter?))
            (and (sub-helper f1 f2 fr/NotTypeFilter? :then subtype-not-type-filter?)
                 (sub-helper f1 f2 fr/NotTypeFilter? :else subtype-not-type-filter?))
            (and (sub-helper f1 f2 fr/NotTypeFilter? :then subtype-not-type-filter?)
                 (sub-helper f1 f2 fr/TypeFilter? :else subtype-type-filter?)))))))

(defn subtype-filter? [f1 f2]
  {:pre [(fr/Filter? f1)
         (not (fr/NoFilter? f1))
         (fr/Filter? f2)
         (not (fr/NoFilter? f2))]
   :post [(boolean? %)]}
  ; assume conjunctive normal form
  ; ie. (V (^ ...) (^ ...))
  (cond
    (= f1 f2) true
    (fr/TopFilter? f2) true
    (fr/BotFilter? f1) true

    (and (fr/TypeFilter? f1)
         (fr/TypeFilter? f2))
    (subtype-type-filter? f1 f2)

    (and (fr/NotTypeFilter? f1)
         (fr/NotTypeFilter? f2))
    (subtype-not-type-filter? f1 f2)
    (fr/AndFilter? f2) (every? (fn [f2*]
                                 (subtype-filter? f1 f2*))
                               (:fs f2))
    (fr/AndFilter? f1) (boolean
                         (some (fn [f1*]
                                 (subtype-filter? f1* f2))
                               (:fs f1)))
    (fr/OrFilter? f1) (every? (fn [f1*]
                                (subtype-filter? f1* f2))
                              (:fs f1))
    (fr/OrFilter? f2) (boolean
                        (some (fn [f2*]
                                (subtype-filter? f1 f2*))
                              (:fs f2)))
    :else false))


(defn subtype-flow-set? [fs1 fs2]
  {:pre [(r/FlowSet? fs1)
         (r/FlowSet? fs2)]}
  (let [n1 (fully-resolve-filter (:normal fs1))
        n2 (fully-resolve-filter (:normal fs2))]
    (= n1 n2)))

(defn subtype-Result
  [{t1 :t f1 :fl o1 :o flow1 :flow :as s}
   {t2 :t f2 :fl o2 :o flow2 :flow :as t}]
  (cond
    ;trivial case
    (and (= o1 o2)
         (subtype-filter-set? f1 f2)
         (subtype-flow-set? flow1 flow2))
    (subtype t1 t2)

    ;we can ignore some interesting results
    (and (orep/EmptyObject? o2)
         (= f2 (fops/-FS fr/-top fr/-top))
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

(defn subtype-TCResult-ignore-type?
  [{f1 :fl o1 :o flow1 :flow :as s}
   {f2 :fl o2 :o flow2 :flow :as t}]
  {:pre [(r/TCResult? s)
         (r/TCResult? t)]
   :post [(boolean? %)]}
  (cond
    ;trivial case
    (and (= o1 o2)
         (subtype-filter-set? f1 f2)
         (subtype-flow-set? flow1 flow2))
    true

    ;we can ignore some interesting results
    (and (orep/EmptyObject? o2)
         (= f2 (fops/-FS fr/-top fr/-top))
         (= flow2 (r/-flow fr/-top)))
    true

    (and (orep/EmptyObject? o2)
         (= f1 f2)
         (= flow2 (r/-flow fr/-top)))
    true

    ;special case for (& (is y sym) ...) <: (is y sym)
    (and (fr/AndFilter? (:then f1))
         (fr/TypeFilter? (:then f2))
         (every? fops/atomic-filter? (:fs (:then f1)))
         (= 1 (count (filter fr/TypeFilter? (:fs (:then f1)))))
         (= fr/-top (:else f2))
         (= flow1 flow2 (r/-flow fr/-top))
         (= o1 o2))
    (let [f1-tf (first (filter fr/TypeFilter? (:fs (:then f1))))]
      (= f1-tf (:then f2)))

    :else false))

(defn subtype-TypeFn-rands?
  [tfn rands1 rands2]
  {:pre [(r/TypeFn? tfn)
         (every? r/Type? rands1)
         (every? r/Type? rands2)]
   :post [(boolean? %)]}
  (and (== (count rands1)
           (count rands2)
           (count (:variances tfn)))
       (every? (fn [[v l r]]
                 {:pre [(r/variance? v)
                        (r/Type? l)
                        (r/Type? r)]}
                 (case v
                   (:covariant) (subtypeA*? *sub-current-seen* l r)
                   (:contravariant) (subtypeA*? *sub-current-seen* r l)
                   (:invariant) (and (subtypeA*? *sub-current-seen* l r)
                                     (subtypeA*? *sub-current-seen* r l))
                   (err/int-error (str "Unknown variance: " v))))
               (map vector (:variances tfn) rands1 rands2))))

(defn subtype-TApp
  [s t]
  (let [s (update s :rator c/fully-resolve-type)
        t (update t :rator c/fully-resolve-type)
        _ (assert (= (:rator s) (:rator t)))]
    (cond
      (and (r/F? (:rator s))
           (r/F? (:rator t)))
      (let [{:keys [upper-bound] :as bnd} (free-ops/free-with-name-bnds (-> s :rator :name))]
        (cond 
          (not bnd) (err/int-error (str "No bounds for " (:name s)))
          :else (let [upper-bound (c/fully-resolve-type upper-bound)]
                  (if (and (r/TypeFn? upper-bound)
                           (subtype-TypeFn-rands? upper-bound (:rands s) (:rands t)))
                    *sub-current-seen*
                    (fail! s t)))))
      (r/TypeFn? (:rator s))
      (let [rator (:rator s)
            variances (:variances rator)
            names (repeatedly (:nbound rator) gensym)
            bbnds (c/TypeFn-bbnds* names rator)]
        (if (and (= (count variances)
                    (count (:rands s))
                    (count (:rands t)))
                 (every? identity
                         (map (fn [variance {:keys [lower-bound upper-bound]} s t]
                                (and (subtype? lower-bound s)
                                     (subtype? lower-bound t)
                                     (subtype? s upper-bound)
                                     (subtype? t upper-bound)
                                     (case variance
                                       :covariant (subtype? s t)
                                       :contravariant (subtype? t s)
                                       :invariant (and (subtype? s t)
                                                       (subtype? t s)))))
                              variances bbnds (:rands s) (:rands t))))
          *sub-current-seen*
          (fail! s t)))
      :else (fail! s t))))

(defn subtype-TypeFn
  [S T]
  {:pre [(r/TypeFn? S)
         (r/TypeFn? T)]}
  (let [;instantiate both type functions with the same names
        names (repeatedly (:nbound S) gensym)
        sbnds (c/TypeFn-bbnds* names S)
        tbnds (c/TypeFn-bbnds* names T)
        sbody (c/TypeFn-body* names S)
        tbody (c/TypeFn-body* names T)]
    (if (and (= (:nbound S) (:nbound T))
             (= (:variances S) (:variances T))
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
  [s t]
  (if (and ;(= (.jtype s) (.jtype t))
           ;contravariant
           (subtype? (:input-type t)
                     (:input-type s))
           ;covariant
           (subtype? (:output-type s)
                     (:output-type t)))
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
  (let [overidden-by (fn [sym o]
                       ;(prn "overriden by" sym (class o) o)
                       (cond
                         ((some-fn r/DataType? r/RClass?) o)
                         (when (#{sym} (:the-class o))
                           o)
                         (r/Protocol? o)
                         ; protocols are extended via their interface if they
                         ; show up in the ancestors of the datatype
                         (when (#{sym} (:on-class o))
                           o)))
        overrides (doall (map c/fully-resolve-type (ancest/get-datatype-ancestors dt)))
        ;_ (prn "datatype name" the-class)
        ;_ (prn "datatype overrides" overrides)
        _ (assert (every? (some-fn r/Protocol? r/DataType? r/RClass?) overrides)
                  "Overriding datatypes to things other than datatypes, protocols and classes NYI")
        ; the classes that this datatype extends.
        ; No vars should occur here because protocol are extend via
        ; their interface.
        normal-asyms (->> (ancestors (coerce/symbol->Class the-class))
                          (filter class?)
                          (map coerce/Class->symbol))
        ;_ (prn "normal-asyms" normal-asyms)
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

(defn ^:private subtype-rclass-protocol
  [s t]
  {:pre [(r/RClass? s)
         (r/Protocol? t)]}
  (impl/assert-clojure)
  (let [;first try and find the datatype in the protocol's extenders
        p-cls-extenders (map coerce/Class->symbol (filter class? (c/Protocol-normal-extenders t)))
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
                   p-cls-extenders (map coerce/Class->symbol (filter class? (c/Protocol-normal-extenders t)))
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
                                         (and (subtype? %2 %3)
                                              (subtype? %3 %2)))
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
(defn coerce-RClass-primitive
  [s t]
  (impl/assert-clojure)
  (cond
    ; (U Integer Long) <: (U int long)
    (and 
      (#{(c/RClass-of Integer) (c/RClass-of Long)} s)
      (#{(c/RClass-of 'int) (c/RClass-of 'long)} t))
    true

    :else
    (let [spcls (coerce/symbol->Class (:the-class s))
          tpcls (coerce/symbol->Class (:the-class t))
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
             (class-isa? scls tcls))

        ;same base class
        (and (= scls tcls)
             (subtype-RClass-common-base s t))

        ;one is a primitive, coerce
        (and (or (.isPrimitive scls)
                 (.isPrimitive tcls))
             (coerce-RClass-primitive s t))

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

(ind-u/add-indirection ind/subtype? subtype?)
