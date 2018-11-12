;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.cs-gen
  (:require [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.filter-rep :as fr]
            [clojure.core.typed.checker.filter-ops :as fo]
            [clojure.core.typed.checker.object-rep :as or]
            ; use subtype? utility defined in this namespace
            [clojure.core.typed.checker.jvm.subtype :as sub]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.core.typed.checker.cs-rep :as cr]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.checker.dvar-env :as denv]
            [clojure.core.typed.checker.frees :as frees]
            [clojure.core.typed.checker.free-ops :as free-ops]
            [clojure.core.typed.checker.promote-demote :as prmt]
            [clojure.core.typed.checker.subst :as subst]
            [clojure.core.typed.checker.indirect-ops :as ind]
            [clojure.core.typed.checker.indirect-utils :as ind-u]
            [clojure.core.typed :as t :refer [letfn>]]
            [clojure.set :as set])
  (:import (clojure.core.typed.checker.type_rep F DataType Function Protocol Bounds FlowSet TCResult HSequential)
           (clojure.core.typed.checker.cs_rep c cset dcon dmap cset-entry)))

(t/typed-deps clojure.core.typed.checker.free-ops
              clojure.core.typed.checker.promote-demote)

(t/ann ^:no-check clojure.core.typed.checker.jvm.subtype/subtype? [r/AnyType r/AnyType -> Boolean])
(t/ann ^:no-check clojure.set/union (t/All [x] [(t/Set x) * -> (t/Set x)]))
(t/ann ^:no-check clojure.core.typed.current-impl/current-impl [-> t/Any])
(t/ann ^:no-check clojure.core.typed.current-impl/checking-clojure? [-> t/Any])

(t/ann gen-repeat [Number (t/Seqable Any) -> (t/Seqable Any)])
(defn ^:private gen-repeat [times repeated]
  (reduce (fn [acc cur]
            (concat acc cur))
          []
          (repeat times repeated)))

; (partition-by-nth 2 [1 2 3 4 5 6]) -> ((1 3 5) (2 4 6))
; (partition-by-nth 3 [1 2 3 4 5 6]) -> ((1 4) (2 5) (3 6))
; util for infer-pdot
(t/ann partition-by-nth (All [a] [Number (t/Seqable a) -> (t/Seq (t/Seq a))]))
(defn partition-by-nth [n lst]
  {:pre [(zero? (rem (count lst) n))]}
  (let [lst-count (count lst)
        keep-rem-of (t/fn keep-rem-of [i :- Number]
                        (keep-indexed (t/fn [index :- Number
                                             item :- a]
                                        (when (= (rem index n) i)
                                          item))
                                      lst))]
    (t/for [i :- Number, (range n)]
      :- (t/Seq a)
      (keep-rem-of i))))

(t/ann subtype? [r/AnyType r/AnyType -> Boolean])
(defn ^:private subtype? [s t]
  (sub/subtype? s t))

(t/ann fail! [t/Any t/Any -> t/Nothing])
(defn fail! [s t]
  (throw u/cs-gen-exn))

(defmacro handle-failure [& body]
  `(u/handle-cs-gen-failure ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint Generation

(t/ann meet [r/Type r/Type -> r/Type])
(defn meet [s t] (c/In s t))

(t/ann join [r/Type r/Type -> r/Type])
(defn join [s t] (c/Un s t))

(t/ann c-meet (t/IFn [c c (t/U nil t/Sym) -> c]
                     [c c -> c]))
(defn c-meet 
  ([c1 c2] (c-meet c1 c2 nil))
  ([{S  :S X  :X T  :T bnds  :bnds :as c1}
    {S* :S X* :X T* :T bnds* :bnds :as c2}
    var]
   (when-not (or var (= X X*))
     (err/int-error (str "Non-matching vars in c-meet:" X X*)))
   (when-not (= bnds bnds*)
     (err/int-error (str "Non-matching bounds in c-meet:" bnds bnds*)))
   (let [;_ (prn "joining" S S* (mapv r/infer-any? [S S*]))
         S (join S S*)
         ;_ (prn "joined:" S)
         ;_ (prn "meeting" T T* (mapv r/infer-any? [T T*]))
         T (meet T T*)
         ;_ (prn "meet:" T)
         ]
     (when-not (subtype? S T)
       (fail! S T))
     (cr/c-maker S (or var X) T bnds))))

(declare dmap-meet)

;FIXME flow error when checking
(t/ann cset-meet [cset cset -> cset])
(defn cset-meet [{maps1 :maps :as x} {maps2 :maps :as y}]
  {:pre [(cr/cset? x)
         (cr/cset? y)]
   :post [(cr/cset? %)]}
  (let [maps (filter (t/inst identity (t/U false cset-entry))
                     (doall (t/for
                              [{map1 :fixed dmap1 :dmap} :- cset-entry, maps1
                               {map2 :fixed dmap2 :dmap} :- cset-entry, maps2]
                              :- (t/U false cset-entry)
                              (handle-failure
                                (cr/cset-entry-maker
                                  (merge-with c-meet map1 map2)
                                  (dmap-meet dmap1 dmap2))))))]
    (when (empty? maps)
      (fail! maps1 maps2))
    (cr/cset-maker maps)))

(t/ann cset-meet* [(t/U nil (t/Seqable cset)) -> cset])
(defn cset-meet* [args]
  {:pre [(every? cr/cset? args)]
   :post [(cr/cset? %)]}
  (reduce cset-meet
          (cr/cset-maker [(cr/cset-entry-maker {} (cr/dmap-maker {}))])
          args))

(t/ann cset-combine [(t/U nil (t/Seqable cset)) -> cset])
(defn cset-combine [l]
  {:pre [(every? cr/cset? l)]}
  (let [mapss (map (-> :maps 
                       (t/inst (t/U nil (t/Seqable cset-entry)))) 
                   l)]
    (-> mapss
        (t/ann-form (t/Seqable (t/U nil (t/Seqable cset-entry)))))
    (cr/cset-maker (apply concat mapss))))

;add new constraint to existing cset
(t/ann insert-constraint [cset t/Sym r/Type r/Type Bounds -> cset])
(defn insert-constraint [cs var S T bnds]
  {:pre [(cr/cset? cs)
         (symbol? var)
         (r/Type? S)
         (r/Type? T)
         (r/Bounds? bnds)]
   :post [(cr/cset? %)]}
  (cr/cset-maker 
    (doall
      (t/for
        [{fmap :fixed dmap :dmap} :- cset-entry, (:maps cs)]
        :- cset-entry
        (cr/cset-entry-maker 
          (assoc fmap var (cr/c-maker S var T bnds))
          dmap)))))

; FIXME no-checked because of massive performance issues. revisit
(t/ann ^:no-check dcon-meet [cr/DCon cr/DCon -> cr/DCon])
(defn dcon-meet [dc1 dc2]
  {:pre [(cr/dcon-c? dc1)
         (cr/dcon-c? dc2)]
   :post [(cr/dcon-c? %)]}
  (cond
    (and (cr/dcon-exact? dc1)
         (or (cr/dcon? dc2) 
             (cr/dcon-exact? dc2)))
    (let [{fixed1 :fixed rest1 :rest} dc1
          {fixed2 :fixed rest2 :rest} dc2]
      (when-not (and rest2 (= (count fixed1) (count fixed2)))
        (fail! fixed1 fixed2))
      (cr/dcon-exact-maker
        (doall
          (let [vector' (t/ann-form vector [c c -> '[c c]])]
            (t/for
              [[c1 c2] :- '[c c], (map vector' fixed1 fixed2)]
              :- c
              (c-meet c1 c2 (:X c1)))))
        (c-meet rest1 rest2 (:X rest1))))
    ;; redo in the other order to call the first case
    (and (cr/dcon? dc1)
         (cr/dcon-exact? dc2))
    (dcon-meet dc2 dc1)

    (and (cr/dcon? dc1)
         (not (:rest dc1))
         (cr/dcon? dc2)
         (not (:rest dc2)))
    (let [{fixed1 :fixed} dc1
          {fixed2 :fixed} dc2]
      (when-not (= (count fixed1) (count fixed2))
        (fail! fixed1 fixed2))
      (cr/dcon-maker
        (doall
          (for [[c1 c2] (map vector fixed1 fixed2)]
            (c-meet c1 c2 (:X c1))))
        nil))

    (and (cr/dcon? dc1)
         (not (:rest dc1))
         (cr/dcon? dc2))
    (let [{fixed1 :fixed} dc1
          {fixed2 :fixed rest :rest} dc2]
      (assert rest)
      (when-not (>= (count fixed1) (count fixed2))
        (fail! fixed1 fixed2))
      (cr/dcon-maker
        (let [vector' (t/inst vector c c t/Any t/Any t/Any t/Any)]
          (doall
            (t/for
              [[c1 c2] :- '[c c], (map vector' fixed1 (concat fixed2 (repeat rest)))]
              :- c
              (c-meet c1 c2 (:X c1)))))
        nil))

    (and (cr/dcon? dc1)
         (cr/dcon? dc2)
         (not (:rest dc2)))
    (dcon-meet dc2 dc1)

    (and (cr/dcon? dc1)
         (cr/dcon? dc2))
    (let [{fixed1 :fixed rest1 :rest} dc1
          {fixed2 :fixed rest2 :rest} dc2
          [shorter longer srest lrest]
          (if (< (count fixed1) (count fixed2))
            [fixed1 fixed2 rest1 rest2]
            [fixed2 fixed1 rest2 rest1])]
      (cr/dcon-maker
        (let [vector' (t/inst vector c c t/Any t/Any t/Any t/Any)]
          (doall
            (t/for
              [[c1 c2] :- '[c c], (map vector' longer (concat shorter (repeat srest)))]
              :- c
              (c-meet c1 c2 (:X c1)))))
        (c-meet lrest srest (:X lrest))))

    (and (cr/dcon-dotted? dc1)
         (cr/dcon-dotted? dc2))
    (let [{fixed1 :fixed c1 :dc {bound1 :name} :dbound} dc1
          {fixed2 :fixed c2 :dc {bound2 :name} :dbound} dc2]
      (when-not (and (= (count fixed1) (count fixed2))
                     (= bound1 bound2))
        (fail! bound1 bound2))
      (cr/dcon-dotted-maker 
        (let [vector' (t/inst vector c c t/Any t/Any t/Any t/Any)]
          (doall 
            (t/for
              [[c1 c2] :- '[c c], (map vector' fixed1 fixed2)]
              :- c
              (c-meet c1 c2 (:X c1)))))
        (c-meet c1 c2 bound1) bound1))

    (and (cr/dcon? dc1)
         (cr/dcon-dotted? dc2))
    (fail! dc1 dc2)

    (and (cr/dcon-dotted? dc1)
         (cr/dcon? dc2))
    (fail! dc1 dc2)

    (and (cr/dcon-repeat? dc1)
         (cr/dcon? dc2)
         (not (:rest dc2)))
    (let [{fixed1 :fixed repeated :repeat} dc1
          {fixed2 :fixed} dc2
          fixed1-count (count fixed1)
          fixed2-count (count fixed2)
          repeat-count (count repeated)
          diff (- fixed2-count fixed1-count)]
      (assert repeated)
      (when-not (and (>= fixed2-count fixed1-count)
                    (zero? (rem diff repeat-count)))
        (fail! fixed1 fixed2))
      (cr/dcon-repeat-maker
        (let [vector' (t/inst vector c c Any Any Any Any)]
          (doall
            (t/for 
              [[c1 c2] :- '[c c], (map vector'
                                       fixed2
                                       (concat fixed1
                                               (gen-repeat (quot diff repeat-count) repeated)))]
              :- c
              (c-meet c1 c2 (:X c1)))))
        repeated))
    (and (cr/dcon-repeat? dc2)
         (cr/dcon? dc1)
         (not (:rest dc1)))
    (dcon-meet dc2 dc1)

    (every? cr/dcon-repeat? [dc1 dc2])
    (let [[{short-fixed :fixed short-repeat :repeat}
           {long-fixed :fixed long-repeat :repeat}]
          (sort-by (fn [x] (-> x :fixed count)) [dc1 dc2])
          s-fixed-count (count short-fixed)
          l-fixed-count (count long-fixed)
          s-repeat-count (count short-repeat)
          l-repeat-count (count long-repeat)
          diff (- l-fixed-count s-fixed-count)
          _ (assert (= s-repeat-count l-repeat-count))
          vector' (t/inst vector c c Any Any Any Any)
          merged-repeat (t/for [[c1 c2] :- '[c c], (map vector' short-repeat long-repeat)]
                              :- c
                              (c-meet c1 c2 (:X c1)))]
      (assert (zero? (rem diff s-repeat-count)))
      (cr/dcon-repeat-maker
        (doall
          (t/for 
            [[c1 c2] :- '[c c], (map vector'
                                     long-fixed
                                     (concat short-fixed
                                             (gen-repeat (quot diff s-repeat-count) short-repeat)))]
            :- c
            (c-meet c1 c2 (:X c1))))
        merged-repeat))

    :else (err/nyi-error (str "NYI dcon-meet " dc1 dc2))))

(t/ann dmap-meet [dmap dmap -> dmap])
(defn dmap-meet [dm1 dm2]
  {:pre [(cr/dmap? dm1)
         (cr/dmap? dm2)]
   :post [(cr/dmap? %)]}
  (cr/dmap-maker (merge-with dcon-meet (:map dm1) (:map dm2))))


;current seen subtype relations, for recursive types
;(Set [Type Type])
(t/ann *cs-current-seen* (t/Set '[r/AnyType r/AnyType]))
(defonce ^:dynamic *cs-current-seen* #{})

(t/defalias NoMentions
  "A set of variables not to mention in the constraints"
  (t/Set t/Sym))

(t/defalias ConstrainVars
  "The map of variables to be constrained to their bounds"
  (t/Map t/Sym Bounds))

;; V : a set of variables not to mention in the constraints
;; X : the map of type variables to be constrained to their bounds
;; Y : the map of index variables to be constrained to their bounds
;; S : a type to be the subtype of T
;; T : a type
;; produces a cset which determines a substitution that makes S a subtype of T
;; implements the V |-_X S <: T => C judgment from Pierce+Turner, extended with
;; the index variables from the TOPLAS paper

(declare cs-gen-right-F cs-gen-left-F cs-gen-datatypes-or-records cs-gen-list
         cs-gen-filter-set cs-gen-object cs-gen-HSequential cs-gen-TApp
         cs-gen-Function cs-gen-FnIntersection cs-gen-Result cs-gen-RClass
         cs-gen-Protocol get-c-from-cmap)

(t/ann ^:no-check cs-gen 
       [(t/Set t/Sym) 
        (t/Map t/Sym Bounds)
        (t/Map t/Sym Bounds)
        r/AnyType
        r/AnyType
        -> cset])
(defn cs-gen [V X Y S T]
  {:pre [((con/set-c? symbol?) V)
         (every? (con/hash-c? symbol? r/Bounds?) [X Y])
         (r/AnyType? S)
         (r/AnyType? T)]
   :post [(cr/cset? %)]}
  ;(prn "cs-gen" S T)
  (if (or (*cs-current-seen* [S T])
          (subtype? S T))
    ;already been around this loop, is a subtype
    (cr/empty-cset X Y)
    (binding [*cs-current-seen* (conj *cs-current-seen* [S T])]
      (cond
        (r/Top? T)
        (cr/empty-cset X Y)

        ;IMPORTANT: handle frees first
        (and (r/F? S)
             (contains? X (:name S)))
        (cs-gen-left-F V X Y S T)

        (and (r/F? T)
             (contains? X (:name T)))
        (cs-gen-right-F V X Y S T)
        
        ;values are subtypes of their classes
        (and (r/Value? S)
             (impl/checking-clojure?))
        (let [sval (:val S)]
          (impl/impl-case
            :clojure (if (nil? sval)
                       (fail! S T)
                       (cs-gen V X Y
                               (apply c/In (c/RClass-of (class sval))
                                      (cond 
                                        ;keyword values are functions
                                        (keyword? sval) [(c/keyword->Fn sval)]
                                        ;strings have a known length as a seqable
                                        (string? sval) [(r/make-ExactCountRange (count sval))]))
                               T))
            :cljs (cond
                    (integer? sval) (cs-gen V X Y (r/CLJSInteger-maker) T)
                    (number? sval) (cs-gen V X Y (r/JSNumber-maker) T)
                    (string? sval) (cs-gen V X Y (r/JSString-maker) T)
                    (boolean? sval) (cs-gen V X Y (r/JSBoolean-maker) T)
                    (symbol? sval) (cs-gen V X Y (c/DataType-of 'cljs.core/Symbol) T)
                    (keyword? sval) (cs-gen V X Y (c/DataType-of 'cljs.core/Keyword) T)
                    :else (fail! S T))))

        (r/Name? S)
        (cs-gen V X Y (c/resolve-Name S) T)

        (r/Name? T)
        (cs-gen V X Y S (c/resolve-Name T))

        (and (r/TApp? S)
             (r/TApp? T)
             (= (:rator S) (:rator T)))
        (cs-gen-TApp V X Y S T)

        (and (r/TApp? S)
             (not (r/F? (:rator S))))
        (cs-gen V X Y (c/resolve-TApp S) T)

        (and (r/TApp? T)
             (not (r/F? (:rator T))))
        (cs-gen V X Y S (c/resolve-TApp T))

        ; copied from TR's infer-unit
        ;; if we have two mu's, we rename them to have the same variable
        ;; and then compare the bodies
        ;; This relies on (B 0) only unifying with itself, and thus only hitting the first case of this `match'
        (and (r/Mu? S)
             (r/Mu? T))
        (cs-gen V X Y (r/Mu-body-unsafe S) (r/Mu-body-unsafe T))

        ;; other mu's just get unfolded
        (r/Mu? S) (cs-gen V X Y (c/unfold S) T)
        (r/Mu? T) (cs-gen V X Y S (c/unfold T))

        ;; similar to Mu+Mu case
        (and (r/Poly? S)
             (r/Poly? T)
             (= (:nbound S) (:nbound T))
             (= (:bbnds S) (:bbnds T)))
        (cs-gen V X Y (r/Poly-body-unsafe* S) (r/Poly-body-unsafe* T))


        ;constrain *each* element of S to be below T, and then combine the constraints
        (r/Union? S)
        (cset-meet*
          (cons (cr/empty-cset X Y)
                (mapv #(cs-gen V X Y % T) (:types S))))

        ;; find *an* element of T which can be made a supertype of S
        (r/Union? T)
        (if-let [cs (seq (filter identity (mapv #(handle-failure (cs-gen V X Y S %))
                                                (:types T))))]
          (cset-combine cs)
          (fail! S T))

        ; Does it matter which order the Intersection cases go?

        ;constrain *every* element of T to be above S, and then meet the constraints
        ; we meet instead of cset-combine because we want all elements of T to be under
        ; S simultaneously.
        (r/Intersection? T)
        (let [ts (sub/simplify-In T)]
          (cset-meet*
            (cons (cr/empty-cset X Y)
                  (mapv #(cs-gen V X Y S %) ts))))

        ;; find *an* element of S which can be made a subtype of T
        (r/Intersection? S)
        (let [ss (sub/simplify-In S)]
          (if-let [cs (some #(handle-failure (cs-gen V X Y % T))
                            ss)]
            (do ;(prn "intersection S normal case" (map prs/unparse-type [S T]))
                cs)
            (fail! S T)))

        (and (r/Extends? S)
             (r/Extends? T))
        (let [;_ (prn "Extends" (prs/unparse-type S) (prs/unparse-type T)
              ;       V X Y)
              ; FIXME handle negative information
              cs (cset-meet*
                   (doall
                     ; for each element of T, we need at least one element of S that works
                     (for [t* (:extends T)]
                       (if-let [results (doall
                                          (seq (filter identity
                                                       (map #(handle-failure
                                                               (cs-gen V X Y % t*))
                                                            (:extends S)))))]
                         (cset-meet* results)
                         (fail! S T)))))]
          cs)

        ;; find *an* element of S which can be made a subtype of T
        ;; we don't care about what S does *not* implement, so we don't
        ;; use the "without" field of Extends
        (r/Extends? S)
        (if-let [cs (some #(handle-failure (cs-gen V X Y % T))
                          (:extends S))]
          cs
          (fail! S T))

        ;constrain *every* element of T to be above S, and then meet the constraints
        ; also ensure T's negative information is reflected in S
        (r/Extends? T)
        (let [cs (cset-meet*
                   (cons (cr/empty-cset X Y)
                         (mapv #(cs-gen V X Y S %) (:extends T))))
              satisfies-without? (not-any? identity 
                                           (doall
                                             (map #(handle-failure (cs-gen V X Y % T))
                                                  (:without T))))]
          (if satisfies-without?
            cs
            (fail! S T)))


        (r/App? S)
        (cs-gen V X Y (c/resolve-App S) T)

        (r/App? T)
        (cs-gen V X Y S (c/resolve-App T))

        ;; constrain body to be below T, but don't mention the new vars
        (r/Poly? S)
        (let [nms (c/Poly-fresh-symbols* S)
              body (c/Poly-body* nms S)
              bbnds (c/Poly-bbnds* nms S)]
          (free-ops/with-bounded-frees (zipmap (map r/F-maker nms) bbnds)
                   (cs-gen (set/union (set nms) V) X Y body T)))

        (and (r/DataType? S)
             (r/DataType? T)) (cs-gen-datatypes-or-records V X Y S T)

        ; handle Record as HMap
        (r/Record? S) (cs-gen V X Y (c/Record->HMap S) T)

        (and (r/HSequential? S)
             (r/HSequential? T))
        (cs-gen-HSequential V X Y S T)

        (and (r/HeterogeneousMap? S)
             (r/HeterogeneousMap? T))
    ; assumes optional/mandatory/absent keys are disjoint
        (let [Skeys (set (keys (:types S)))
              Tkeys (set (keys (:types T)))
              Soptk (set (keys (:optional S)))
              Toptk (set (keys (:optional T)))
              Sabsk (:absent-keys S)
              Tabsk (:absent-keys T)]
          ; All keys must be values
          (when-not (every? r/Value? 
                            (concat
                              Skeys Tkeys
                              Soptk Toptk
                              Sabsk Tabsk))
            (fail! S T))
          ; If the right is complete, the left must also be complete
          (when (c/complete-hmap? T)
            (when-not (c/complete-hmap? S)
              (fail! S T)))
          ; check mandatory keys
          (if (c/complete-hmap? T)
            ; If right is complete, mandatory keys must be identical
            (when-not (= Tkeys Skeys)
              (fail! S T))
            ; If right is partial, all mandatory keys on the right must also appear mandatory on the left
            (when-not (empty? (set/difference Tkeys 
                                Skeys))
              (fail! S T)))
          ; All optional keys on the right must appear either absent, mandatory or optional
          ; on the left
          (when-not (empty? (set/difference Toptk 
                              (set/union Skeys 
                                         Soptk 
                                         Sabsk)))
            (fail! S T))
          ; All absent keys on the right must appear absent on the left
          (when-not (empty? (set/difference Tabsk
                              Sabsk))
            (fail! S T))
          ; now check the values with cs-gen
          (let [;only check mandatory entries that appear on the right
                check-mandatory-keys Tkeys
                Svals (map (:types S) check-mandatory-keys)
                Tvals (map (:types T) check-mandatory-keys)
                _ (assert (every? r/Type? Svals))
                _ (assert (every? r/Type? Tvals))
                ;only check optional entries that appear on the right
                ; and also appear as mandatory or optional on the left
                check-optional-keys (set/intersection
                                      Toptk (set/union Skeys Soptk))
                Sopts (map (some-fn (:types S) (:optional S)) check-optional-keys)
                Topts (map (:optional T) check-optional-keys)
                _ (assert (every? r/Type? Sopts))
                _ (assert (every? r/Type? Topts))]
            (cset-meet* [(cs-gen-list V X Y Svals Tvals)
                         (cs-gen-list V X Y Sopts Topts)])))

        ; covariant, as in TS and GClosure
        (and (r/JSObj? S)
             (r/JSObj? T))
        (let [{Stypes :types} S
              {Ttypes :types} T
              Svals (map Stypes (keys Ttypes))
              Tvals (vals Ttypes)]
          (if (every? r/Type? Svals)
            (cs-gen-list V X Y Svals Tvals)
            (fail! S T)))

        (and (r/GetType? S)
             (not (r/F? (:target S))))
        (cs-gen V X Y (c/-resolve S) T)

        (and (r/GetType? T)
             (not (r/F? (:target T))))
        (cs-gen V X Y S (c/-resolve T))

        (and (r/AssocType? S)
             (r/AssocType? T))
        (let [{S-target :target S-entries :entries S-dentries :dentries} S
              {T-target :target T-entries :entries T-dentries :dentries} T
              cg #(cs-gen V X Y %1 %2)
              target-cset (cg S-target T-target)
              S-entries (reduce concat S-entries)
              T-entries (reduce concat T-entries)
              entries-cset (cs-gen-list V X Y S-entries T-entries)
              _ (when (and S-dentries T-dentries)
                  (err/nyi-error "NYI dentries of Assoc in cs-gen"))
              ]
          (cset-meet* [target-cset entries-cset]))

        (and (r/AssocType? S)
             (r/RClass? T)
             ; (Map xx yy)
             (= 'clojure.lang.IPersistentMap (:the-class T)))
        (let [{:keys [target entries dentries]} S
              {:keys [poly? the-class]} T
              dentries-cset (when-let [{dty :pre-type dbound :name} dentries]
                              (when (and dbound (not (Y dbound)))
                                (fail! S T))
                              ;(println "passed when")
                              (let [merged-X (merge X {dbound (Y dbound)})
                                    get-list-of-c (fn get-list-of-c [t-list]
                                                    (mapv #(get-c-from-cmap % dbound)
                                                          (t/for [t :- r/Type, t-list]
                                                                :- cset
                                                                (cs-gen V merged-X Y dty t))))
                                    repeat-c (get-list-of-c poly?)]
                                (assoc-in (cr/empty-cset X Y)
                                          [:maps 0 :dmap :map dbound]
                                          ; don't constrain on fixed, otherwise will fail
                                          ; on (assoc m x y)
                                          (cr/dcon-repeat-maker [] repeat-c))))
              ;_ (println "dentries-cset" dentries-cset)

              ; if it's nil, we also accept it
              map-cset (when-not (sub/subtype? target r/-nil)
                         (cs-gen V X Y target T))
              entries-keys (map first entries)
              entries-vals (map second entries)
              cg #(cs-gen V X Y %1 %2)
              key-cset (map cg entries-keys (repeat (first poly?)))
              val-cset (map cg entries-vals (repeat (second poly?)))]
          (cset-meet* (concat (when map-cset [map-cset]) key-cset val-cset)))

        ; transform Record to HMap, this is not so useful until we can do
        ; cs-gen Assoc with dentries with HMap
        (and (r/AssocType? S)
             (r/Record? T))
        (let [{:keys [target]} S
              target-cset (cs-gen V X Y target T)
              cset (cs-gen V X Y S (c/Record->HMap T))]
          (cset-meet* [target cset]))

; Completeness matters:
;
; (Assoc x ':a Number ':b Long) <: (HMap {:a Number :b Long} :complete? true)
; (Assoc x ':a Number ':b Long ':c Foo) <!: (HMap {:a Number :b Long} :complete? true)
        (and (r/AssocType? S)
             (r/HeterogeneousMap? T))
        (let [;_ (prn "cs-gen Assoc HMap")
              {:keys [target entries dentries]} S
              {:keys [types absent-keys]} T
              _ (when-not (nil? dentries) (err/nyi-error (pr-str "NYI cs-gen of dentries AssocType with HMap " S T)))
              Assoc-keys (map first entries)
              Tkeys (keys types)
              ; All keys must be keyword values
              _ (when-not (every? c/keyword-value? (concat Tkeys Assoc-keys absent-keys))
                  (fail! S T))
              ; All keys explicitly not in T should not appear in the Assoc operation
              absents-satisfied?
              (if (c/complete-hmap? T)
                ; if T is partial, we just need to ensure the absent keys in T
                ; don't appear in the entries of the Assoc.
                (empty?
                  (set/intersection
                    (set absent-keys)
                    (set (map first entries))))
                ; if T is complete, all entries of the Assoc should *only* have
                ; keys that are mandatory keys of T.
                (empty?
                  (set/difference
                    (set (map first entries))
                    (set Tkeys))))
              _ (when-not absents-satisfied?
                  (fail! S T))
              ;; Isolate the entries of Assoc in a new HMap, with a corresponding expected HMap.
              ; keys on the right overwrite those on the left.
              assoc-args-hmap (c/make-HMap :mandatory (into {} entries))
              expected-assoc-args-hmap (c/make-HMap :mandatory (select-keys (:types assoc-args-hmap) (set Assoc-keys)))
              
              ;; The target of the Assoc needs all the keys not explicitly Assoc'ed.
              expected-target-hmap 
              (let [types (select-keys (into {} entries)
                                       (set/difference (set Assoc-keys) (set Tkeys)))]
                (if (c/complete-hmap? T) 
                  (c/-complete-hmap types)
                  (c/-partial-hmap types absent-keys)))
              
              ;_ (prn assoc-args-hmap :< expected-assoc-args-hmap)
              ;_ (prn (:target S) :< expected-target-hmap)
              ]
          (cs-gen-list V X Y
                       [assoc-args-hmap 
                        (:target S)]
                       [expected-assoc-args-hmap
                        expected-target-hmap]))

        (and (r/AssocType? S)
             (r/HeterogeneousVector? T))
        (let [elem-type (apply c/Un
                               (concat
                                 (:types T)
                                 (when-let [rest (:rest T)]
                                   [rest])
                                 (when (:drest T)
                                   [r/-any])))
              vec-any (r/-hvec [] :rest r/-any)
              num-type (c/RClass-of 'java.lang.Number)
              target-cset (cs-gen V X Y (:target S) vec-any)
              entries-key (map first (:entries S))
              entries-val (map second (:entries S))
              key-cset (cs-gen-list V X Y entries-key (repeat (count entries-key)
                                                              num-type))
              ;_ (println "key-cset" key-cset)
              val-cset (cs-gen-list V X Y entries-val (repeat (count entries-val)
                                                              elem-type))
              ;_ (println "val-cset" val-cset)
              dentries-cset (when-let [{dty :pre-type dbound :name} (:dentries S)]
                              (when (and dbound (not (Y dbound)))
                                (fail! S T))
                              ;(println "passed when")
                              (let [merged-X (merge X {dbound (Y dbound)})
                                    get-list-of-c (fn get-list-of-c [t-list]
                                                    (mapv #(get-c-from-cmap % dbound)
                                                          (t/for [t :- r/Type, t-list]
                                                            :- cset
                                                            (cs-gen V merged-X Y dty t))))
                                    repeat-c (get-list-of-c [num-type elem-type])]
                                (assoc-in (cr/empty-cset X Y)
                                          [:maps 0 :dmap :map dbound]
                                          ; don't constrain on fixed, otherwise will fail
                                          ; on (assoc m x y)
                                          (cr/dcon-repeat-maker [] repeat-c))))
              ]
          (cset-meet* (concat [target-cset key-cset val-cset]
                              (when dentries-cset [dentries-cset]))))

        (and (r/PrimitiveArray? S)
             (r/PrimitiveArray? T)
             (impl/checking-clojure?))
        (cs-gen-list 
          V X Y
          ;input contravariant
          ;output covariant
          [(:input-type T) (:output-type S)]
          [(:input-type S) (:output-type T)])

        ; some RClass's have heterogeneous vector ancestors (in "unchecked ancestors")
        ; It's useful to also trigger this case with HSequential, as that's more likely
        ; to be on the right.
        (and (r/RClass? S)
             (r/HSequential? T))
        (if-let [[Sv] (seq
                        (filter r/HSequential? (map c/fully-resolve-type (c/RClass-supers* S))))]
          (cs-gen V X Y Sv T)
          (fail! S T))
        
        (and (r/FnIntersection? S)
             (r/FnIntersection? T))
        (cs-gen-FnIntersection V X Y S T)

;; extract IFn unchecked ancestor
        (and (r/RClass? S)
             (c/ifn-ancestor S)
             (r/FnIntersection? T))
        (cs-gen V X Y (c/ifn-ancestor S) T)

        (and (r/Function? S)
             (r/Function? T))
        (cs-gen-Function V X Y S T)

        (and (r/Result? S)
             (r/Result? T))
        (cs-gen-Result V X Y S T)

        (and (r/Value? S)
             (r/AnyValue? T))
        (cr/empty-cset X Y)

        ; TODO add :repeat support
        (and (r/HSequential? S)
             (r/RClass? T))
        (cs-gen V X Y
                (let [ss (apply c/Un
                                (concat
                                  (:types S)
                                  (when-let [rest (:rest S)]
                                    [rest])
                                  (when (:drest S)
                                    [r/-any])))]
                  (c/In (impl/impl-case
                          :clojure (case (:kind S)
                                     :vector (c/RClass-of clojure.lang.APersistentVector [ss])
                                     :seq (c/RClass-of clojure.lang.ISeq [ss])
                                     :list (c/RClass-of clojure.lang.IPersistentList [ss])
                                     :sequential (c/In (c/RClass-of clojure.lang.IPersistentCollection [ss])
                                                       (c/RClass-of clojure.lang.Sequential)))
                          :cljs (throw (Exception. "TODO CLJS HSequential cs-gen")))
                        ((if (or (:rest S) (:drest S)) r/make-CountRange r/make-ExactCountRange)
                         (count (:types S)))))
                T)


        (and (r/RClass? S)
             (r/RClass? T))
        (cs-gen-RClass V X Y S T)

        (and (r/Protocol? S)
             (r/Protocol? T))
        (cs-gen-Protocol V X Y S T)

        (r/HeterogeneousMap? S)
        (let [new-S (c/upcast-hmap S)]
          (cs-gen V X Y new-S T))

        (r/HSet? S)
        (let [new-S (c/upcast-hset S)]
          (cs-gen V X Y new-S T))

        (r/HSequential? S)
        (cs-gen V X Y (c/upcast-HSequential S) T)

        (and (r/AssocType? S)
             (r/Protocol? T))
        (cs-gen V X Y (:target S) T)

        :else
        (do (when-not (subtype? S T) 
              (fail! S T))
            (cr/empty-cset X Y))))))

(declare var-store-take move-vars-to-dmap)

(t/ann cs-gen-HSequential [NoMentions ConstrainVars ConstrainVars HSequential HSequential
                           -> cset])
(defn cs-gen-HSequential
  [V X Y S T]
  {:pre [(r/HSequential? S)
         (r/HSequential? T)]
   :post [(cr/cset? %)]}
  (when-not (r/compatible-HSequential-kind? (:kind S) (:kind T))
    (fail! S T))
  (cset-meet* (concat
                (cond
                  ;simple case
                  (not-any? (some-fn :rest :drest :repeat) [S T])
                  [(cs-gen-list V X Y (:types S) (:types T))]

                  ;rest on right, optionally on left
                  (and (:rest T)
                       (not-any? (some-fn :drest :repeat) [S]))
                  (concat [(cs-gen-list V X Y (:types S) (concat (:types T)
                                                                 (repeat (- (count (:types S))
                                                                            (count (:types T)))
                                                                         (:rest T))))]
                          (when (:rest S)
                            [(cs-gen V X Y (:rest S) (:rest T))]))

                  ; repeat on right, nothing on left
                  (and (:repeat T)
                       (not-any? (some-fn :rest :drest :repeat) [S]))
                  (let [s-types (:types S)
                        t-types (:types T)
                        s-types-count (count s-types)
                        t-types-count (count t-types)]
                    (if (and (>= s-types-count t-types-count)
                             (zero? (rem s-types-count t-types-count)))
                      [(cs-gen-list V X Y s-types (gen-repeat (/ s-types-count
                                                                 t-types-count)
                                                              t-types))]
                      (fail! S T)))

                  ; repeat on left, rest on right
                  (and (:repeat S)
                       (:rest T))
                  (let [s-types (:types S)
                        t-types (:types T)
                        s-types-count (count s-types)
                        t-types-count (count t-types)]
                    (if (>= s-types-count t-types-count)
                      [(cs-gen-list V X Y s-types (concat t-types
                                                          (repeat (- s-types-count
                                                                     t-types-count)
                                                                  (:rest T))))]
                      (err/nyi-error (pr-str "NYI HSequential inference " S T))))

                  ; repeat on left, drest on right
                  (and (:repeat S)
                       (:drest T))
                  (let [{t-dty :pre-type dbound :name} (:drest T)
                        _ (when-not (Y dbound)
                            (fail! S T))
                        merged-X (merge X {dbound (Y dbound)})
                        get-list-of-c (fn get-list-of-c [S-list]
                                        (mapv #(get-c-from-cmap % dbound)
                                              (t/for [s :- r/Type, S-list]
                                                    :- cset
                                                    (cs-gen V merged-X Y s t-dty))))
                        repeat-c (get-list-of-c (:types S))]
                    [(assoc-in (cr/empty-cset X Y) [:maps 0 :dmap :map dbound] (cr/dcon-repeat-maker [] repeat-c))])

                  ;; dotted on the left, nothing on the right
                  (and (:drest S)
                       (not-any? (some-fn :rest :drest :repeat) [T]))
                  (let [{dty :pre-type dbound :name} (:drest S)]
                    (when-not (Y dbound)
                      (fail! S T))
                    (when-not (<= (count (:types S)) (count (:types T)))
                      (fail! S T))
                    (let [vars (var-store-take dbound dty (- (count (:types T))
                                                             (count (:types S))))
                          new-tys (doall (t/for
                                           [var :- t/Sym, vars] :- r/AnyType
                                           (subst/substitute (r/make-F var) dbound dty)))
                          new-s-hsequential (r/-hsequential (concat (:types S) new-tys))
                          new-cset (cs-gen-HSequential V 
                                                    ;move dotted lower/upper bounds to vars
                                                    (merge X (zipmap vars (repeat (Y dbound)))) Y new-s-hsequential T)]
                      [(move-vars-to-dmap new-cset dbound vars)]))

                  ;; dotted on the right, nothing on the left
                  (and (not-any? (some-fn :rest :drest :repeat) [S])
                       (:drest T))
                  (let [{dty :pre-type dbound :name} (:drest T)]
                    (when-not (Y dbound)
                      (fail! S T))
                    (when-not (<= (count (:types T)) (count (:types S)))
                      (fail! S T))
                    (let [vars (var-store-take dbound dty (- (count (:types S)) (count (:types T))))
                          new-tys (doall
                                    (t/for
                                       [var :- t/Sym, vars] :- r/AnyType
                                       (subst/substitute (r/make-F var) dbound dty)))
                          new-t-hsequential (r/-hsequential (concat (:types T) new-tys))
                          new-cset (cs-gen-HSequential V 
                                                       ;move dotted lower/upper bounds to vars
                                                       (merge X (zipmap vars (repeat (Y dbound)))) Y S new-t-hsequential)]
                      [(move-vars-to-dmap new-cset dbound vars)]))

                  ;TODO cases
                  :else (err/nyi-error (pr-str "NYI HSequential inference " S T)))
                (map (fn [fs1 fs2]
                       (cs-gen-filter-set V X Y fs1 fs2))
                     (:fs S) (:fs T))
                (map (fn [o1 o2]
                       (cs-gen-object V X Y o1 o2))
                     (:objects S) (:objects T)))))

;; FIXME - anything else to say about And and OrFilters?
(t/ann cs-gen-filter [NoMentions ConstrainVars ConstrainVars fr/Filter fr/Filter
                      -> cset])
(defn cs-gen-filter [V X Y s t]
  {:pre [((con/set-c? symbol?) V)
         (every? (con/hash-c? symbol? r/Bounds?) [X Y])
         (fr/Filter? s)
         (fr/Filter? t)]
   :post [(cr/cset? %)]}
  (cond
    (= s t) (cr/empty-cset X Y)
    (fr/TopFilter? t) (cr/empty-cset X Y)

    (and (fr/TypeFilter? s)
         (fr/TypeFilter? t)
         (and (= (:path s) (:path t))
              (= (:id s) (:id t))))
    (cset-meet (cs-gen V X Y (:type s) (:type t))
               (cs-gen V X Y (:type t) (:type s)))

    (and (fr/NotTypeFilter? s)
         (fr/NotTypeFilter? t)
         (and (= (:path s) (:path t))
              (= (:id s) (:id t))))
    (cset-meet (cs-gen V X Y (:type s) (:type t))
               (cs-gen V X Y (:type t) (:type s)))

    ; simple case for unifying x and y in (& (is x sym) ...) (is y sym)
;    (and (fr/AndFilter? s)
;         (fr/TypeFilter? t)
;         (every? fo/atomic-filter? (:fs s))
;         (= 1 (count (filter fr/TypeFilter? (:fs s)))))
;    (let [tf (first (filter fr/TypeFilter? (:fs s)))]
;      (cs-gen-filter V X Y tf t))
    :else (fail! s t)))

;must be *latent* flow sets
(t/ann cs-gen-flow-set [NoMentions ConstrainVars ConstrainVars FlowSet FlowSet
                        -> cset])
(defn cs-gen-flow-set [V X Y s t]
  {:pre [((con/set-c? symbol?) V)
         (every? (con/hash-c? symbol? r/Bounds?) [X Y])
         (r/FlowSet? s)
         (r/FlowSet? t)]
   :post [(cr/cset? %)]}
  (cond
    (= s t) (cr/empty-cset X Y)
    :else
    (let [{n1 :normal} s
          {n2 :normal} t]
      (cs-gen-filter V X Y n1 n2))))

;must be *latent* filter sets
(t/ann cs-gen-filter-set [NoMentions ConstrainVars ConstrainVars fr/Filter fr/Filter
                          -> cset])
(defn cs-gen-filter-set [V X Y s t]
  {:pre [((con/set-c? symbol?) V)
         (every? (con/hash-c? symbol? r/Bounds?) [X Y])
         (fr/FilterSet? s)
         (fr/FilterSet? t)]
   :post [(cr/cset? %)]}
  (cond
    (= s t) (cr/empty-cset X Y)
    :else
    (let [{s+ :then s- :else} s
          {t+ :then t- :else} t]
      (cset-meet (cs-gen-filter V X Y s+ t+)
                 (cs-gen-filter V X Y s- t-)))))

(t/ann cs-gen-object [NoMentions ConstrainVars ConstrainVars
                      or/RObject or/RObject -> cset])
(defn cs-gen-object [V X Y s t]
  {:pre [((con/set-c? symbol?) V)
         (every? (con/hash-c? symbol? r/Bounds?) [X Y])
         (or/RObject? s)
         (or/RObject? t)]
   :post [(cr/cset? %)]}
  (cond
    (= s t) (cr/empty-cset X Y)
    (or/EmptyObject? t) (cr/empty-cset X Y)
    ;;FIXME do something here
    :else (fail! s t)))

(declare cs-gen-Function cs-gen-list-with-variances)

(defn cs-gen-TApp
  [V X Y S T]
  {:pre [(r/TApp? S)
         (r/TApp? T)
         (= (:rator S) (:rator T))]}
  (let [tfn (-> T :rator c/fully-resolve-type)]
    (assert (r/TypeFn? tfn) "Found something other than a TFn in a TApp")
    (cs-gen-list-with-variances 
      V X Y
      (:variances tfn)
      (:rands S) (:rands T))))

(defn cs-gen-FnIntersection
  [V X Y S T] 
  {:pre [(r/FnIntersection? S)
         (r/FnIntersection? T)]}
  ;(prn "cs-gen FnIntersections")
  (cset-meet*
    (doall
      (t/for
        [t-arr :- Function, (:types T)] :- cset
        ;; for each t-arr, we need to get at least s-arr that works
        (let [results (filter (t/inst identity (t/U false cset))
                              (doall
                                (t/for
                                  [s-arr :- Function, (:types S)] :- (t/U false cset)
                                  (let [r (handle-failure
                                            (cs-gen-Function V X Y s-arr t-arr))]
                                    r))))
              ;_ (prn "results" (count results))
              ;_ (clojure.pprint/pprint results) 
              ;_ (flush)
              ;; ensure that something produces a constraint set
              _ (when (empty? results) 
                  (fail! S T))
              comb (cset-combine results)]
          ;(prn "combined" comb)
          comb)))))

(defn cs-gen-Result
  [V X Y S T] 
  {:pre [(r/Result? S)
         (r/Result? T)]}
  (cset-meet* [(cs-gen V X Y (r/Result-type* S) (r/Result-type* T))
               (cs-gen-filter-set V X Y (r/Result-filter* S) (r/Result-filter* T))
               (cs-gen-object V X Y (r/Result-object* S) (r/Result-object* T))
               (cs-gen-flow-set V X Y (r/Result-flow* S) (r/Result-flow* T))]))

(t/ann cs-gen-datatypes-or-records
       [NoMentions ConstrainVars ConstrainVars DataType DataType -> cset])
(defn cs-gen-datatypes-or-records 
  [V X Y S T]
  {:pre [(every? r/DataType? [S T])]}
  (when-not (= (:the-class S) (:the-class T)) 
    (fail! S T))
  (if (seq (:poly? S))
    (cs-gen-list-with-variances V X Y (:variances T) (:poly? S) (:poly? T))
    (cr/empty-cset X Y)))

; constrain si and ti according to variance
(t/ann cs-gen-with-variance [NoMentions ConstrainVars ConstrainVars r/Variance
                             r/AnyType r/AnyType -> cset])
(defn cs-gen-with-variance
  [V X Y variance si ti]
  {:pre [(r/variance? variance)
         (r/AnyType? si)
         (r/AnyType? ti)]
   :post [(cr/cset? %)]}
  (case variance
    (:covariant :constant) (cs-gen V X Y si ti)
    :contravariant (cs-gen V X Y ti si)
    :invariant (cset-meet (cs-gen V X Y si ti)
                          (cs-gen V X Y ti si))))

;constrain lists of types ss and ts according to variances
(t/ann cs-gen-list-with-variances 
       [NoMentions ConstrainVars ConstrainVars (t/U nil (t/Seqable r/Variance))
        (t/U nil (t/Seqable r/AnyType)) (t/U nil (t/Seqable r/AnyType)) -> cset])
(defn cs-gen-list-with-variances
  [V X Y variances ss ts]
  {:pre [(every? r/variance? variances)
         (every? r/AnyType? ss)
         (every? r/AnyType? ts)
         (apply = (count variances) (map count [ss ts]))]
   :post [(cr/cset? %)]}
  (cset-meet*
    (cons (cr/empty-cset X Y)
          (doall
            (map (t/fn [variance :- r/Variance
                        si :- r/AnyType 
                        ti :- r/AnyType]
                   (cs-gen-with-variance V X Y variance si ti))
                 variances ss ts)))))

(defn cs-gen-RClass
  [V X Y S T]
  {:pre [(r/RClass? S)
         (r/RClass? T)]}
  (let [rsupers (c/RClass-supers* S)
        relevant-S (some #(when (r/RClass? %)
                            (and (= (:the-class %) (:the-class T))
                                 %))
                         (map c/fully-resolve-type (conj rsupers S)))]
  ;  (prn "S" (prs/unparse-type S))
  ;  (prn "T" (prs/unparse-type T))
;    (prn "supers" (map (juxt prs/unparse-type class) rsupers))
;    (when relevant-S
  ;    (prn "relevant-S" (prs/unparse-type relevant-S)))
    (cond
      relevant-S
      (cs-gen-list-with-variances 
        V X Y
        (:variances T)
        (:poly? relevant-S)
        (:poly? T))
      :else (fail! S T))))

(defn cs-gen-Protocol
  [V X Y S T]
  {:pre [(r/Protocol? S)
         (r/Protocol? T)]}
  (t/ann-form [S T] (t/Seqable Protocol))
  (if (= (:the-var S)
         (:the-var T))
    (cset-meet*
      (cons (cr/empty-cset X Y)
            (doall
              (t/for
                [[vari si ti] :- '[r/Variance r/Type r/Type]
                  (map (-> vector
                           (t/inst r/Variance r/Type r/Type t/Any t/Any t/Any))
                       (:variances T)
                       (t/ann-form (:poly? S) (t/U nil (t/Seqable r/Type)))
                       (:poly? T))]
                :- cset
                (case vari
                  (:covariant :constant) (cs-gen V X Y si ti)
                  :contravariant (cs-gen V X Y ti si)
                  :invariant (cset-meet (cs-gen V X Y si ti)
                                        (cs-gen V X Y ti si)))))))
    (fail! S T)))

(t/ann demote-F [NoMentions ConstrainVars ConstrainVars F r/Type -> cset])
(defn demote-F [V X Y {:keys [name] :as S} T]
  {:pre [(r/F? S)]}
  ;constrain T to be below S (but don't mention V)
  (assert (contains? X name) (str X name))
  (when (and (r/F? T)
             (denv/bound-index? (:name T))
             (not (free-ops/free-in-scope (:name T))))
    (fail! S T))
  (let [dt (prmt/demote-var T V)
        bnd (X name)
        _ (assert bnd)]
    (-> (cr/empty-cset X Y)
      (insert-constraint name (r/Bottom) dt bnd))))

(t/ann promote-F [NoMentions ConstrainVars ConstrainVars r/Type F -> cset])
(defn promote-F [V X Y S {:keys [name] :as T}]
  {:pre [(r/F? T)]}
  ;T is an F
  ;S is any Type
  ;constrain T to be above S (but don't mention V)
  (assert (contains? X name) (str X T))
  (when (and (r/F? S)
             (denv/bound-index? (:name S))
             (not (free-ops/free-in-scope (:name S))))
    (fail! S T))
  (let [ps (prmt/promote-var S V)
        bnd (X name)
        _ (assert bnd)]
    (-> (cr/empty-cset X Y)
      (insert-constraint name ps r/-any bnd))))

(t/ann cs-gen-left-F [NoMentions ConstrainVars ConstrainVars F r/Type -> cset])
(defn cs-gen-left-F [V X Y S T]
  {:pre [(r/F? S)]}
  (cond
    (contains? X (:name S))
    (demote-F V X Y S T)

    (and (r/F? T)
         (contains? X (:name T)))
    (promote-F V X Y S T)

    :else (fail! S T)))

(t/ann cs-gen-right-F [NoMentions ConstrainVars ConstrainVars r/Type F -> cset])
(defn cs-gen-right-F [V X Y S T]
  {:pre [(r/F? T)]}
  (cond
    (contains? X (:name T))
    (promote-F V X Y S T)

    (and (r/F? S)
         (contains? X (:name S)))
    (demote-F V X Y S T)

    :else (fail! S T)))

(t/ann singleton-dmap [t/Sym cr/DCon -> dmap])
(defn singleton-dmap [dbound dcon]
  (cr/dmap-maker {dbound dcon}))

(t/ann mover [cset t/Sym (t/U nil (t/Seqable t/Sym)) [cr/CMap cr/DMap -> cr/DCon] -> cset])
(defn mover [cset dbound vars f]
  {:pre [(cr/cset? cset)
         (symbol? dbound)
         (every? symbol? vars)]
   :post [(cr/cset? %)]}
  (cr/cset-maker
    (map
      (t/fn [{cmap :fixed dmap :dmap} :- cset-entry]
        (cr/cset-entry-maker
          (apply dissoc cmap dbound vars)
          (dmap-meet 
            (singleton-dmap 
              dbound
              (f cmap dmap))
            (cr/dmap-maker (dissoc (:map dmap) dbound)))))
      (:maps cset))))

;; dbound : index variable
;; cset : the constraints being manipulated
;FIXME needs no-check for unreachable flow filter error
(t/ann ^:no-check move-rest-to-dmap [cset t/Sym & :optional {:exact (t/U nil true)} -> cset])
(defn move-rest-to-dmap [cset dbound & {:keys [exact]}]
  {:pre [(cr/cset? cset)
         (symbol? dbound)
         ((some-fn nil? true?) exact)]
   :post [(cr/cset? %)]}
  (mover cset dbound nil
         (fn [cmap dmap]
           ((if exact cr/dcon-exact-maker cr/dcon-maker)
              nil
              (if-let [c (cmap dbound)]
                c
                (err/int-error (str "No constraint for bound " dbound)))))))

; FIXME why we need a list of cset-entry?
(t/ann get-c-from-cmap [cset t/Sym -> c])
(defn get-c-from-cmap [cset dbound]
  {:pre [(cr/cset? cset)
         (symbol? dbound)]
   :post [(cr/c? %)]}
  (if-let [result ((-> cset :maps first :fixed) dbound)]
    result
    (err/int-error (str "No constraint for bound " dbound))))

;; dbound : index variable
;; vars : listof[type variable] - temporary variables
;; cset : the constraints being manipulated
;; takes the constraints on vars and creates a dmap entry constraining dbound to be |vars|
;; with the constraints that cset places on vars
(t/ann move-vars-to-dmap [cset t/Sym (t/U nil (t/Seqable t/Sym)) -> cset])
;FIXME no-check, flow error
(defn ^:no-check move-vars-to-dmap [cset dbound vars]
  {:pre [(cr/cset? cset)
         (symbol? dbound)
         (every? symbol? vars)]
   :post [(cr/cset? %)]}
  (mover cset dbound vars
         (fn [cmap dmap]
           (cr/dcon-maker (doall (t/for [v :- t/Sym, vars] :- c
                               (if-let [c (cmap v)]
                                 c
                                 (err/int-error (str "No constraint for new var " v)))))
                      nil))))

;; This one's weird, because the way we set it up, the rest is already in the dmap.
;; This is because we create all the vars, then recall cgen/arr with the new vars
;; in place, and the "simple" case will then call move-rest-to-dmap.  This means
;; we need to extract that result from the dmap and merge it with the fixed vars
;; we now handled.  So I've extended the mover to give access to the dmap, which we use here.
;FIXME no-check because of unreachable flow 
(t/ann ^:no-check move-vars+rest-to-dmap 
       [cset t/Sym (t/U nil (t/Seqable t/Sym)) & :optional {:exact (t/U nil true)} -> cset])
(defn move-vars+rest-to-dmap [cset dbound vars & {:keys [exact]}]
  {:pre [(cr/cset? cset)
         (symbol? dbound)
         (every? symbol? vars)
         ((some-fn nil? true?) exact)]
   :post [(cr/cset? %)]}
  (mover cset dbound vars
         (fn [cmap dmap]
           ((if exact cr/dcon-exact-maker cr/dcon-maker)
              (doall
                (for [v vars]
                  (if-let [c (cmap v)]
                    c
                    (err/int-error (str "No constraint for new var " v)))))
              (if-let [c ((:map dmap) dbound)]
                (cond
                  (and (cr/dcon? c)
                       (not (:fixed c))) (:rest c)
                  (and (cr/dcon-exact? c)
                       (not (:fixed c))) (:rest c)
                  :else (err/int-error (str "did not a get a rest-only dcon when moving to the dmap")))
                (err/int-error (str "No constraint for bound " dbound)))))))

;; Maps dotted vars (combined with dotted types, to ensure global uniqueness)
;; to "fresh" symbols.
;; That way, we can share the same "fresh" variables between the elements of a
;; cset if they're talking about the same dotted variable.
;; This makes it possible to reduce the size of the csets, since we can detect
;; identical elements that would otherwise differ only by these fresh vars.
;; The domain of this map is pairs (var . dotted-type).
;; The range is this map is a list of symbols generated on demand, as we need
;; more dots.
(t/ann DOTTED-VAR-STORE (t/Atom1 (t/Map '[r/Type t/Sym] (t/Seq t/Sym))))
(defonce ^:private DOTTED-VAR-STORE (atom {}))

(t/ann reset-dotted-var-store! [-> nil])
(defn reset-dotted-var-store! []
  (reset! DOTTED-VAR-STORE {})
  nil)

;; Take (generate as needed) n symbols that correspond to variable var used in
;; the context of type t.
;FIXME no-check, trans-dots needs to be generalised
(t/ann ^:no-check var-store-take [t/Sym r/Type t/Int -> (t/Seqable t/Sym)])
(defn- var-store-take [var t n]
  (let [key [t n]
        res (@DOTTED-VAR-STORE key)]
    (if (>= (count res) n)
      ;; there are enough symbols already, take n
      (take n res)
      ;; we need to generate more
      (let [new (take (- n (count res))
                      (repeatedly #(gensym var)))
            all (concat res new)]
        (let [assoc' (t/inst assoc '[r/Type t/Sym] t/Sym t/Any)
              swap!' (t/inst swap! (t/Map '[r/Type t/Sym] t/Sym) (t/Map '[r/Type t/Sym] t/Sym)
                             '[r/Type t/Sym] t/Sym)]
          (swap!' DOTTED-VAR-STORE assoc' key all))
        all))))

(defn pad-right
  "Returns a sequence of length cnt that is s padded to the right with copies
  of v."
  [^long cnt s v]
  {:pre [(integer? cnt)
         (<= (count s) cnt)]
   ;careful not to shadow cnt here
   :post [(== cnt (count %))]}
  (concat s
          (repeat (- cnt (count s)) v)))

(defn cs-gen-Function-just-rests [V X Y S T]
  ;just a rest arg, no drest, no keywords, no prest
  {:pre [(and (some-fn :rest [S T])
              (not-any? (some-fn :drest :kws :prest) [S T]))]}
  (let [arg-mapping (cond
                      ;both rest args are present, so make them the same length
                      (and (:rest S) (:rest T))
                      (cs-gen-list V X Y 
                                   (cons (:rest T) (u/pad-right (count (:dom S)) (:dom T) (:rest T)))
                                   (cons (:rest S) (u/pad-right (count (:dom T)) (:dom S) (:rest S))))
                      ;no rest arg on the right, so just pad left and forget the rest arg
                      (and (:rest S) (not (:rest T)))
                      (let [new-S (u/pad-right (count (:dom T)) (:dom S) (:rest S))]
                        ;                            (prn "infer rest arg on left")
                        ;                            (prn "left dom" (map prs/unparse-type (:dom S)))
                        ;                            (prn "right dom" (map prs/unparse-type (:dom T)))
                        ;                            (prn "new left dom" (map prs/unparse-type new-S))
                        (cs-gen-list V X Y (:dom T) new-S))
                      ;no rest arg on left, or wrong number = fail
                      :else (fail! S T))
        ret-mapping (cs-gen V X Y (:rng S) (:rng T))]
    (cset-meet* [arg-mapping ret-mapping])))

(defn cs-gen-Function-rest-prest [cg V X Y S T]
  {:pre [(and (:rest S)
              (:prest T))]}
  (let [S-dom (:dom S)
        S-dom-count (count S-dom)
        T-dom (:dom T)
        T-dom-count (count T-dom)
        S-rest (:rest S)
        T-prest-types (-> T :prest :types)
        T-prest-types-count (count T-prest-types)
        ret-mapping (cg (:rng S) (:rng T))
        rest-prest-mapping (cs-gen-list V X Y T-prest-types (repeat T-prest-types-count S-rest))]
    (if (> S-dom-count T-dom-count)
      ; hard mode
      (let [[S-dom-short S-dom-rest] (split-at T-dom-count S-dom)
            ;_ (println "in hard-mode of rest prest")
            arg-mapping (cs-gen-list V X Y T-dom S-dom-short)
            remain-repeat-count (rem (count S-dom-rest) T-prest-types-count)
            ceiling (fn [up low]
                      {:pre [(every? integer? [up low])]}
                      (let [result (quot up low)]
                        (if (zero? (rem up low))
                          result
                          (inc result))))
            repeat-times (if (empty? S-dom-rest)
                           0
                           (ceiling (count S-dom-rest) T-prest-types-count))
            repeat-mapping (cs-gen-list V X Y
                                        (concat S-dom-rest
                                                (repeat remain-repeat-count S-rest))
                                        (gen-repeat repeat-times T-prest-types))]
        (cset-meet* [arg-mapping repeat-mapping rest-prest-mapping ret-mapping]))
      ; easy mode
      (let [[T-dom-short T-dom-rest] (split-at S-dom-count T-dom)
            arg-mapping (cs-gen-list V X Y T-dom-short S-dom)
            rest-mapping (cs-gen-list V X Y T-dom-rest (repeat (count T-dom-rest) S-rest))]
        (cset-meet* [arg-mapping rest-mapping rest-prest-mapping ret-mapping])))))

(defn cs-gen-Function-prest-on-left [V X Y S T]
  ; prest on left, nothing on right
  {:pre [(and (:prest S)
              (not-any? (some-fn :rest :drest :kws :prest) [T]))]}
  (let [s-dom (:dom S)
        t-dom (:dom T)
        s-dom-count (count s-dom)
        t-dom-count (count t-dom)]
    (if (and (<= t-dom-count s-dom-count)
             (not (zero? (rem (- t-dom-count s-dom-count)
                              (count (-> S :prest :types))))))
      (fail! S T)
      (let [[short-T rest-T] (split-at s-dom-count t-dom)
            short-cs (cs-gen-list V X Y short-T s-dom)
            s-prest-types (-> S :prest :types)
            rest-S (gen-repeat (/ (count rest-T) (count s-prest-types)) s-prest-types)
            rest-cs (cs-gen-list V X Y rest-T rest-S)
            ret-mapping (cs-gen V X Y (:rng S) (:rng T))]
        (cset-meet* [short-cs rest-cs ret-mapping])))))

(defn cs-gen-Function-prest-drest [cg V X Y S T]
  (let [{t-dty :pre-type dbound :name} (:drest T)
        _ (when-not (Y dbound)
            (fail! S T))
        S-dom (:dom S)
        S-dom-count (count S-dom)
        T-dom (:dom T)
        T-dom-count (count T-dom)
        S-prest-types (-> S :prest :types)
        S-prest-types-count (count S-prest-types)
        merged-X (merge X {dbound (Y dbound)})
        get-list-of-c (fn get-list-of-c [S-list]
                        (mapv #(get-c-from-cmap % dbound)
                              (map
                                (fn [s] (cs-gen V merged-X Y t-dty s))
                                S-list)))
        repeat-c (get-list-of-c S-prest-types)
        ret-mapping (cg (:rng S) (:rng T))]
    (if (<= S-dom-count T-dom-count)
      ; hard mode
      (let [T-rest-count (- T-dom-count S-dom-count)
            [arg-S-prest remain-S-prest] (split-at (rem T-rest-count
                                                        S-prest-types-count) S-prest-types)
            new-S (concat S-dom
                          (gen-repeat (quot T-rest-count S-prest-types-count) S-prest-types)
                          arg-S-prest)
            arg-mapping (cs-gen-list V X Y T-dom new-S)
            fixed-c (if (= (count arg-S-prest) 0)
                      []
                      (get-list-of-c remain-S-prest))
            darg-mapping (assoc-in (cr/empty-cset X Y)
                                   [:maps 0 :dmap :map dbound]
                                   (cr/dcon-repeat-maker fixed-c repeat-c))]
        (cset-meet* [arg-mapping darg-mapping ret-mapping]))
      ; easy mode
      (let [[arg-S rest-S] (split-at T-dom-count S-dom)
            arg-mapping (cs-gen-list V X Y T-dom arg-S)
            fixed-c (get-list-of-c rest-S)
            darg-mapping (assoc-in (cr/empty-cset X Y)
                                   [:maps 0 :dmap :map dbound]
                                   (cr/dcon-repeat-maker fixed-c repeat-c))]
        (cset-meet* [arg-mapping darg-mapping ret-mapping])))))

(defn cs-gen-Function-dotted-left-nothing-right [V X Y S T]
  {:pre [(and (:drest S)
              (not-any? (some-fn :rest :drest :kws :prest) [T]))]}
  (let [{dty :pre-type dbound :name} (:drest S)]
    (when-not (Y dbound)
      (fail! S T))
    (when-not (<= (count (:dom S)) (count (:dom T)))
      (fail! S T))
    (let [vars (var-store-take dbound dty (- (count (:dom T))
                                             (count (:dom S))))
          new-tys (mapv (fn [var]
                          (subst/substitute (r/make-F var) dbound dty))
                        vars)
          new-s-arr (r/Function-maker (concat (:dom S) new-tys) (:rng S) nil nil nil nil nil)
          new-cset (cs-gen-Function V 
                                    ;move dotted lower/upper bounds to vars
                                    (merge X (zipmap vars (repeat (Y dbound)))) Y new-s-arr T)]
      (move-vars-to-dmap new-cset dbound vars))))

(defn cs-gen-Function-dotted-right-nothing-left [V X Y S T]
  {:pre [(and (not-any? (some-fn :rest :drest :kws :prest) [S])
              (:drest T))]}
  (let [{dty :pre-type dbound :name} (:drest T)]
    (when-not (Y dbound)
      (fail! S T))
    (when-not (<= (count (:dom T)) (count (:dom S)))
      (fail! S T))
    (let [vars (var-store-take dbound dty (- (count (:dom S)) (count (:dom T))))
          new-tys (mapv
                    (fn [var]
                      (subst/substitute (r/make-F var) dbound dty))
                    vars)
          ;_ (prn "dotted on the right, nothing on the left")
          ;_ (prn "vars" vars)
          new-t-arr (r/Function-maker (concat (:dom T) new-tys) (:rng T) nil nil nil nil nil)
          ;_ (prn "S" (prs/unparse-type S))
          ;_ (prn "new-t-arr" (prs/unparse-type new-t-arr))
          new-cset (cs-gen-Function V 
                                    ;move dotted lower/upper bounds to vars
                                    (merge X (zipmap vars (repeat (Y dbound)))) Y S new-t-arr)]
      (move-vars-to-dmap new-cset dbound vars))))

;; * <: ...
(defn cs-gen-Function-star-<-dots [cg V X Y S T]
  {:pre [(and (:rest S)
              (:drest T))]}
  (let [{t-dty :pre-type dbound :name} (-> T :drest)]
    (when-not (Y dbound)
      (fail! S T))
    (if (<= (count (:dom S)) (count (:dom T)))
      ;; the simple case
      (let [arg-mapping (cs-gen-list V X Y (:dom T) (u/pad-right (count (:dom T)) (:dom S) (:rest S)))
            darg-mapping (move-rest-to-dmap (cs-gen V (merge X {dbound (Y dbound)}) Y t-dty (:rest S)) dbound)
            ret-mapping (cg (:rng S) (:rng T))]
        (cset-meet* [arg-mapping darg-mapping ret-mapping]))
      ;; the hard case
      (let [vars (var-store-take dbound t-dty (- (count (:dom S)) (count (:dom T))))
            new-tys (mapv (fn [var]
                            (subst/substitute (r/make-F var) dbound t-dty))
                          vars)
            new-t-arr (r/Function-maker (concat (:dom T) new-tys) (:rng T) nil (r/DottedPretype1-maker t-dty dbound) nil nil nil)
            new-cset (cs-gen-Function V (merge X (zipmap vars (repeat (Y dbound))) X) Y S new-t-arr)]
        (move-vars+rest-to-dmap new-cset dbound vars)))))

;; ... <: *
; Typed Racket notes that this might not be a correct subtyping case?
(defn cs-gen-Function-dots-<-star [cg V X Y S T]
  {:pre [(and (:drest S)
              (:rest T))]}
  (let [{s-dty :pre-type dbound :name} (-> S :drest)]
    (when-not (Y dbound)
      (fail! S T))
    (cond 
      (< (count (:dom S)) (count (:dom T)))
      ;; the hard case
      (let [vars (var-store-take dbound s-dty (- (count (:dom T)) (count (:dom S))))
            new-tys (mapv (fn [var]
                            (subst/substitute (r/make-F var) dbound s-dty))
                          vars)
            new-s-arr (r/Function-maker (concat (:dom S) new-tys) (:rng S) nil (r/DottedPretype1-maker s-dty dbound) nil nil nil)
            new-cset (cs-gen-Function V (merge X (zipmap vars (repeat (Y dbound))) X) Y new-s-arr T)]
        (move-vars+rest-to-dmap new-cset dbound vars :exact true))

      (== (count (:dom S)) (count (:dom T)))
      ;the simple case
      (let [arg-mapping (cs-gen-list V X Y (u/pad-right (count (:dom S)) (:dom T) (:rest T)) (:dom S))
            darg-mapping (move-rest-to-dmap (cs-gen V (merge X {dbound (Y dbound)}) Y (:rest T) s-dty) dbound :exact true)
            ret-mapping (cg (:rng S) (:rng T))]
        (cset-meet* [arg-mapping darg-mapping ret-mapping]))

      :else (fail! S T))))

(t/ann ^:no-check cs-gen-Function
       [NoMentions ConstrainVars ConstrainVars Function Function -> cset])
(defn cs-gen-Function
  [V X Y S T]
  {:pre [((con/set-c? symbol?) V)
         (every? (con/hash-c? symbol? r/Bounds?) [X Y])
         (r/Function? S)
         (r/Function? T)]
   :post [(cr/cset? %)]}
  ;(prn "cs-gen-Function" (prs/unparse-type S) (prs/unparse-type T))
  (letfn> [cg :- [r/AnyType r/AnyType -> cset]
           (cg [S T] (cs-gen V X Y S T))]
    (cond
      ;easy case - no rests, drests, kws, prest
      (not-any? (some-fn :rest :drest :kws :prest) [S T])
      ; contravariant
      (let [;_ (prn "easy case")
            ]
        (cset-meet* [(cs-gen-list V X Y (:dom T) (:dom S))
                     ; covariant
                     (cg (:rng S) (:rng T))]))

      ;just a rest arg, no drest, no keywords, no prest
      (and (some-fn :rest [S T])
           (not-any? (some-fn :drest :kws :prest) [S T]))
      (cs-gen-Function-just-rests V X Y S T)

      ; :rest is less restricted than :prest
      (and (:prest S)
           (:rest T)
           (> (-> S :prest :types count) 1))
      (fail! S T)

      (and (:rest S)
           (:prest T))
      (cs-gen-Function-just-rests cg V X Y S T)

      ; prest on left, nothing on right
      (and (:prest S)
           (not-any? (some-fn :rest :drest :kws :prest) [T]))
      (cs-gen-Function-prest-on-left V X Y S T)

      ; prest on left, drest on right
      (and (:prest S)
           (:drest T))
      (cs-gen-Function-prest-drest cg V X Y S T)

      ;; dotted on the left, nothing on the right
      (and (:drest S)
           (not-any? (some-fn :rest :drest :kws :prest) [T]))
      (cs-gen-Function-dotted-left-nothing-right V X Y S T)

      ;; dotted on the right, nothing on the left
      (and (not-any? (some-fn :rest :drest :kws :prest) [S])
           (:drest T))
      (cs-gen-Function-dotted-right-nothing-left V X Y S T)

      ;; * <: ...
      (and (:rest S)
           (:drest T))
      (cs-gen-Function-star-<-dots cg V X Y S T)

      ;; ... <: *
      ; Typed Racket notes that this might not be a correct subtyping case?
      (and (:drest S)
           (:rest T))
      (cs-gen-Function-dots-<-star cg V X Y S T)

      :else 
      (err/nyi-error (pr-str "NYI Function inference " (prs/unparse-type S) (prs/unparse-type T))))))

;; C : cset? - set of constraints found by the inference engine
;; Y : (setof symbol?) - index variables that must have entries
;; R : Type? - result type into which we will be substituting
;TODO no-check, very slow!
(t/ann ^:no-check subst-gen [cset (t/Set t/Sym) r/AnyType & :optional {:T (t/U nil (t/Seqable r/Type))} -> (t/U nil cr/SubstMap)])
(defn subst-gen [C Y R & {:keys [T]}]
  {:pre [(cr/cset? C)
         ((con/set-c? symbol?) Y)
         (r/AnyType? R)
         (every? r/Type? T)]
   :post [((some-fn nil? cr/substitution-c?) %)]}
  (let [var-hash (apply frees/combine-frees
                        (frees/fv-variances R)
                        (mapv frees/fv-variances T))
        idx-hash (frees/idx-variances R)]
    (letfn> 
           [
            ;; v : Symbol - variable for which to check variance
            ;; h : (Hash F Variance) - hash to check variance in (either var or idx hash)
            ;; variable: Symbol - variable to use instead, if v was a temp var for idx extension
            constraint->type :- [c frees/VarianceMap & :optional {:variable (t/U nil t/Sym)} -> r/Variance]
            (constraint->type [{{:keys [upper-bound lower-bound]} :bnds :keys [S X T] :as v} h & {:keys [variable]}]
              {:pre [(cr/c? v)
                     (frees/variance-map? h)
                     ((some-fn nil? symbol?) variable)]}
              (when-not (subtype? S T) (fail! S T))
              (when (some r/TypeFn? [upper-bound lower-bound]) (err/nyi-error "Higher kinds"))
              (let [var (h (or variable X) :constant)
                    inferred (case var
                               (:constant :covariant) S
                               :contravariant T
                               ;; FIXME unsure if In is correct here
                               :invariant (c/In S T))]
                inferred))
            ;TODO implement generalize
            ;                  (let [gS (generalize S)]
            ;                    (if (subtype? gS T)
            ;                      gS
            ;                      S))

            ;; Since we don't add entries to the empty cset for index variables (since there is no
            ;; widest constraint, due to dcon-exacts), we must add substitutions here if no constraint
            ;; was found.  If we're at this point and had no other constraints, then adding the
            ;; equivalent of the constraint (dcon null (c Bot X Top)) is okay.
            extend-idxs :- [cr/SubstMap -> (t/U nil cr/SubstMap)]
            (extend-idxs [S]
              {:pre [(cr/substitution-c? S)]}
              (let [fi-R (frees/fi R)] ;free indices in R
                ;; If the index variable v is not used in the type, then
                ;; we allow it to be replaced with the empty list of types;
                ;; otherwise we error, as we do not yet know what an appropriate
                ;; lower bound is.
                (letfn> [demote-check-free :- [t/Sym -> cr/SubstRHS]
                         (demote-check-free [v]
                           {:pre [(symbol? v)]}
                           (if (fi-R v)
                             (err/int-error "attempted to demote dotted variable")
                             (cr/i-subst-maker nil)))]
                  ;; absent-entries is false if there's an error in the substitution, otherwise
                  ;; it's a list of variables that don't appear in the substitution
                  (let [absent-entries
                        (reduce (t/fn [no-entry :- t/Any
                                       v :- t/Sym]
                                  {:pre [(symbol? v)]}
                                  (let [entry (S v)]
                                    ;; Make sure we got a subst entry for an index var
                                    ;; (i.e. a list of types for the fixed portion
                                    ;;  and a type for the starred portion)
                                    (cond
                                      (false? no-entry) no-entry
                                      (not entry) (cons v no-entry)
                                      (or (cr/i-subst? entry)
                                          (cr/i-subst-starred? entry)
                                          (cr/i-subst-dotted? entry)) no-entry
                                      :else false)))
                                [] Y)]
                    (and absent-entries
                         (merge (into {}
                                      (map
                                        (fn [missing]
                                          (let [var (idx-hash missing :constant)]
                                            [missing
                                             (case var
                                               (:constant :covariant :invariant) (demote-check-free missing)
                                               :contravariant (cr/i-subst-starred-maker nil r/-any))]))
                                        absent-entries))
                                S))))))]

      (let [{cmap :fixed dmap* :dmap} (if-let [c (-> C :maps first)]
                                        c
                                        (err/int-error "No constraints found"))
            ; Typed Racket arbitrarily picks the first constraint here, we follow.
            ;
            ;_ (when-not (= 1 (count (:maps C))) 
            ;    (err/int-error "More than one constraint set found"))
            dm (:map dmap*)
            subst (merge 
                    (into {}
                          (map
                            (fn [[k dc]]
                              (cond
                                (and (cr/dcon? dc) (not (:rest dc)))
                                [k (cr/i-subst-maker 
                                     (mapv
                                       (fn [f]
                                         (constraint->type f idx-hash :variable k))
                                       (:fixed dc)))]
                                (and (cr/dcon? dc) (:rest dc))
                                [k (cr/i-subst-starred-maker 
                                     (mapv
                                       (fn [f]
                                         (constraint->type f idx-hash :variable k))
                                       (:fixed dc))
                                     (constraint->type (:rest dc) idx-hash))]
                                (cr/dcon-exact? dc)
                                [k (cr/i-subst-starred-maker 
                                     (mapv
                                       (fn [f]
                                         (constraint->type f idx-hash :variable k))
                                       (:fixed dc))
                                     (constraint->type (:rest dc) idx-hash))]
                                (cr/dcon-dotted? dc)
                                [k (cr/i-subst-dotted-maker
                                     (mapv
                                       (fn [f]
                                         (constraint->type f idx-hash :variable k))
                                       (:fixed dc))
                                     (constraint->type (:dc dc) idx-hash :variable k)
                                     (:dbound dc))]

                                (cr/dcon-repeat? dc)
                                [k (cr/i-subst-maker
                                     (mapv
                                       (fn [f]
                                         (constraint->type f idx-hash :variable k))
                                       (:fixed dc)))]

                                :else (err/int-error (prn-str "What is this? " dc)))))
                          dm)

                    (into {}
                          (map
                            (fn [[k v]]
                              [k (cr/t-subst-maker
                                   (constraint->type v var-hash)
                                   (:bnds v))]))
                          cmap))
            ;check delayed constraints and type variable bounds
            _ (let [t-substs (into {} (filter (t/fn [[_ v] :- '[t/Sym cr/SubstRHS]]
                                                (cr/t-subst? v))
                                              subst))
                    [names images] (let [s (seq t-substs)]
                                     [(map first s)
                                      (map (comp :type second) s)])]
                (t/doseq [[nme {inferred :type :keys [bnds]}] :- '[t/Sym t-subst], t-substs]
                  (when (some r/TypeFn? [(:upper-bound bnds) (:lower-bound bnds)]) (err/nyi-error "Higher kinds"))
                  (let [lower-bound (subst/substitute-many (:lower-bound bnds) images names)
                        upper-bound (subst/substitute-many (:upper-bound bnds) images names)]
                    (cond
                      (not (subtype? lower-bound upper-bound))
                      (fail! lower-bound upper-bound)

                      (not (subtype? inferred upper-bound))
                      (fail! inferred upper-bound)

                      (not (subtype? lower-bound inferred))
                      (fail! lower-bound inferred)))))]
        ;; verify that we got all the important variables
        (when-let [r (and (every? identity
                                  (map
                                    (fn [v]
                                      (let [entry (subst v)]
                                        (and entry (cr/t-subst? entry))))
                                    (frees/fv R)))
                          (extend-idxs subst))]
          r)))))

;; V : a set of variables not to mention in the constraints
;; X : the set of type variables to be constrained mapped to their bounds
;; Y : the set of index variables to be constrained mapped to their bounds
;; S : a list of types to be the subtypes of T
;; T : a list of types
;; expected-cset : a cset representing the expected type, to meet early and
;;  keep the number of constraints in check. (empty by default)
;; produces a cset which determines a substitution that makes the Ss subtypes of the Ts
(t/ann cs-gen-list
       [NoMentions ConstrainVars ConstrainVars 
        (t/U nil (t/Seqable r/Type)) (t/U nil (t/Seqable r/Type))
        & :optional {:expected-cset (t/U nil cset)}
        -> cset])
(defn cs-gen-list [V X Y S T & {:keys [expected-cset] :or {expected-cset (cr/empty-cset {} {})}}]
  {:pre [((con/set-c? symbol?) V)
         (every? (con/hash-c? symbol? r/Bounds?) [X Y])
         (every? r/Type? (concat S T))
         (cr/cset? expected-cset)]
   :post [(cr/cset? %)]}
;  (prn "cs-gen-list" 
;       V X Y
;       (map prs/unparse-type S)
;       (map prs/unparse-type T))
  (when-not (= (count S) (count T))
    (fail! S T))
  (cset-meet*
    ;; We meet early to prune the csets to a reasonable size.
    ;; This weakens the inference a bit, but sometimes avoids
    ;; constraint explosion.
    (cons
      (cr/empty-cset X Y)
      (let [vector' (t/inst vector t/Any r/Type r/Type)]
        (doall 
          (t/for [[s t] :- '[r/Type r/Type], (map vector' S T)] 
            :- cset
            (let [c (cs-gen V X Y s t)
                  ;_ (prn "csgen-list 1")
                  ;_ (prn "V" V)
                  ;_ (prn "X" X)
                  ;_ (prn "Y" Y)
                  ;_ (prn "s" (prs/unparse-type s))
                  ;_ (prn "t" (prs/unparse-type t))
                  ;_ (prn "c")
                  ;_ (clojure.pprint/pprint c)
                  ;_ (flush)
                  ;_ (prn "expected cset" expected-cset)
                  m (cset-meet c expected-cset)]
              ;(prn "meet:")
              ;(clojure.pprint/pprint m)
              ;(flush)
              m)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Infer

;; like infer, but dotted-var is the bound on the ...
;; and T-dotted is the repeated type
(t/ann infer-dots
  (t/IFn [ConstrainVars 
       t/Sym 
       Bounds
       (t/U nil (t/Seqable r/Type)) 
       (t/U nil (t/Seqable r/Type))
       r/Type 
       (t/U nil r/AnyType) 
       (t/Set t/Sym)
       & :optional {:expected (t/U nil r/Type)} -> cr/SubstMap]))
(defn infer-dots [X dotted-var dotted-bnd S T T-dotted R must-vars & {:keys [expected]}]
  {:pre [((con/hash-c? symbol? r/Bounds?) X)
         (symbol? dotted-var)
         (r/Bounds? dotted-bnd)
         (every? (con/every-c? r/Type?) [S T])
         (r/Type? T-dotted) 
         (r/AnyType? R)
         ((con/set-c? symbol?) must-vars)
         ((some-fn nil? r/Type?) expected)]
   :post [(cr/substitution-c? %)]}
;  (prn "infer-dots")
;  (prn "must-vars" must-vars)
  (let [[short-S rest-S] (split-at (count T) S)
;        _ (prn "short-S" (map prs/unparse-type short-S))
;        _ (prn "T" (map prs/unparse-type T))
;        _ (prn "rest-S" (map prs/unparse-type rest-S))
        expected-cset (if expected
                        (cs-gen #{} X {dotted-var dotted-bnd} R expected)
                        (cr/empty-cset {} {}))
        ;_ (prn "expected-cset" expected-cset)
        cs-short (cs-gen-list #{} X {dotted-var dotted-bnd} short-S T
                                :expected-cset expected-cset)
        ;_ (prn "cs-short" cs-short)
        new-vars (var-store-take dotted-var T-dotted (count rest-S))
        new-Ts (doall
                 (t/for
                   [v :- t/Sym, new-vars] :- r/Type
                   (let [target (subst/substitute-dots (map r/make-F new-vars) nil dotted-var T-dotted)]
                     #_(prn "replace" v "with" dotted-var "in" (prs/unparse-type target))
                     (subst/substitute (r/make-F v) dotted-var target))))
        ;_ (prn "new-Ts" new-Ts)
        cs-dotted (cs-gen-list #{} (merge X (zipmap new-vars (repeat dotted-bnd))) {dotted-var dotted-bnd} rest-S new-Ts
                               :expected-cset expected-cset)
        ;_ (prn "cs-dotted" cs-dotted)
        cs-dotted (move-vars-to-dmap cs-dotted dotted-var new-vars)
        ;_ (prn "cs-dotted" cs-dotted)
        cs (cset-meet cs-short cs-dotted)
        ;_ (prn "cs" cs)
        ]
    ;; FIXME pass variances via :T
    (subst-gen (cset-meet cs expected-cset) #{dotted-var} R)))

(declare infer)

;; like infer-dot, but for infering pdot function
;; T-dotted is the one with `:repeat true`
;; FIXME dotted-bnd seems not that useful, cause it can't constrain on serveral pretypes
;; on pdot
(t/ann infer-pdot
  (Fn [ConstrainVars
       t/Sym
       Bounds
       (U nil (t/Seqable r/Type))
       (U nil (t/Seqable r/Type))
       r/Type
       (U nil r/AnyType)
       (t/Set t/Sym)
       & :optional {:expected (U nil r/Type)} -> cr/SubstMap]))
(defn infer-pdot [X dotted-var dotted-bnd S T T-dotted R must-vars & {:keys [expected]}]
  {:pre [((con/hash-c? symbol? r/Bounds?) X)
         (symbol? dotted-var)
         (r/Bounds? dotted-bnd)
         (every? (con/every-c? r/Type?) [S T])
         (and (r/Type? T-dotted) (:repeat T-dotted) (:types T-dotted))
         (r/AnyType? R)
         ((con/set-c? symbol?) must-vars)
         ((some-fn nil? r/Type?) expected)]
   :post [(cr/substitution-c? %)]}
  ;(prn "infer-pdot")
  (let [[short-S rest-S] (split-at (count T) S)
        _ (when-not (zero? (rem (- (count S) (count T))
                                (-> T-dotted :types count)))
            (fail! S T))
        ;_ (prn "short-S" (map prs/unparse-type short-S))
        ;_ (prn "T" (map prs/unparse-type T))
        ;_ (prn "rest-S" (map prs/unparse-type rest-S))
        ;_ (prn "R" R)
        ;_ (prn "expected" expected)
        expected-cset (if expected
                        (cs-gen #{} X {dotted-var dotted-bnd} R expected)
                        (cr/empty-cset {} {}))
        ;_ (prn "expected-cset" expected-cset)
        cs-short (cs-gen-list #{} X {dotted-var dotted-bnd} short-S T
                              :expected-cset expected-cset)
        ;_ (prn "cs-short" cs-short)
        new-vars (var-store-take dotted-var T-dotted (count rest-S))
        new-Ts (doall
                 (let [list-of-vars (partition-by-nth (-> T-dotted :types count) new-vars)
                       ;_ (println "list-of-vars" list-of-vars)
                       list-of-result (map (fn [vars pre-type]
                                             (for [v vars]
                                               (let [target (subst/substitute-dots
                                                              (map r/make-F vars)
                                                              nil dotted-var pre-type)]
                                                 (subst/substitute (r/make-F v) dotted-var target))))
                                           list-of-vars
                                           (:types T-dotted))]
                   (apply interleave list-of-result)))
        cs-dotted (cs-gen-list #{} (merge X (zipmap new-vars (repeat dotted-bnd)))
                               {dotted-var dotted-bnd} rest-S new-Ts
                               :expected-cset expected-cset)
        ;_ (prn "cs-dotted" cs-dotted)
        cs-dotted (move-vars-to-dmap cs-dotted dotted-var new-vars)
        ;_ (prn "cs-dotted" cs-dotted)
        cs (cset-meet cs-short cs-dotted)
        ;_ (prn "cs" cs)
        ]
    ;; FIXME pass variances via :T
    (subst-gen (cset-meet cs expected-cset) #{dotted-var} R)))

;; like infer-vararg, but T-var is the prest type:
(t/ann infer-prest
  [ConstrainVars ConstrainVars
   (U nil (t/Seqable r/Type)) (U nil (t/Seqable r/Type))
   r/Type (U nil r/AnyType) (U nil TCResult)
   -> (U nil true false cr/SubstMap)])
(defn infer-prest
  [X Y S T T-var R expected]
  {:pre [(every? (con/hash-c? symbol? r/Bounds?) [X Y])
         (every? r/Type? S)
         (every? r/Type? T)
         (r/Type? T-var)
         (r/AnyType? R)
         ((some-fn nil? r/AnyType?) expected)]
   :post [(or (nil? %)
              (cr/substitution-c? %))]}
  #_(println "infer-prest\n"
           "X" X "\n"
           "Y" Y "\n"
           "S" S "\n"
           "T" T "\n"
           "T-var" T-var "\n"
           )
  (and (>= (count S) (count T))
       (let [[short-S rest-S] (split-at (count T) S)
             ; wrap rest-S into HeterogeneousVector, this is semantic meaning of <*
             new-rest-S (r/-hvec (vec rest-S))
             new-S (concat short-S [new-rest-S])
             new-T (concat T [T-var])]
         (infer X Y new-S new-T R expected))))

;; like infer, but T-var is the vararg type:
(t/ann infer-vararg
  (t/IFn [ConstrainVars ConstrainVars 
       (t/U nil (t/Seqable r/Type)) (t/U nil (t/Seqable r/Type))
       (t/U nil r/Type)
       (t/U nil r/AnyType) -> (t/U nil true false cr/SubstMap)]
      [ConstrainVars ConstrainVars 
       (t/U nil (t/Seqable r/Type)) (t/U nil (t/Seqable r/Type))
       (t/U nil r/Type)
       (t/U nil r/AnyType) (t/U nil TCResult) -> (t/U nil true false cr/SubstMap)]))
(defn infer-vararg 
  ([X Y S T T-var R] (infer-vararg X Y S T T-var R nil))
  ([X Y S T T-var R expected]
   {:pre [(every? (con/hash-c? symbol? r/Bounds?) [X Y])
          (every? r/Type? S)
          (every? r/Type? T)
          ((some-fn nil? r/Type?) T-var)
          (r/AnyType? R)
          ((some-fn nil? r/AnyType?) expected)]
    :post [(or (nil? %)
               (cr/substitution-c? %))]}
   ;(prn "infer-vararg" "X:" X)
   (let [new-T (if T-var
                 ;Pad out T
                 (concat T (repeat (- (count S) (count T)) T-var))
                 T)]
     ;    (prn "S" (map unparse-type S))
     ;    (prn "new-T" (map unparse-type new-T))
     ;    (prn "R" (unparse-type R))
     ;    (prn "expected" (class expected) (when expected (unparse-type expected)))
     (and (>= (count S) (count T))
          (infer X Y S new-T R expected)))))

;; X : variables to infer mapped to their bounds
;; Y : indices to infer mapped to their bounds
;; S : actual argument types
;; T : formal argument types
;; R : result type
;; expected : #f or the expected type
;; returns a substitution
;; if R is nil, we don't care about the substituion
;; just return a boolean result
(t/ann infer
  (t/IFn [ConstrainVars ConstrainVars 
       (t/U nil (t/Seqable r/Type)) (t/U nil (t/Seqable r/Type))
       (t/U nil r/AnyType) -> (t/U nil true cr/SubstMap)]
      [ConstrainVars ConstrainVars 
       (t/U nil (t/Seqable r/Type)) (t/U nil (t/Seqable r/Type))
       (t/U nil r/AnyType) (t/U nil TCResult) -> (t/U nil true cr/SubstMap)]))
(defn infer 
  ([X Y S T R] (infer X Y S T R nil))
  ([X Y S T R expected]
   {:pre [(every? (con/hash-c? symbol? r/Bounds?) [X Y])
          (every? r/Type? S)
          (every? r/Type? T)
          (r/AnyType? R)
          ((some-fn nil? r/AnyType?) expected)]
    :post [(or (nil? %)
               (true? %)
               (cr/substitution-c? %))]}
   ;  (prn "infer" )
   ;  (prn "X:" X) 
   ;  (prn "Y:" Y) 
   ;  (prn "S:" (map prs/unparse-type S))
   ;  (prn "T:" (map prs/unparse-type T))
   ;  (when R
   ;    (prn "R:" (class R) (prs/unparse-type R)))
   ;  (when expected
   ;    (prn "expected:" (class expected) (prs/unparse-type expected)))
   (let [;_ (prn "before expected cset" R expected)
         expected-cset (if expected
                         (cs-gen #{} X Y R expected)
                         (cr/empty-cset {} {}))
         ;_ (prn "expected cset" expected-cset)
         cs (cs-gen-list #{} X Y S T :expected-cset expected-cset)
         cs* (cset-meet cs expected-cset)]
     ;(prn "final cs" cs*)
     (if R
       (subst-gen cs* (set (keys Y)) R :T T)
       true))))

(ind-u/add-indirection ind/infer infer)

(defmacro unify-or-nil [{:keys [fresh out]} s t]
  (assert (every? simple-symbol? fresh))
  (assert (apply distinct? fresh))
  (assert (some #{out} fresh))
  (let [fresh-names (map (juxt identity gensym) fresh)
        name-lookup (into {} fresh-names)]
    `(let [[lhs# rhs#] (let ~(vec
                               (mapcat (fn [[fresh name]]
                                         [fresh `(r/make-F '~name)])
                                       fresh-names))
                         [~s ~t])
           _# (assert (r/Type? lhs#))
           _# (assert (r/Type? rhs#))
           substitution#
           (handle-failure
             (infer
               (zipmap '~(map second fresh-names) (repeat r/no-bounds))
               {}
               [lhs#]
               [rhs#]
               r/-any))
           res# (when substitution#
                  (when-let [s# (get substitution# '~(name-lookup out))]
                    (:type s#)))
           _# (assert ((some-fn nil? r/Type?) res#))]
       res#)))

(comment
  (impl/with-clojure-impl
         (let [x (gensym)]
           (infer {x r/no-bounds} {} 
                  [(c/RClass-of clojure.lang.IPersistentCollection [(c/RClass-of Number)])]
                  [(c/RClass-of clojure.lang.Seqable [(r/make-F x)])]
                  r/-any)))
  (map prs/unparse-type (c/RClass-supers* (c/RClass-of clojure.lang.IPersistentCollection [(c/RClass-of Number)])))
  )
