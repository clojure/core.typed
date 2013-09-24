(ns ^:skip-wiki clojure.core.typed.type-ctors
  (:refer-clojure :exclude [defrecord])
  (:require [clojure.core.typed.utils :as u :refer [p]]
            [clojure.core.typed.impl-protocols :as p]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.filter-rep :as fr]
            [clojure.core.typed.object-rep :as or]
            [clojure.core.typed.path-rep :as path]
            [clojure.core.typed.rclass-env :as rcls]
            [clojure.core.typed.cs-rep :as crep]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.fold-rep :as f]
            [clojure.core.typed.datatype-env :as dtenv]
            [clojure.core.typed.protocol-env :as prenv]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed :as t :refer [fn>]]
            [clojure.math.combinatorics :as comb]
            [clojure.set :as set]
            [clojure.reflect :as reflect]
            [clojure.repl :as repl])

  (:import (clojure.core.typed.type_rep HeterogeneousMap Poly TypeFn PolyDots TApp App Value
                                        Union Intersection F Function Mu B KwArgs KwArgsSeq RClass
                                        Bounds Name Scope CountRange Intersection DataType Extends
                                        JSNominal Protocol)
           (clojure.lang Seqable IPersistentSet IPersistentMap Symbol Keyword
                         Atom Var)))

(alter-meta! *ns* assoc :skip-wiki true)

; create an alias cr -> cs-rep
(t/tc-ignore
  (create-ns 'clojure.core.typed.cs-rep)
  (alias 'cr 'clojure.core.typed.cs-rep)
  )

(t/tc-ignore
(defn- unparse-type-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.parse-unparse)
                      'unparse-type)]
    (assert (var? v) "unparse-type unbound")
    v))

  (defn- unparse-type [t]
    (@(unparse-type-var) t))
  )

(declare Un make-Union)

(t/ann make-Union [(U nil (Seqable r/Type)) -> r/Type])
(defn- make-Union
  "Arguments should not overlap or be unions"
  [args]
  (cond
    (= 1 (count args)) (first args)
    :else (r/Union-maker (set args))))

(t/ann bottom r/Type)
(def ^:private bottom (make-Union []))

;; Heterogeneous maps

(t/ann ^:no-check -hmap (Fn [(Seqable r/Type) -> r/Type]
                           [(Seqable r/Type) Boolean -> r/Type]
                           [(Seqable r/Type) (IPersistentSet r/Type) Boolean -> r/Type]))
(defn -hmap 
  ([types] (-hmap types #{} true))
  ([types other-keys?] (-hmap types #{} other-keys?))
  ([types absent-keys other-keys?]
   (if (or ; simplify to bottom if an entry is bottom
           (some #{bottom} (concat (keys types) (vals types) absent-keys))
           ; contradictory overlap in present/absent keys
           (seq (set/intersection (set (keys types)) (set absent-keys))))
     bottom
     (r/HeterogeneousMap-maker types absent-keys other-keys?))))

(t/ann -complete-hmap [(Seqable r/Type) -> r/Type])
(defn -complete-hmap [types]
  (-hmap types false))

(t/def-alias TypeMap
  "A regular map with types as keys and vals."
  (IPersistentMap r/Type r/Type))

(t/ann ^:no-check make-HMap (Fn [TypeMap TypeMap -> r/Type]
                               [TypeMap TypeMap Any -> r/Type]))
(defn make-HMap 
  "Generate a type which is every possible combination of mandatory
  and optional key entries. Takes an optional third parameter which
  is true if the entries are complete (ie. we know there are no more entries),
  and false otherwise. Defaults to false.
  
  Options:
  - :absent-keys  a set of types that are not keys this/these maps"
  ([mandatory optional]
   (make-HMap mandatory optional false))
  ([mandatory optional complete? & {:keys [absent-keys]}]
   ; simplifies to bottom with contradictory options
   (if (seq (set/intersection (-> mandatory keys set)
                              (-> optional keys set)
                              (set absent-keys)))
     (make-Union [])
     (make-Union 
            (remove
              #{(make-Union [])}
              (for [ss (map #(into {} %) (comb/subsets optional))]
                (let [new-mandatory (merge mandatory ss)
                      ;other optional keys cannot appear...
                      new-absent (set/union
                                   (set/difference (set (keys optional))
                                                   (set (keys ss)))
                                   (set absent-keys))
                      ;...but we don't know about other keys
                      new-other-keys? (not complete?)]
                  (-hmap new-mandatory new-absent new-other-keys?))))))))

;TODO to type check this, need to un-munge instance field names
(t/ann complete-hmap? [HeterogeneousMap -> Any])
(defn complete-hmap? [^HeterogeneousMap hmap]
  {:pre [(r/HeterogeneousMap? hmap)]}
  (not (.other-keys? hmap)))

;; Unions

(t/tc-ignore
(defn- subtype?-var []
  (let [n (find-ns 'clojure.core.typed.subtype)
        _ (assert n "subtype ns doesn't exist")
        v (ns-resolve n 'subtype?)]
    (assert (var? v) "subtype? unbound")
    v))
  )

(t/def-alias TypeCache 
  (IPersistentMap (IPersistentSet r/Type) r/Type))

(t/ann ^:no-check Un-cache (t/Atom1 TypeCache))
(defonce Un-cache (atom {}))

(t/ann ^:no-check reset-Un-cache [-> nil])
(defn reset-Un-cache []
  (reset! Un-cache {})
  nil)

(declare flatten-unions)

(t/ann ^:no-check Un [r/Type * -> r/Type])
(defn Un [& types]
  ;(prn "Un" (map @(unparse-type-var) types))
  (if-let [hit (p :Union-cache-lookup (@Un-cache (p :Union-calc-hash (set (map r/type-id types)))))]
    (do (p :Un-cache-hit)
        hit)
  (p :type-ctors/Un-ctor
  (let [_ (p :Un-cache-miss)
        res (let [subtype? @(subtype?-var)]
              (letfn [;; a is a Type (not a union type)
                      ;; b is a Set[Type] (non overlapping, non Union-types)
                      ;; The output is a non overlapping list of non Union types.
                      (merge-type [a b]
                        {:pre [(set? b)
                               (do (assert (r/Type? a) a)
                                   true)
                               (not (r/Union? a))]
                         :post [(set? %)]}
                        #_(prn "merge-type" a b)
                        (let [b* (make-Union b)
                              ;_ (prn "merge-type" (@(unparse-type-var) a) (@(unparse-type-var) b*))
                              res (cond
                                    ; don't resolve type applications in case types aren't
                                    ; fully defined yet
                                    ; TODO basic error checking, eg. number of params
                                    (some (some-fn r/Name? r/TApp?) (conj b a)) (conj b a)
                                    (subtype? a b*) b
                                    (subtype? b* a) #{a}
                                    :else (conj b a))]
                          ;(prn "res" res)
                          res))]
                (let [types (set types)]
                  (cond
                    (empty? types) r/empty-union
                    (= 1 (count types)) (first types)
                    :else 
                    (p :Un-merge-type 
                       (make-Union
                         (reduce (fn [acc t] (p :Un-merge-type-inner (merge-type t acc)))
                                 #{}
                                 (p :Un-flatten-unions 
                                    (set (flatten-unions types))))))))))]
    (swap! Un-cache assoc (set (map r/type-id types)) res)
    res))))

;; Intersections

(declare overlap In)

(t/ann In-cache (t/Atom1 TypeCache))
(defonce In-cache (atom {}))

(t/ann intersect-cache (t/Atom1 TypeCache))
(defonce intersect-cache (atom {}))

(t/ann reset-In-cache [-> nil])
(defn reset-In-cache []
  (reset! In-cache {})
  (reset! intersect-cache {})
  nil)

(t/ann ^:no-check make-Intersection [(U nil (Seqable r/Type)) -> r/Type])
(defn make-Intersection [types]
  #_(prn "make-Intersection" types)
  (r/Intersection-maker (set types)))

(declare RClass-of)

(t/ann ^:no-check intersect [r/Type r/Type -> r/Type])
(defn intersect [t1 t2]
  {:pre [(r/Type? t1)
         (r/Type? t2)
         (not (r/Union? t1))
         (not (r/Union? t2))]
   :post [(r/Type? %)]}
  (let [subtype? @(subtype?-var)]
    ;(prn "intersect" (map unparse-type [t1 t2]))
    (if-let [hit (@intersect-cache (set [(r/type-id t1) (r/type-id t2)]))]
      (do
        ;(prn "intersect hit" (unparse-type hit))
        (p :intersect-cache-hit)
        hit)
      (let [_ (p :intersect-cache-miss)
            t (cond
                (and (r/HeterogeneousMap? t1)
                     (r/HeterogeneousMap? t2))
                (-hmap
                  (merge-with In
                              (:types t1)
                              (:types t2))
                  (set/union (:absent-keys t1)
                             (:absent-keys t2))
                  (or (:other-keys? t1)
                      (:other-keys? t2)))

                ;RClass's with the same base, intersect args pairwise
                (and (r/RClass? t1)
                     (r/RClass? t2)
                     (= (:the-class t1) (:the-class t2)))
                (let [args (doall (map intersect (:poly? t1) (:poly? t2)))]
                  ; if a new arg is bottom when none of the old args are bottom,
                  ; reduce type to bottom
                  (if (some (fn [[new [old1 old2]]]
                              (and (every? (complement #{(Un)}) [old1 old2])
                                   (#{(Un)} new)))
                            (map vector args (map vector (:poly? t1) (:poly? t2))))
                    (Un)
                    (RClass-of (:the-class t1) args)))

                (not (overlap t1 t2)) bottom

                (subtype? t1 t2) t1
                (subtype? t2 t1) t2
                :else (do
                        #_(prn "failed to eliminate intersection" (@(unparse-type-var) (make-Intersection [t1 t2])))
                        (make-Intersection [t1 t2])))]
        (swap! intersect-cache assoc (set [(r/type-id t1) (r/type-id t2)]) t)
        ;(prn "intersect miss" (unparse-type t))
        t))))

(t/ann ^:no-check flatten-intersections [(U nil (Seqable r/Type)) -> (Seqable r/Type)])
(defn flatten-intersections [types]
  {:pre [(every? r/Type? types)]
   :post [(every? r/Type? %)]}
  (apply concat
         (for [t types]
           (if (r/Intersection? t)
             (:types t)
             [t]))))

(t/ann ^:no-check flatten-unions [(U nil (Seqable r/Type)) -> (Seqable r/Type)])
(defn flatten-unions [types]
  {:pre [(every? r/Type? types)]
   :post [(every? (every-pred r/Type? (complement r/Union?)) %)]}
  (apply concat
         (for [t (set types)]
           (if (r/Union? t)
             (:types t)
             [t]))))

(t/ann ^:no-check In [r/Type * -> r/Type])
(defn In [& types]
  {:pre [(every? r/Type? types)]
   :post [(r/Type? %)]}
  ;(prn "In" (map @(unparse-type-var) types))
;  (if-let [hit (@In-cache (set types))]
;    (do #_(prn "In hit" (@(unparse-type-var) hit))
;        hit)
  (p :type-ctors/In-ctor
    (let [res (let [ts (set (flatten-intersections types))]
                (cond
                  ; empty intersection is bottom
                  (or (empty? ts)
                      (contains? ts bottom)) bottom

                  (= 1 (count ts)) (first ts)

                  ; normalise (I t1 t2 (U t3 t4))
                  ; to (U (I t1 t2) (I t1 t2 t3) (U t1 t2 t4))
                  :else (let [{unions true non-unions false} (group-by r/Union? ts)
                              ;_ (prn "unions" (map unparse-type unions))
                              ;_ (prn "non-unions" (map unparse-type non-unions))
                              ;intersect all the non-unions to get a possibly-nil type
                              intersect-non-unions 
                              (p :intersect-in-In (when (seq non-unions)
                                                    (reduce intersect non-unions)))
                              ;if we have an intersection above, use it to update each
                              ;member of the unions we're intersecting
                              flat-unions (set (flatten-unions unions))
                              intersect-union-ts (cond 
                                                   intersect-non-unions
                                                   (if (seq flat-unions)
                                                     (reduce (fn [acc union-m]
                                                               (conj acc (intersect intersect-non-unions union-m)))
                                                             #{} flat-unions)
                                                     #{intersect-non-unions})

                                                   :else flat-unions)
                              _ (assert (every? r/Type? intersect-union-ts)
                                        intersect-union-ts)
                              ;_ (prn "intersect-union-ts" (map unparse-type intersect-union-ts))
                              ]
                          (apply Un intersect-union-ts))))]
      ;(swap! In-cache assoc (set types) res)
      #_(prn 'IN res (class res))
      res))
  )

(declare TypeFn* instantiate-poly instantiate-typefn abstract-many instantiate-many)

;; JS Nominal

(t/ann ^:no-check JSNominal*
  (Fn [Symbol -> r/Type]
      [(Seqable Symbol) (Seqable r/Variance) (Seqable r/Type) Symbol (Seqable Bounds) -> r/Type]))
(defn JSNominal* 
  ([name] (JSNominal* nil nil nil name nil))
  ([names variances poly? name bnds]
   {:pre [(every? symbol? names)
          (every? r/variance? variances)
          (= (count variances) (count poly?))
          (every? r/Type? poly?)
          (every? r/Bounds? bnds)
          (symbol? name)]
    :post [(r/Type? %)]}
   (let [p (r/JSNominal-maker (seq variances) (seq poly?) name)]
     (if (seq variances)
       (TypeFn* names variances bnds p)
       p))))

(t/ann ^:no-check JSNominal-of (Fn [Symbol -> r/Type]
                                   [Symbol (U nil (Seqable r/Type)) -> r/Type]))
(defn JSNominal-of
  ([sym] (JSNominal-of sym nil))
  ([sym args]
   {:pre [(symbol? sym)
          (every? r/Type? args)]
    :post [(r/Type? %)]}
   (let [p ((impl/v 'clojure.core.typed.jsnominal-env/get-jsnominal) sym)]
     (assert ((some-fn r/TypeFn? r/JSNominal? nil?) p))
     ; parameterised nominals must be previously annotated
     (assert (or (r/TypeFn? p) (empty? args))
             (str "Cannot instantiate non-polymorphic JS nominal " sym))
     (cond 
       (r/TypeFn? p) (instantiate-typefn p args)
       (r/JSNominal? p) p
       ; allow unannotated nominals if unparameterised
       :else (JSNominal* sym)))))

;Datatype

(t/ann ^:no-check DataType*
  [(Seqable Symbol) (Seqable r/Variance) (Seqable r/Type) Symbol (Seqable Bounds) -> r/Type])
(defn DataType* [names variances poly? name bnds fields record?]
  {:pre [(every? symbol? names)
         (every? r/variance? variances)
         (= (count variances) (count poly?))
         (every? r/Type? poly?)
         (every? r/Bounds? bnds)
         (symbol? name)]
   :post [(r/Type? %)]}
  (let [p (r/DataType-maker name (seq variances) (seq poly?) fields record?)]
    (if (seq variances)
      (TypeFn* names variances bnds p)
      p)))

(t/ann ^:no-check DataType-of (Fn [Symbol -> r/Type]
                                  [Symbol (U nil (Seqable r/Type)) -> r/Type]))
(defn DataType-of
  ([sym] (DataType-of sym nil))
  ([sym args]
   {:pre [(symbol? sym)
          (every? r/Type? args)]
    :post [(r/Type? %)]}
   (let [p (dtenv/get-datatype sym)]
     (assert ((some-fn r/TypeFn? r/DataType? nil?) p))
     ; parameterised datatypes must be previously annotated
     (assert (or (r/TypeFn? p) (empty? args))
             (str "Cannot instantiate non-polymorphic datatype " sym))
     (cond 
       (r/TypeFn? p) (instantiate-typefn p args)
       (r/DataType? p) p
       ; allow unannotated datatypes if unparameterised
       :else (DataType* nil nil nil sym nil {} false)))))


;; Protocol

(t/ann ^:no-check Protocol*
  [(Seqable Symbol) (Seqable r/Variance) (Seqable r/Type) Symbol Symbol (IPersistentMap Symbol r/Type) (Seqable Bounds) 
   & :optional {:declared? Boolean} -> r/Type])
(defn Protocol* [names variances poly? the-var on-class methods bnds
                 & {:keys [declared?] :or {declared? false}}]
  {:pre [(every? symbol? names)
         (every? r/variance? variances)
         (= (count variances) (count poly?))
         (every? r/Type? poly?)
         (every? r/Bounds? bnds)
         (symbol? the-var)
         (symbol? on-class)]
   :post [(r/Type? %)]}
  (let [methods (into {} (for [[k v] methods]
                           [k (abstract-many names v)]))
        p (r/Protocol-maker the-var (seq variances) (seq poly?) on-class methods declared?)]
    (if (seq variances)
      (TypeFn* names variances bnds p)
      p)))

(t/ann ^:no-check Protocol-var->on-class [Symbol -> Symbol])
(defn Protocol-var->on-class 
  "Given the var symbol of a protocol, returns the corresponding
  class the protocol is based on as a munged symbol."
  [sym]
  {:pre [(symbol? sym)]
   :post [(symbol? %)]}
  (symbol (str (munge (namespace sym)) \. (name sym))))

(t/ann ^:no-check Protocol-interface->on-var [Symbol -> Symbol])
(defn Protocol-interface->on-var
  "Given the interface symbol of a protocol, returns the corresponding
  var the protocol is based on as a symbol. Assumes the interface is possible
  to demunge. Only useful for Clojure implementation."
  [sym]
  {:pre [(symbol? sym)]
   :post [(symbol? %)]}
  (let [segs (vec (partition-by #{\.} (str (repl/demunge (str sym)))))
        segs (assoc-in segs [(- (count segs) 2)] '(\/))
        var-sym (symbol (apply str (apply concat segs)))]
    var-sym))

(t/ann resolve-Protocol [Protocol -> Var])
(defn resolve-Protocol
  [{:keys [the-var]}]
  {:post [(var? %)]}
  (let [v (resolve the-var)]
    (assert (var? v) (str "Cannot resolve protocol: " the-var))
    v))

(t/ann Protocol-normal-extenders [Protocol -> (t/Set (U nil Class))])
(defn Protocol-normal-extenders
  [p]
  (set (extenders @(resolve-Protocol p))))

(t/ann ^:no-check Protocol-of (Fn [Symbol -> r/Type]
                                  [Symbol (U nil (Seqable r/Type)) -> r/Type]))
(defn Protocol-of 
  ([sym] (Protocol-of sym nil))
  ([sym args]
   {:pre [(symbol? sym)
          (every? r/Type? args)]
    :post [(r/Type? %)]}
   (let [p (prenv/get-protocol sym)]
     (assert ((some-fn r/TypeFn? r/Protocol? nil?) p))
     ; parameterised protocols must be previously annotated
     (assert (or (r/TypeFn? p) (empty? args))
             (str "Cannot instantiate non-polymorphic Protocol " sym))
     (cond 
       (r/TypeFn? p) (instantiate-typefn p args)
       (r/Protocol? p) p
       ; allow unannotated protocols if unparameterised
       :else (r/Protocol-maker sym nil nil (Protocol-var->on-class sym) {} false)))))

;; RClass

(t/ann *current-RClass-super* (U nil Symbol))
(defonce ^:dynamic *current-RClass-super* nil)

;smart constructor
(t/ann ^:no-check RClass* 
  (Fn [(Seqable Symbol) (Seqable r/Variance) (Seqable r/Type) Symbol (IPersistentMap Symbol r/Type) -> r/Type]
      [(Seqable Symbol) (Seqable r/Variance) (Seqable r/Type) Symbol 
       (IPersistentMap Symbol r/Type) (IPersistentSet r/Type) -> r/Type]))
(defn RClass* 
  ([names variances poly? the-class replacements]
   (RClass* names variances poly? the-class replacements #{}))
  ([names variances poly? the-class replacements unchecked-ancestors]
   (RClass* names variances poly? the-class replacements unchecked-ancestors (repeat (count names) r/no-bounds)))
  ([names variances poly? the-class replacements unchecked-ancestors bnds]
  {:pre [(every? symbol? names)
         (every? r/variance? variances)
         (= (count variances) (count poly?))
         (every? r/Type? poly?)
         (every? r/Bounds? bnds)
         (symbol? the-class)]
   :post [((some-fn r/TypeFn? r/RClass?) %)]}
   (let [repl (into {} (for [[k v] replacements]
                         [k (abstract-many names v)]))
         uncked (set (for [u unchecked-ancestors]
                       (abstract-many names u)))]
     (if (seq variances)
       (TypeFn* names variances bnds (r/RClass-maker variances poly? the-class repl uncked))
       (r/RClass-maker nil nil the-class repl uncked)))))

(t/ann ^:no-check isa-DataType? [(U Symbol Class) -> Any])
(defn isa-DataType? [sym-or-cls]
  {:pre [((some-fn symbol? class?) sym-or-cls)]}
  (let [cls (if (class? sym-or-cls)
              sym-or-cls
              (u/symbol->Class sym-or-cls))]
    (and (isa? cls clojure.lang.IType)
         (not= cls clojure.lang.IType))))

(t/ann ^:no-check isa-Record? [(U Symbol Class) -> Any])
(defn isa-Record? [sym-or-cls]
  {:pre [((some-fn symbol? class?) sym-or-cls)]}
  (let [cls (if (class? sym-or-cls)
              sym-or-cls
              (u/symbol->Class sym-or-cls))]
    (and (isa? cls clojure.lang.IRecord)
         (not= cls clojure.lang.IRecord))))

(t/ann ^:no-check Record->HMap [DataType -> r/Type])
(defn Record->HMap [^DataType r]
  {:pre [(r/Record? r)]
   :post [(r/Type? %)]}
  (let [kf (zipmap (map (comp r/-val keyword) (keys (.fields r)))
                   (vals (.fields r)))]
    (-hmap kf)))

(t/ann RClass-of-cache (t/Atom1 (t/Map Any r/Type)))
(defonce ^:private RClass-of-cache (atom {}))

(t/ann reset-RClass-of-cache! [-> nil])
(defn reset-RClass-of-cache! []
  (reset! RClass-of-cache {})
  nil)

(t/ann ^:no-check RClass-of (Fn [(U Symbol Class) -> r/Type]
                               [(U Symbol Class) (U nil (Seqable r/Type)) -> r/Type]))
(defn RClass-of 
  ([sym-or-cls] (RClass-of sym-or-cls nil))
  ([sym-or-cls args]
   {:pre [((some-fn class? symbol?) sym-or-cls)
          (every? r/Type? args)]
    :post [((some-fn r/RClass? r/DataType?) %)]}
   (u/p :ctors/RClass-of
   (let [sym (if (class? sym-or-cls)
               (u/Class->symbol sym-or-cls)
               sym-or-cls)
         cache-key-hash [(keyword sym) (mapv r/type-id args)]
         cache-hit (@RClass-of-cache cache-key-hash)]
     (if cache-hit
       (u/p :ctors/RClass-of-cache-hit
            cache-hit)
       (u/p :ctors/RClass-of-cache-miss
         (let [rc ((some-fn dtenv/get-datatype rcls/get-rclass) 
                   sym)
               _ (assert ((some-fn r/TypeFn? r/RClass? r/DataType? nil?) rc))
               _ (assert (or (r/TypeFn? rc) (empty? args))
                         (str "Cannot instantiate non-polymorphic RClass " sym
                              (when *current-RClass-super*
                                (str " when checking supertypes of RClass " *current-RClass-super*))))
               res (cond 
                     (r/TypeFn? rc) (instantiate-typefn rc args)
                     ((some-fn r/DataType? r/RClass?) rc) rc
                     :else
                     (let [cls (u/symbol->Class sym)]
                       (if (isa-DataType? cls)
                         (do (println (str "WARNING: Assuming unannotated Clojure type " sym
                                           " is a datatype"))
                             (flush)
                             (when (isa-Record? cls)
                               (println (str "WARNING: " sym " is probably a record because it extends IRecord."
                                             " Annotate with ann-record above the first time it is parsed"))
                               (flush))
                           (r/DataType-maker sym nil nil (array-map) (isa-Record? cls)))
                         (r/RClass-maker nil nil sym {} #{}))))]
           (swap! RClass-of-cache assoc cache-key-hash res)
           res)))))))

(t/ann ^:no-check most-general-on-variance [(Seqable r/Variance) (Seqable Bounds) -> r/Type])
(defn most-general-on-variance [variances bnds]
  (doall
    (for [[variance bnd] (map vector variances bnds)]
      (if (= r/no-bounds bnd)
        (case variance
          (:invariant :constant :covariant) r/-any
          :contravariant r/-nothing)
        (assert nil (str "Don't know most general type for bounds other than Any/Nothing" 
                         (unparse-type (:upper-bound bnd))
                         (unparse-type (:lower-bound bnd))))))))

(declare TypeFn-bbnds*)

;FIXME rename to RClass-with-unknown-params
(t/ann ^:no-check RClass-of-with-unknown-params [(U Symbol Class) -> r/Type])
(defn RClass-of-with-unknown-params
  ([sym-or-cls]
   {:pre [((some-fn class? symbol?) sym-or-cls)]
    :post [((some-fn r/RClass? r/DataType?) %)]}
   (let [sym (if (class? sym-or-cls)
               (u/Class->symbol sym-or-cls)
               sym-or-cls)
         rc ((some-fn dtenv/get-datatype rcls/get-rclass) sym)
         args (when (r/TypeFn? rc)
                (most-general-on-variance (:variances rc)
                                          (TypeFn-bbnds* (repeatedly (count (:variances rc)) gensym) rc)))]
     (RClass-of sym args))))

(t/ann ^:no-check DataType-with-unknown-params [Symbol -> r/Type])
(defn DataType-with-unknown-params
  ([sym]
   {:pre [(symbol? sym)]
    :post [((some-fn r/DataType?) %)]}
   (let [t (dtenv/get-datatype sym)
         args (when (r/TypeFn? t)
                (most-general-on-variance (:variances t)
                                          (TypeFn-bbnds* (repeatedly (count (:variances t)) gensym) t)))]
     (DataType-of sym args))))

(t/ann ^:no-check JSNominal-with-unknown-params [Symbol -> r/Type])
(defn JSNominal-with-unknown-params
  ([sym]
   {:pre [(symbol? sym)]
    :post [((some-fn r/JSNominal?) %)]}
   (let [t ((impl/v 'clojure.core.typed.jsnominal-env/get-jsnominal) sym)
         args (when (r/TypeFn? t)
                (most-general-on-variance (:variances t)
                                          (TypeFn-bbnds* (repeatedly (count (:variances t)) gensym) t)))]
     (JSNominal-of sym args))))

(t/ann ^:no-check JSNominal-method* [JSNominal Symbol -> r/Type])
(defn JSNominal-method*
  [{:keys [name poly?] :as jsnom} msym]
  {:pre [(r/JSNominal? jsnom)
         (symbol? msym)]
   :post [(r/Type? %)]}
  (if-let [t ((impl/v 'clojure.core.typed.jsnominal-env/get-method) name poly? msym)]
    t
    (assert nil (str "JS nominal type " name " does not have method " msym))))

(t/ann ^:no-check JSNominal-field* [JSNominal Symbol -> r/Type])
(defn JSNominal-field*
  [{:keys [name poly?] :as jsnom} fsym]
  {:pre [(r/JSNominal? jsnom)
         (symbol? fsym)]
   :post [(r/Type? %)]}
  (if-let [t ((impl/v 'clojure.core.typed.jsnominal-env/get-field) name poly? fsym)]
    t
    (assert nil (str "JS nominal type " name " does not have field " fsym))))

(t/ann ^:no-check JSNominal-ctor* [JSNominal -> r/Type])
(defn JSNominal-ctor*
  [{:keys [name poly?] :as jsnom}]
  {:pre [(r/JSNominal? jsnom)]
   :post [(r/Type? %)]}
  (if-let [t ((impl/v 'clojure.core.typed.jsnominal-env/get-ctor) name poly?)]
    t
    (assert nil (str "JS nominal type " name " does not have a constructor."))))

(t/ann ^:no-check Protocol-with-unknown-params [Symbol -> r/Type])
(defn Protocol-with-unknown-params
  ([sym]
   {:pre [(symbol? sym)]
    :post [((some-fn r/Protocol?) %)]}
   (let [t (prenv/get-protocol sym)
         args (when (r/TypeFn? t)
                (most-general-on-variance (:variances t)
                                          (TypeFn-bbnds* (repeatedly (count (:variances t)) gensym) t)))]
     (Protocol-of sym args))))

(t/tc-ignore
(defn- infer-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.cs-gen) 'infer)]
    (assert (var? v) "infer unbound")
    v))
  )

(t/tc-ignore
(defn- subst-all-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.subst) 'subst-all)]
    (assert (var? v) "subst-all unbound")
    v))
  )

(declare make-simple-substitution)

(t/ann ^:no-check inst-and-subst [(U r/Type Scope) (U nil (Seqable r/Type)) -> r/Type])
(defn inst-and-subst 
  "Instantiate target type with ts number of
  free names. Target must be wrapped in ts number
  of Scopes. Substitutes the temporary names with
  types ts."
  [target ts]
  {:pre [((some-fn r/Type? r/Scope?) target)
         (every? r/Type? ts)]
   :post [(r/Type? %)]}
  (u/p :ctors/inst-and-subst
  (let [subst-all @(subst-all-var)
        names (repeatedly (count ts) gensym)
        fs (map r/make-F names)
        t (instantiate-many names target)
        _ (assert (r/Type? t))
        subst (make-simple-substitution names ts)]
    (subst-all subst t))))

(t/ann ^:no-check RClass-replacements* [RClass -> (IPersistentMap Symbol r/Type)])
(defn RClass-replacements*
  "Return the replacements map for the RClass"
  [^RClass rcls]
  {:pre [(r/RClass? rcls)]
   :post [((u/hash-c? symbol? r/Type?) %)]}
  (let [subst-all @(subst-all-var)
        poly (.poly? rcls)]
    (into {} (for [[k v] (.replacements rcls)]
               [k (inst-and-subst v poly)]))))

(t/ann ^:no-check RClass-unchecked-ancestors* [RClass -> (IPersistentSet r/Type)])
(defn RClass-unchecked-ancestors*
  [^RClass rcls]
  {:pre [(r/RClass? rcls)]
   :post [((u/set-c? r/Type?) %)]}
  (u/p :ctors/RClass-unchecked-ancestors*
  (let [subst-all @(subst-all-var)
        poly (.poly? rcls)
        names (repeatedly (count poly) gensym)
        fs (map r/make-F names)]
    (set (for [u (.unchecked-ancestors rcls)]
           (let [t (instantiate-many names u)
                 subst (make-simple-substitution names poly)]
             (subst-all subst t)))))))

(t/ann supers-cache (t/Atom1 (t/Map Number (t/Map Symbol r/Type))))
(defonce ^:private supers-cache (atom {}))

(t/ann reset-supers-cache! [-> nil])
(defn reset-supers-cache! []
  (reset! supers-cache {})
  nil)

;TODO won't type check because records+destructuring
(t/ann ^:no-check RClass-supers* [RClass -> (t/Set r/Type)])
(defn RClass-supers* 
  "Return a set of ancestors to the RClass"
  [{:keys [the-class] :as rcls}]
  {:pre [(r/RClass? rcls)]
   :post [((u/set-c? r/Type?) %)
          ]}
  (u/p :ctors/RClass-supers*
  ;(prn "RClass-supers*" the-class (@(unparse-type-var) rcls))
  (let [cache-key (r/type-id rcls)
        cache-hit (@supers-cache cache-key)]
    (if cache-hit
      (u/p :ctors/RClass-supers-cache-hit
       cache-hit)
      (u/p :ctors/RClass-supers-cache-miss
        (let [unchecked-ancestors (RClass-unchecked-ancestors* rcls)
              ;_ (prn "unchecked-ancestors" (map @(unparse-type-var) unchecked-ancestors))
              replacements (RClass-replacements* rcls)
              ;_ (prn "replacements" (map @(unparse-type-var) (vals replacements)))
              ;set of symbols of Classes we haven't explicitly replaced
              not-replaced (set/difference (set (map u/Class->symbol (-> the-class u/symbol->Class supers)))
                                           (set (keys replacements)))
              ;(prn "not-replaced" not-replaced)
              res (set/union (binding [*current-RClass-super* the-class]
                       (set (doall 
                              (for [csym not-replaced]
                                (RClass-of-with-unknown-params csym)))))
                     (set (vals replacements))
                     #{(RClass-of Object)}
                     unchecked-ancestors)]
          (assert (<= (count (filter (some-fn r/FnIntersection? r/Poly? r/PolyDots?) res))
                      1)
                  (str "Found more than one function supertype for RClass " (unparse-type rcls) ": \n"
                       (mapv unparse-type (filter (some-fn r/FnIntersection? r/Poly? r/PolyDots?) res))
                       "\nReplacements:" (into {} (map (fn [[k v]] [k (unparse-type v)]) replacements))
                       "\nNot replaced:" not-replaced
                       (try (throw (Exception. ""))
                            (catch Exception e
                              (with-out-str (clojure.repl/pst e 40))))))
          (swap! supers-cache assoc cache-key res)
          res))))))

(t/ann ^:no-check DataType-fields* [DataType -> (IPersistentMap Symbol r/Type)])
(defn DataType-fields* [^DataType dt]
  {:pre [(r/DataType? dt)]
   :post [((u/array-map-c? symbol? r/Type?) %)]}
  (:fields dt))

;; TypeFn

;smart constructor
(t/ann ^:no-check TypeFn* [(Seqable Symbol) (Seqable r/Variance) (Seqable Bounds) r/Type -> r/Type])
(defn TypeFn* [names variances bbnds body]
  {:pre [(every? symbol names)
         (every? r/variance? variances)
         (every? r/Bounds? bbnds)
         (apply = (map count [names variances bbnds]))
         ((some-fn r/TypeFn? r/Type?) body)]
   :post [(r/Type? %)]}
  (if (empty? names)
    body
    (r/TypeFn-maker (count names) 
                    variances
                    (vec
                      (for [bnd bbnds]
                        (r/visit-bounds bnd #(abstract-many names %))))
                    (abstract-many names body))))

;smart destructor
(t/ann ^:no-check TypeFn-body* [(Seqable Symbol) TypeFn -> r/Type])
(defn TypeFn-body* [names ^TypeFn typefn]
  {:pre [(every? symbol? names)
         (r/TypeFn? typefn)]}
  (u/p :ctors/TypeFn-body*
  (assert (= (.nbound typefn) (count names)) "Wrong number of names")
  (instantiate-many names (.scope typefn))))

(t/ann ^:no-check TypeFn-bbnds* [(Seqable Symbol) TypeFn -> (Seqable Bounds)])
(defn TypeFn-bbnds* [names ^TypeFn typefn]
  {:pre [(every? symbol? names)
         (r/TypeFn? typefn)]
   :post [(every? r/Bounds? %)]}
  (assert (= (.nbound typefn) (count names)) "Wrong number of names")
  (mapv (fn [b]
          (r/visit-bounds b #(instantiate-many names %)))
        (.bbnds typefn)))

;; Poly

;smart constructor
(t/ann ^:no-check Poly* [(Seqable Symbol) (Seqable Bounds) r/Type (Seqable Symbol) -> r/Type])
(defn Poly* [names bbnds body free-names]
  {:pre [(every? symbol names)
         (every? r/Bounds? bbnds)
         (r/Type? body)
         (every? symbol? free-names)
         (apply = (map count [names bbnds free-names]))]}
  (if (empty? names)
    body
    (r/Poly-maker (count names) 
                  (vec
                    (for [bnd bbnds]
                      (r/visit-bounds bnd #(abstract-many names %))))
                  (abstract-many names body)
                  free-names)))

(t/ann ^:no-check Poly-free-names* [Poly -> (Seqable Symbol)])
(defn Poly-free-names* [^Poly poly]
  {:pre [(r/Poly? poly)]
   :post [((every-pred seq (u/every-c? symbol?)) %)]}
  (.actual-frees poly))

;smart destructor
(t/ann ^:no-check Poly-body* [(Seqable Symbol) Poly -> r/Type])
(defn Poly-body* [names ^Poly poly]
  {:pre [(every? symbol? names)
         (r/Poly? poly)]}
  (u/p :ctors/Poly-body*
  (assert (= (.nbound poly) (count names)) "Wrong number of names")
  (instantiate-many names (.scope poly))))

(t/ann ^:no-check Poly-bbnds* [(Seqable Symbol) Poly -> (Seqable Bounds)])
(defn Poly-bbnds* [names ^Poly poly]
  {:pre [(every? symbol? names)
         (r/Poly? poly)]}
  (assert (= (.nbound poly) (count names)) "Wrong number of names")
  (mapv (fn [b]
          (r/visit-bounds b #(instantiate-many names %)))
        (.bbnds poly)))

;; PolyDots

;smart constructor
(t/ann ^:no-check PolyDots* [(Seqable Symbol) (Seqable Bounds) r/Type (Seqable Symbol) -> r/Type])
(defn PolyDots* [names bbnds body free-names]
  {:pre [(every? symbol names)
         (every? r/Bounds? bbnds)
         (r/Type? body)]}
  (assert (= (count names) (count bbnds)) "Wrong number of names")
  (if (empty? names)
    body
    (r/PolyDots-maker (count names) 
                      (mapv (fn [bnd] 
                              (r/visit-bounds bnd #(abstract-many names %)))
                            bbnds)
                      (abstract-many names body)
                      free-names)))

;smart destructor
(t/ann ^:no-check PolyDots-body* [(Seqable Symbol) PolyDots -> r/Type])
(defn PolyDots-body* [names ^PolyDots poly]
  {:pre [(every? symbol? names)
         (r/PolyDots? poly)]}
  (assert (= (.nbound poly) (count names)) "Wrong number of names")
  (instantiate-many names (.scope poly)))

(t/ann ^:no-check PolyDots-bbnds* [(Seqable Symbol) PolyDots -> (Seqable Bounds)])
(defn PolyDots-bbnds* [names ^PolyDots poly]
  {:pre [(every? symbol? names)
         (r/PolyDots? poly)]}
  (assert (= (.nbound poly) (count names)) "Wrong number of names")
  (mapv (fn [b]
          (r/visit-bounds b #(instantiate-many names %)))
        (.bbnds poly)))

(t/ann ^:no-check PolyDots-free-names* [Poly -> (Seqable Symbol)])
(defn PolyDots-free-names* [^PolyDots poly]
  {:pre [(r/PolyDots? poly)]
   :post [((every-pred seq (u/every-c? symbol?)) %)]}
  (.actual-frees poly))


;; Instantiate ops

(t/ann ^:no-check make-simple-substitution [(Seqable Symbol) (Seqable r/Type) -> cr/SubstMap])
(defn make-simple-substitution [vs ts]
  {:pre [(every? symbol? vs)
         (every? r/Type? ts)
         (= (count vs)
            (count ts))]}
  (into {} (for [[v t] (map vector vs ts)]
             [v (crep/->t-subst t r/no-bounds)])))

(t/ann ^:no-check instantiate-typefn [TypeFn (Seqable r/Type) -> r/Type])
(defn instantiate-typefn [^TypeFn t types]
  (let [subst-all @(subst-all-var)
        unparse-type @(unparse-type-var)]
    (assert (r/TypeFn? t) (str "instantiate-typefn requires a TypeFn: " (unparse-type t)))
    (do (assert (= (.nbound t) (count types)) (u/error-msg "Wrong number of arguments passed to type function: "
                                                         (unparse-type t) (mapv unparse-type types)))
        (let [nms (repeatedly (.nbound t) gensym)
              body (TypeFn-body* nms t)]
          (subst-all (make-simple-substitution nms types) body)))))

(t/ann ^:no-check instantiate-poly [Poly (Seqable r/Type) -> r/Type])
(defn instantiate-poly [t types]
  (let [subst-all @(subst-all-var)
        unparse-type @(unparse-type-var)]
    (cond
      (r/Poly? t) (do (assert (= (:nbound t) (count types)) (u/error-msg "Wrong number of arguments (" (count types) 
                                                                       ") passed to polymorphic type: "
                                                                       (unparse-type t)
                                                                       (when (bound? #'*current-RClass-super*)
                                                                         (str " when checking ancestors of " *current-RClass-super*))))
                      (let [nms (repeatedly (:nbound t) gensym)
                            body (Poly-body* nms t)]
                        (subst-all (make-simple-substitution nms types) body)))
      ;PolyDots NYI
      :else (throw (Exception. "instantiate-poly: requires Poly, and PolyDots NYI")))))

;; Resolve

(declare resolve-tapp* -resolve resolve-app*)

(t/ann ^:no-check resolve-TApp [TApp -> r/Type])
(defn resolve-TApp [^TApp app]
  {:pre [(r/TApp? app)]}
  (resolve-tapp* (.rator app) (.rands app)))

(t/ann ^:no-check resolve-tapp* [r/Type (Seqable r/Type) -> r/Type])
(defn resolve-tapp* [rator rands]
  (let [unparse-type @(unparse-type-var)
        ^TypeFn rator (-resolve rator)
        _ (assert (r/TypeFn? rator) (unparse-type rator))]
    (when-not (= (count rands) (.nbound rator))
      (u/int-error (str "Wrong number of arguments (" (count rands) ", expected " (:nbound rator) ") provided to type function "
                        (unparse-type rator) (mapv unparse-type rands))))
    (instantiate-typefn rator rands)))

(t/ann ^:no-check resolve-App [App -> r/Type])
(defn resolve-App [^App app]
  {:pre [(r/App? app)]}
  (resolve-app* (.rator app) (.rands app)))

(t/ann ^:no-check resolve-app* [r/Type (Seqable r/Type) -> r/Type])
(defn resolve-app* [rator rands]
  (let [unparse-type @(unparse-type-var)
        rator (-resolve rator)]
    (cond
      (r/Poly? rator) (do (assert (= (count rands) (.nbound ^Poly rator))
                                  (u/error-msg "Wrong number of arguments provided to polymorphic type"
                                             (unparse-type rator)))
                          (instantiate-poly rator rands))
      ;PolyDots NYI
      :else (throw (Exception. (str (when vs/*current-env*
                                      (str (:line vs/*current-env*) ": "))
                                    "Cannot apply non-polymorphic type " (unparse-type rator)))))))

(declare resolve-Name unfold fully-resolve-type)

(t/ann -resolve [r/Type -> r/Type])
(defn -resolve [ty]
  {:pre [(r/AnyType? ty)]
   :post [(r/AnyType? %)]}
  (u/p :type-ctors/-resolve
  (cond 
    (r/Name? ty) (resolve-Name ty)
    (r/Mu? ty) (unfold ty)
    (r/App? ty) (resolve-App ty)
    (r/TApp? ty) (resolve-TApp ty)
    :else ty)))

(t/ann requires-resolving? [r/Type -> Any])
(defn requires-resolving? [ty]
  {:pre [(r/AnyType? ty)]}
  (u/p :ctors/requires-resolving?
  (or (r/Name? ty)
      (r/App? ty)
      (and (r/TApp? ty)
           (not (r/F? (fully-resolve-type (.rator ^TApp ty)))))
      (r/Mu? ty))))

(t/ann resolve-Name [Name -> r/Type])
(defn resolve-Name [nme]
  {:pre [(r/Name? nme)]}
  (let [resolve-name* (impl/v 'clojure.core.typed.name-env/resolve-name*)]
    (resolve-name* (:id nme))))

(t/ann fully-resolve-type 
       (Fn [r/Type -> r/Type]
           [r/Type (IPersistentSet r/Type) -> r/Type]))
(defn fully-resolve-type 
  ([t seen]
   (let [_ (assert (not (seen t)) "Infinite non-Rec type detected")
         seen (conj seen t)]
     (if (requires-resolving? t)
       (recur (-resolve t) seen)
       t)))
  ([t] (u/p :ctors/fully-resolve-type (fully-resolve-type t #{}))))

;; Mu

(declare abstract instantiate)

;smart constructor
(t/ann Mu* [Symbol r/Type -> r/Type])
(defn Mu* [name body]
  (r/Mu-maker (abstract name body)))

;smart destructor
(t/ann Mu-body* [Symbol Mu -> r/Type])
(defn Mu-body* [name t]
  {:pre [(r/Mu? t)
         (symbol? name)]}
  (instantiate name (p/mu-scope t)))

(t/tc-ignore
(defn- substitute-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.subst) 'substitute)]
    (assert (var? v) "substitute unbound")
    v))
  )

(t/ann ^:no-check unfold [Mu -> r/Type])
(defn unfold [t]
  {:pre [(r/Mu? t)]
   :post [(r/Type? %)]}
  (let [substitute @(substitute-var)
        sym (gensym)
        body (Mu-body* sym t)]
    (substitute t sym body)))

;; Utils

(t/ann Value->Class [Value -> Class])
(defn ^Class Value->Class [^Value tval]
  {:post [(class? %)]}
  (class (.val tval)))

(t/ann keyword-value? [Any -> Any])
(defn keyword-value? [^Value val]
  (boolean
    (when (r/Value? val)
      (keyword? (.val val)))))

(t/ann number-value? [Any -> Any])
(defn number-value? [^Value val]
  (boolean
    (when (r/Value? val)
      (number? (.val val)))))

;; Overlap

;; FIXME much better algorithms around I'm sure
(t/ann ^:no-check countrange-overlap? [CountRange CountRange -> Any])
(defn countrange-overlap? 
  [{lowerl :lower upperl :upper :as l}
   {lowerr :lower upperr :upper :as r}]
  {:pre [(r/CountRange? l)
         (r/CountRange? r)]}
  (cond 
    (and upperl upperr)
        (or 
          ;; -----
          ;;   -------
          ;; and
          ;;   ---
          ;;   -------
          (<= lowerl lowerr upperl upperr)

          ;;    --
          ;;   -------
          (<= lowerr lowerl upperl upperr)

          ;;     ------
          ;; -------
          ;; and
          ;;     ---
          ;; -------
          (<= lowerr lowerl upperr upperl)

          ;; otherwise no overlap
          false)

    upperl ;; and (not upperr)
      (or 
        ;; ----
        ;;  ----->>
        ;; and
        ;;  ---
        ;;  ----->>
        (<= lowerl lowerr upperl)
        ;;   ---
        ;;  ----->>
        (<= lowerr lowerl)
        ;; otherwise no overlap
        false)
    upperr
      (or
        ;; ------>>
        ;;  ----
        ;; and
        ;;  ----->>
        ;;  ---
        (<= lowerl lowerr)
        
        ;;   --->>
        ;; ----
        (<= lowerr lowerl upperr)

        ;; else no overlap
        false)
    :else ;; (and (not upperl) (not upperr))
    ;; ---->>
    ;;   -->>
    ;; and
    ;;   -->>
    ;; ---->>
    true))

;true if types t1 and t2 overlap (NYI)
(t/ann ^:no-check overlap [r/Type r/Type -> Any])
(defn overlap [t1 t2]
  (let [subtype? @(subtype?-var)
        t1 (fully-resolve-type t1)
        t2 (fully-resolve-type t2)
        eq (= t1 t2)
        hmap-and-seq? (fn [h s] (and (r/HeterogeneousMap? h)
                                     (r/RClass? s)
                                     (= (u/Class->symbol clojure.lang.ISeq) (:the-class s))))
        hvec-and-seq? (fn [h s] (and (r/HeterogeneousVector? h)
                                     (r/RClass? s)
                                     (= (u/Class->symbol clojure.lang.ISeq) (:the-class s))))
        record-and-iseq? (fn [r s]
                           (and (r/Record? r)
                                (subtype? s (RClass-of clojure.lang.ISeq [r/-any]))))]
    (cond 
      eq eq

      (and (r/Value? t1)
           (r/Value? t2))
      eq

      (r/Union? t1)
      (boolean 
        (some #(overlap % t2) (.types ^Union t1)))

      (r/Union? t2)
      (boolean 
        (some #(overlap t1 %) (.types ^Union t2)))

      (r/Intersection? t1)
      (every? #(overlap % t2) (.types ^Intersection t1))

      (r/Intersection? t2)
      (every? #(overlap t1 %) (.types ^Intersection t2))

      (and (r/NotType? t1)
           (r/NotType? t2))
      ;FIXME what if both are Not's?
      true

      ; eg. (overlap (Not Number) Integer) => false
      ;     (overlap (Not Integer) Number) => true
      ;     (overlap (Not y) x) => true
      (r/NotType? t1)
      (let [neg-type (fully-resolve-type (:type t1))]
        (or (some (some-fn r/B? r/F?) [neg-type t2])
            (not (overlap neg-type t2))))

      (r/NotType? t2)
      ;switch arguments to catch above case
      (overlap t2 t1)

      ;if both are Classes, and at least one isn't an interface, then they must be subtypes to have overlap
;      (and (r/RClass? t1)
;           (r/RClass? t2)
;           (let [{t1-flags :flags} (reflect/type-reflect (r/RClass->Class t1))
;                 {t2-flags :flags} (reflect/type-reflect (r/RClass->Class t2))]
;             (some (complement :interface) [t1-flags t2-flags])))
;      (or (subtype? t1 t2)
;          (subtype? t2 t1))
      (and (r/RClass? t1)
           (r/RClass? t2))
      (let [{t1-flags :flags} (reflect/type-reflect (r/RClass->Class t1))
            {t2-flags :flags} (reflect/type-reflect (r/RClass->Class t2))]
        ; there is only an overlap if a class could have both classes as parents
        (or (subtype? t1 t2)
            (subtype? t2 t1)
            ; from here they are disjoint

            (cond
              ; no potential ancestors
              (some :final [t1-flags t2-flags]) false
              ; if we have two things that are not interfaces, ie. abstract, normal
              ; classes, there is no possibility of overlap
              (every? (complement :interface) [t1-flags t2-flags]) false
              :else true)))

      (some r/Extends? [t1 t2])
      (let [[^Extends the-extends other-type] (if (r/Extends? t1)
                                                [t1 t2]
                                                [t2 t1])]
        ; returns true if at least one +ve type overlaps, and if
        ; no negative types overlap, else false
        (boolean
          (and (some (fn [pos] (overlap pos other-type)) (.extends the-extends))
               (not-any? (fn [neg] (overlap neg other-type)) (.without the-extends)))))

      (or (r/Value? t1)
          (r/Value? t2)) 
      (or (subtype? t1 t2)
          (subtype? t2 t1))

      (and (r/CountRange? t1)
           (r/CountRange? t2)) 
      (countrange-overlap? t1 t2)

      (and (r/HeterogeneousMap? t1)
           (r/HeterogeneousMap? t2)) 
      (and (= (set (-> t1 :types keys))
              (set (-> t2 :types keys)))
           (every? true?
                   (for [[k1 v1] (:types t1)]
                     (let [v2 ((:types t2) k1)]
                       (overlap v1 v2)))))

      ;for map destructuring mexpansion
      (or (hmap-and-seq? t1 t2)
          (hmap-and-seq? t2 t1))
      false

      ;for vector destructuring mexpansion
      (or (hvec-and-seq? t1 t2)
          (hvec-and-seq? t2 t1))
      false

      ;for map destructuring of records. A record is never an ISeq
      (or (record-and-iseq? t1 t2)
          (record-and-iseq? t2 t1))
      false

      :else true))) ;FIXME conservative result

; restrict t1 to be a subtype of t2
(t/ann ^:no-check restrict [r/Type r/Type -> r/Type])
(defn restrict [t1 t2]
  (let [subtype? @(subtype?-var)
        subst-all @(subst-all-var)
        infer @(infer-var)]
    (cond
      (subtype? t1 t2) t1 ;; already a subtype

      (not (overlap t1 t2)) (Un) ;there's no overlap, so the restriction is empty

      (r/Union? t1) (apply Un (map (fn [e] (restrict e t2)) (:types t1)))
      (r/Union? t2) (apply Un (map (fn [e] (restrict t1 e)) (:types t2)))

      (r/Poly? t2)
      (let [names (repeatedly (:nbound t2) gensym)
            t (Poly-body* names t2)
            bbnds (Poly-bbnds* names t2)
            subst (u/handle-cs-gen-failure
                    (infer (zipmap names bbnds) {} (list t1) (list t) t1))]
        (and subst (restrict t1 (subst-all subst t1))))

      ;TODO other cases
      :else (In t2 t1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable rep

(t/ann ^:no-check add-scopes [t/AnyInteger r/Type -> (U r/Type Scope)])
(defn add-scopes 
  "Wrap type in n Scopes"
  [n t]
  {:pre [(u/nat? n)
         (r/Type? t)]
   :post [((some-fn r/Scope? r/Type?) %)]}
  (last 
    (take (inc n) (iterate r/Scope-maker t))))

(t/ann ^:no-check remove-scopes [t/AnyInteger (U Scope r/Type) -> (U Scope r/Type)])
(defn remove-scopes 
  "Unwrap n Scopes"
  [n sc]
  {:pre [(u/nat? n)
         (or (zero? n)
             (r/Scope? sc))]
   :post [(or (r/Scope? %) (r/Type? %))]}
  (last
    (take (inc n) (iterate (fn> [t :- Scope]
                             (assert (r/Scope? t) "Tried to remove too many Scopes")
                             (:body t))
                           sc))))

(t/ann ^:no-check rev-indexed (All [x] [(Seqable x) -> (Seqable '[t/AnyInteger x])]))
(defn- rev-indexed 
  "'(a b c) -> '([2 a] [1 b] [0 c])"
  [c]
  (map vector (iterate dec (dec (count c))) c))

(t/tc-ignore
(derive ::abstract-many f/fold-rhs-default)

(f/add-fold-case ::abstract-many
                 F
                 (fn [{name* :name :as t} {{:keys [name count outer sb]} :locals}]
                   (if (= name name*)
                     (r/B-maker (+ count outer))
                     t)))

(f/add-fold-case ::abstract-many
                 Function
                 (fn [{:keys [dom rng rest drest kws] :as ty} {{:keys [name count outer sb]} :locals}]
                   (r/Function-maker (doall (map sb dom))
                                 (sb rng)
                                 (when rest (sb rest))
                                 (when drest
                                   (-> drest
                                       (update-in [:pre-type] sb)
                                       (update-in [:name] #(if (= % name)
                                                             (+ count outer)
                                                             %))))
                                 (when kws
                                   (letfn [(abstract-kw-map [m]
                                             {:pre [(map? m)]}
                                             (into {} (for [[k v] m]
                                                        [k (sb v)])))]
                                     (-> kws
                                       (update-in [:mandatory] abstract-kw-map)
                                       (update-in [:optional] abstract-kw-map)))))))

(f/add-fold-case ::abstract-many
                 Mu
                 (fn [{:keys [scope]} {{:keys [name count type outer name-to]} :locals}]
                   (let [body (remove-scopes 1 scope)]
                     (r/Mu-maker (r/Scope-maker (name-to name count type (inc outer) body))))))

(f/add-fold-case ::abstract-many
                 PolyDots
                 (fn [{bbnds* :bbnds n :nbound body* :scope :as ty} {{:keys [name count type outer name-to]} :locals}]
                   (let [rs #(remove-scopes n %)
                         body (rs body*)
                         bbnds (mapv #(r/visit-bounds % rs) bbnds*)
                         as #(add-scopes n (name-to name count type (+ n outer) %))]
                     (r/PolyDots-maker n 
                                       (mapv #(r/visit-bounds % rs) bbnds)
                                       (as body)
                                       (PolyDots-free-names* ty)))))

(f/add-fold-case ::abstract-many
                 Poly
                 (fn [{bbnds* :bbnds n :nbound body* :scope :as poly} {{:keys [name count type outer name-to]} :locals}]
                   (let [rs #(remove-scopes n %)
                         body (rs body*)
                         bbnds (mapv #(r/visit-bounds % rs) bbnds*)
                         as #(add-scopes n (name-to name count type (+ n outer) %))]
                     (r/Poly-maker n 
                             (mapv #(r/visit-bounds % as) bbnds)
                             (as body)
                             (Poly-free-names* poly)))))

(f/add-fold-case ::abstract-many
                 TypeFn
                 (fn [{bbnds* :bbnds n :nbound body* :scope :keys [variances]} {{:keys [name count type outer name-to]} :locals}]
                   (let [rs #(remove-scopes n %)
                         body (rs body*)
                         bbnds (mapv #(r/visit-bounds % rs) bbnds*)
                         as #(add-scopes n (name-to name count type (+ n outer) %))]
                     (r/TypeFn-maker n 
                                 variances
                                 (mapv #(r/visit-bounds % as) bbnds)
                                 (as body)))))
  )

(t/ann ^:no-check abstract-many [(Seqable Symbol) r/Type -> (U r/Type Scope)])
(defn abstract-many 
  "Names Type -> Scope^n  where n is (count names)"
  [names ty]
  {:pre [(every? symbol? names)
         ((some-fn r/Type? r/TypeFn?) ty)]}
  (letfn [(name-to 
            ([name count type] (name-to name count type 0 type))
            ([name count type outer ty]
             (letfn [(sb [t] (name-to name count type outer t))]
               (f/fold-rhs ::abstract-many
                 {:type-rec sb
                  :filter-rec (f/sub-f sb ::abstract-many)
                  :object-rec (f/sub-o sb ::abstract-many)
                  :locals {:name name
                           :count count
                           :outer outer
                           :sb sb
                           :name-to name-to}}
                 ty))))]
    (if (empty? names)
      ty
      (let [n (count names)]
        (loop [ty ty
               names names
               count (dec n)]
          (if (zero? count)
            (add-scopes n (name-to (first names) 0 ty))
            (recur (name-to (first names) count ty)
                   (next names)
                   (dec count))))))))

(t/tc-ignore
(derive ::instantiate-many f/fold-rhs-default)

(f/add-fold-case ::instantiate-many
               B
               (fn [{:keys [idx] :as t} {{:keys [count outer image sb]} :locals}]
                 (if (= (+ count outer) idx)
                   (r/F-maker image)
                   t)))

(f/add-fold-case ::instantiate-many
               Function
               (fn [{:keys [dom rng rest drest kws]} {{:keys [count outer image sb]} :locals}]
                 (r/Function-maker (map sb dom)
                             (sb rng)
                             (when rest
                               (sb rest))
                             (when drest
                               (-> drest
                                 (update-in [:pre-type] sb)
                                 (update-in [:name] #(do
                                                       (assert (u/nat? %))
                                                       (if (= (+ count outer) %)
                                                         image
                                                         %)))))
                             (when kws
                               (letfn [(instantiate-kw-map [m]
                                         {:pre [(map? m)]}
                                         (into {} (for [[k v] m]
                                                    [k (sb v)])))]
                                 (-> kws
                                   (update-in [:mandatory] instantiate-kw-map)
                                   (update-in [:optional] instantiate-kw-map)))))))

(f/add-fold-case ::instantiate-many
               Mu
               (fn [{:keys [scope]} {{:keys [replace count outer image sb type]} :locals}]
                 (let [body (remove-scopes 1 scope)]
                   (r/Mu-maker (r/Scope-maker (replace image count type (inc outer) body))))))

(f/add-fold-case ::instantiate-many
               PolyDots
               (fn [{bbnds* :bbnds n :nbound body* :scope :as ty} {{:keys [replace count outer image sb type]} :locals}]
                 (let [rs #(remove-scopes n %)
                       body (rs body*)
                       bbnds (mapv #(r/visit-bounds % rs) bbnds*)
                       as #(add-scopes n (replace image count type (+ n outer) %))]
                   (r/PolyDots-maker n 
                                     (mapv #(r/visit-bounds % as) bbnds)
                                     (as body)
                                     (PolyDots-free-names* ty)))))

(f/add-fold-case ::instantiate-many
               Poly
               (fn [{bbnds* :bbnds n :nbound body* :scope :as poly} {{:keys [replace count outer image sb type]} :locals}]
                 (let [rs #(remove-scopes n %)
                       body (rs body*)
                       bbnds (mapv #(r/visit-bounds % rs) bbnds*)
                       as #(add-scopes n (replace image count type (+ n outer) %))]
                   (r/Poly-maker n 
                           (mapv #(r/visit-bounds % as) bbnds)
                           (as body)
                           (Poly-free-names* poly)))))

(f/add-fold-case ::instantiate-many
               TypeFn
               (fn [{bbnds* :bbnds n :nbound body* :scope :keys [variances]} {{:keys [replace count outer image sb type]} :locals}]
                 (let [rs #(remove-scopes n %)
                       body (rs body*)
                       bbnds (mapv #(r/visit-bounds % rs) bbnds*)
                       as #(add-scopes n (replace image count type (+ n outer) %))]
                   (r/TypeFn-maker n 
                             variances
                             (mapv #(r/visit-bounds % as) bbnds)
                             (as body)))))
  )

(t/ann ^:no-check instantiate-many [(Seqable Symbol) Scope -> r/Type])
(defn instantiate-many 
  "instantiate-many : List[Symbols] Scope^n -> Type
  Instantiate de Bruijn indices in sc to frees named by
  images, preserving upper/lower bounds"
  [images sc]
  {:pre [(every? symbol? images)
         (or (r/Scope? sc)
             (empty? images))]
   :post [((some-fn r/Type? r/TypeFn?) %)]}
  (u/p :ctors/instantiate-many
  (letfn [(replace 
            ([image count type] (replace image count type 0 type))
            ([image count type outer ty]
             (letfn [(sb [t] (replace image count type outer t))]
               (let [sf (f/sub-f sb ::instantiate-many)]
                 (f/fold-rhs ::instantiate-many
                   {:type-rec sb 
                    :filter-rec sf 
                    :object-rec (f/sub-o sb ::instantiate-many)
                    :locals {:count count
                             :outer outer
                             :image image
                             :sb sb
                             :type type
                             :replace replace}}
                   ty)))))]
    (if (empty? images)
      sc
      (let [n (count images)]
        (loop [ty (remove-scopes n sc)
               images images
               count (dec n)]
          (if (zero? count)
            (replace (first images) 0 ty)
            (recur (replace (first images) count ty)
                   (next images)
                   (dec count)))))))))

(t/ann abstract [Symbol r/Type -> Scope])
(defn abstract 
  "Make free name bound"
  [name ty]
  {:pre [(symbol? name)
         (r/Type? ty)]
   :post [(r/Scope? %)]}
  (abstract-many [name] ty))

(t/ann instantiate [Symbol p/IScope -> r/Type])
(defn instantiate 
  "Instantiate bound name to free"
  [f sc]
  {:pre [(symbol? f)
         (p/IScope? sc)]}
  (instantiate-many [f] sc))

;TODO not sure why this fails to type check
;(All [x]
;  (Fn ['{kw x} -> x :object {:id 0, :path [Key]}]
;      [(U '{kw x} (HMap :without [(Value kw)]) nil) -> (U x nil) :object {:id 0, :path [Key]}]
;      [Any -> Any :object {:id 0, :path [Key]}]))
(t/ann ^:no-check keyword->Fn [Keyword -> r/Type])
(defn keyword->Fn [kw]
  {:pre [(keyword? kw)]
   :post [(r/Type? %)]}
  (Poly* ['x]
         [r/no-bounds]
         (r/make-FnIntersection
           (r/make-Function
             [(-hmap {(r/-val kw) (r/make-F 'x)})]
             (r/make-F 'x)
             nil nil
             :object (or/->Path [(path/->KeyPE kw)] 0))
           (r/make-Function
             [(Un (-hmap {(r/-val kw) (r/make-F 'x)})
                  (-hmap {} #{(r/-val kw)} true)
                  r/-nil)]
             (Un r/-nil (r/make-F 'x))
             nil nil
             :object (or/->Path [(path/->KeyPE kw)] 0))
           (r/make-Function
             [r/-any]
             r/-any
             nil nil
             :object (or/->Path [(path/->KeyPE kw)] 0)))
         ['x]))

(t/ann KeywordValue->Fn [Value -> r/Type])
(defn KeywordValue->Fn [{:keys [val] :as t}]
  {:pre [(keyword-value? t)
         ;redundant test for core.typed
         (keyword? val)]}
  (impl/assert-clojure)
  (keyword->Fn val))

;; Extends

(t/tc-ignore
(defn -extends [clss & {:keys [without]}]
  (r/Extends-maker clss without))
  )

;;; KwArgs

(t/ann KwArgs->Type [KwArgs -> r/Type])
(defn KwArgs->Type [^KwArgs kws]
  {:pre [(r/KwArgs? kws)]
   :post [(r/Type? %)]}
  (impl/assert-clojure)
  (r/KwArgsSeq-maker (.mandatory kws)
                 (.optional kws)))

(t/ann KwArgsSeq->HMap [KwArgsSeq -> r/Type])
(defn KwArgsSeq->HMap [^KwArgsSeq kws]
  {:pre [(r/KwArgsSeq? kws)]
   :post [(r/Type? %)]}
  (make-HMap (.mandatory kws) (.optional kws)))
