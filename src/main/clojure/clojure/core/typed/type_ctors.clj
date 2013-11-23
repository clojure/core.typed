(ns ^:skip-wiki clojure.core.typed.type-ctors
  (:refer-clojure :exclude [defrecord])
  (:require [clojure.core.typed.utils :as u :refer [p]]
            [clojure.core.typed.impl-protocols :as p]
            [clojure.core.typed.type-rep :as r :refer [ret-t]]
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
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.tvar-bnds :as bnds]
            [clojure.core.typed :as t :refer [fn>]]
            [clojure.math.combinatorics :as comb]
            [clojure.set :as set]
            [clojure.reflect :as reflect]
            [clojure.repl :as repl]
            [clojure.core.cache :as cache]
            )

  (:import (clojure.core.typed.type_rep HeterogeneousMap Poly TypeFn PolyDots TApp App Value
                                        Union Intersection F Function Mu B KwArgs KwArgsSeq RClass
                                        Bounds Name Scope CountRange Intersection DataType Extends
                                        JSNominal Protocol HeterogeneousVector)
           (clojure.lang Seqable IPersistentSet IPersistentMap IPersistentVector Symbol Keyword
                         Atom Var)))

(t/typed-deps clojure.core.typed.name-env)

(t/tc-ignore
(alter-meta! *ns* assoc :skip-wiki true)
  )

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

(t/ann fresh-symbol [Symbol -> Symbol])
(defn fresh-symbol [s]
  {:pre [(symbol? s)]
   :post [(symbol? %)]}
  (with-meta (gensym s) {:original-name s}))

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

(t/ann -partial-hmap (Fn [(Seqable r/Type) -> r/Type]
                         [(Seqable r/Type) (IPersistentSet r/Type) -> r/Type]))
(defn -partial-hmap 
  ([types] (-partial-hmap types #{}))
  ([types absent-keys] (-hmap types absent-keys true)))

(t/ann hmap-present-key? [HeterogeneousMap r/Type -> Boolean])
(defn hmap-present-key? 
  "Returns true if hmap always has a keyt entry."
  [hmap keyt]
  {:pre [(r/HeterogeneousMap? hmap)
         (r/Type? keyt)]}
  (contains? (:types hmap) keyt))

(t/ann hmap-absent-key? [HeterogeneousMap r/Type -> Boolean])
(defn hmap-absent-key?
  "Returns true if hmap never has a keyt entry."
  [hmap keyt]
  {:pre [(r/HeterogeneousMap? hmap)
         (r/Type? keyt)]}
  (boolean
    (when-not (hmap-present-key? hmap keyt)
      (or ; absent if in :absent-keys
          (contains? (:absent-keys hmap) keyt)
          ; absent if no other keys
          (not (:other-keys? hmap))))))

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

(defn- subtype? [& args]
  (apply (impl/v 'clojure.core.typed.subtype/subtype?) args))
  )

(t/def-alias TypeCache 
  (t/Map (t/Set r/Type) r/Type))

(t/ann ^:no-check initial-Un-cache TypeCache)
(def ^:private initial-Un-cache (cache/lu-cache-factory {} :threshold 256))

(t/ann ^:no-check Un-cache (t/Atom1 TypeCache))
(defonce Un-cache (atom initial-Un-cache))

(t/ann ^:no-check reset-Un-cache [-> nil])
(defn reset-Un-cache []
  (reset! Un-cache initial-Un-cache)
  nil)

(declare flatten-unions)

(t/ann ^:no-check Un [r/Type * -> r/Type])
(defn Un [& types]
  ;(prn "Un" (map @(unparse-type-var) types))
  (if-let [hit (p :Union-cache-lookup (get @Un-cache (p :Union-calc-hash (set (map r/type-id types)))))]
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
  (let [p (r/Protocol-maker the-var (seq variances) (seq poly?) on-class methods declared?)]
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

(t/ann resolve-Protocol [Protocol -> (Var Nothing Any)])
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

(declare TypeFn-bbnds* TypeFn-fresh-symbols*)

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
                (let [syms (TypeFn-fresh-symbols* rc)]
                  (most-general-on-variance (:variances rc)
                                            (TypeFn-bbnds* syms rc))))]
     (RClass-of sym args))))

(t/ann ^:no-check DataType-with-unknown-params [Symbol -> r/Type])
(defn DataType-with-unknown-params
  ([sym]
   {:pre [(symbol? sym)]
    :post [((some-fn r/DataType?) %)]}
   (let [t (dtenv/get-datatype sym)
         args (when (r/TypeFn? t)
                (let [syms (TypeFn-fresh-symbols* t)]
                  (most-general-on-variance (:variances t)
                                            (TypeFn-bbnds* syms t))))]
     (DataType-of sym args))))

(t/ann ^:no-check JSNominal-with-unknown-params [Symbol -> r/Type])
(defn JSNominal-with-unknown-params
  ([sym]
   {:pre [(symbol? sym)]
    :post [((some-fn r/JSNominal?) %)]}
   (let [t ((impl/v 'clojure.core.typed.jsnominal-env/get-jsnominal) sym)
         args (when (r/TypeFn? t)
                (let [syms (TypeFn-fresh-symbols* t)]
                  (most-general-on-variance (:variances t)
                                            (TypeFn-bbnds* syms t))))]
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
                (let [syms (TypeFn-fresh-symbols* t)]
                  (most-general-on-variance (:variances t)
                                            (TypeFn-bbnds* syms t))))]
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
        ; these names are eliminated immediately, they don't need to be
        ; created with fresh-symbol
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
; FIXME this is dumb, unchecked-ancestors field should be properly unwrapped
; as an RClass field
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
    (let [t (r/TypeFn-maker (count names) 
                            variances
                            (vec
                              (for [bnd bbnds]
                                (r/visit-bounds bnd #(abstract-many names %))))
                            (abstract-many names body))]
      (r/set-name-table! t names)
      t)))

;smart destructor
(t/ann ^:no-check TypeFn-body* [(Seqable Symbol) TypeFn -> r/Type])
(defn TypeFn-body* [names typefn]
  {:pre [(every? symbol? names)
         (r/TypeFn? typefn)]}
  (u/p :ctors/TypeFn-body*
  (assert (= (:nbound typefn) (count names)) "Wrong number of names")
  (let [body (instantiate-many names (:scope typefn))
        ; We don't check variances are consistent at parse-time. Instead
        ; we check at instantiation time. This avoids some implementation headaches,
        ; like dealing with partially defined types.
        bbnds (TypeFn-bbnds* names typefn)
        fv-variances (impl/v 'clojure.core.typed.frees/fv-variances)
        vs (free-ops/with-bounded-frees 
             (map vector (map r/make-F names) bbnds)
             (fv-variances body))
        _ (doseq [[nme variance] (map vector names (:variances typefn))]
            (when-let [actual-v (vs nme)]
              (when-not (= (vs nme) variance)
                (u/int-error (str "Type variable " nme " appears in " (name actual-v) " position "
                                  "when declared " (name variance))))))]
    body)))

(t/ann ^:no-check TypeFn-bbnds* [(Seqable Symbol) TypeFn -> (Seqable Bounds)])
(defn TypeFn-bbnds* [names ^TypeFn typefn]
  {:pre [(every? symbol? names)
         (r/TypeFn? typefn)]
   :post [(every? r/Bounds? %)]}
  (assert (= (.nbound typefn) (count names)) "Wrong number of names")
  (mapv (fn [b]
          (r/visit-bounds b #(instantiate-many names %)))
        (.bbnds typefn)))

(t/ann ^:no-check TypeFn-free-names* [TypeFn -> (Seqable Symbol)])
(defn ^:private TypeFn-free-names* [tfn]
  {:pre [(r/TypeFn? tfn)]
   :post [((some-fn nil? 
                    (every-pred seq (u/every-c? symbol?))) 
           %)]}
  (r/lookup-name-table tfn))

(t/ann ^:no-check TypeFn-fresh-symbols* [TypeFn -> (Seqable Symbol)])
(defn TypeFn-fresh-symbols* [tfn]
  {:pre [(r/TypeFn? tfn)]
   :post [((every-pred seq (u/every-c? symbol?)) %)]}
  (map fresh-symbol (or (TypeFn-free-names* tfn)
                        (repeatedly (:nbound tfn) gensym))))

;; Poly

;smart constructor
;;
;; Corresponds to closing a type in locally nameless representation
;; (turns free `names` into bound De Bruijn vars)
;; Also keeps track of the original name in a table to recover names
;; for debugging or to correlate with surface syntax
;;
;; Provide #:original-names if the names that you are closing off
;; are *different* from the names you want recorded in the table.
;;
(t/ann ^:no-check Poly* [(Seqable Symbol) (Seqable Bounds) r/Type (Seqable Symbol) 
                         & :optional {:original-names (Seqable Symbol)} -> r/Type])
(defn Poly* [names bbnds body & {:keys [original-names] :or {original-names names}}]
  {:pre [(every? symbol names)
         (every? r/Bounds? bbnds)
         (r/Type? body)
         (every? symbol? original-names)
         (apply = (map count [names bbnds original-names]))]}
  (if (empty? names)
    body
    (let [v (r/Poly-maker (count names)
                          (vec
                            (for [bnd bbnds]
                              (r/visit-bounds bnd #(abstract-many names %))))
                          (abstract-many names body))]
      (r/set-name-table! v original-names)
      v)))

(t/ann ^:no-check Poly-free-names* [Poly -> (U nil (Seqable Symbol))])
(defn ^:private Poly-free-names* [poly]
  {:pre [(r/Poly? poly)]
   :post [((some-fn nil? 
                    (every-pred seq (u/every-c? symbol?)))
           %)]}
  (r/lookup-name-table poly))

(t/ann ^:no-check Poly-fresh-symbols* [Poly -> (Seqable Symbol)])
(defn Poly-fresh-symbols* [poly]
  {:pre [(r/Poly? poly)]
   :post [((every-pred seq (u/every-c? symbol?)) %)]}
  (map fresh-symbol (or (Poly-free-names* poly)
                        (repeatedly (:nbound poly) gensym))))

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
(t/ann ^:no-check PolyDots* [(Seqable Symbol) (Seqable Bounds) r/Type 
                             & :optional {:original-names (Seqable Symbol)}-> r/Type])
(defn PolyDots* [names bbnds body & {:keys [original-names] :or {original-names names}}]
  {:pre [(every? symbol names)
         (every? r/Bounds? bbnds)
         (r/Type? body)]}
  (assert (= (count names) (count bbnds)) "Wrong number of names")
  (if (empty? names)
    body
    (let [v (r/PolyDots-maker (count names) 
                              (mapv (fn [bnd] 
                                      (r/visit-bounds bnd #(abstract-many names %)))
                                    bbnds)
                              (abstract-many names body))]
      (r/set-name-table! v original-names)
      v)))

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

(t/ann ^:no-check PolyDots-free-names* [Poly -> (U nil (Seqable Symbol))])
(defn ^:private PolyDots-free-names* [poly]
  {:pre [(r/PolyDots? poly)]
   :post [((some-fn nil? 
                    (every-pred seq (u/every-c? symbol?))) 
           %)]}
  (r/lookup-name-table poly))

(t/ann ^:no-check PolyDots-fresh-symbols* [PolyDots -> (Seqable Symbol)])
(defn PolyDots-fresh-symbols* [poly]
  {:pre [(r/PolyDots? poly)]
   :post [((every-pred seq (u/every-c? symbol?)) %)]}
  (map fresh-symbol (or (PolyDots-free-names* poly)
                        (repeatedly (:nbound poly) gensym))))

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
        (let [nms (TypeFn-fresh-symbols* t)
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
                      (let [nms (Poly-fresh-symbols* t)
                            body (Poly-body* nms t)]
                        (subst-all (make-simple-substitution nms types) body)))
      ;PolyDots NYI
      :else (throw (Exception. "instantiate-poly: requires Poly, and PolyDots NYI")))))

;; Resolve

(declare resolve-tapp* -resolve resolve-app*)

(t/ann ^:no-check resolve-TApp [TApp -> r/Type])
(defn resolve-TApp [^TApp app]
  {:pre [(r/TApp? app)]}
  (resolve-tapp* (.rator app) (.rands app) :tapp app))

(t/ann ^:no-check resolve-tapp* [r/Type (Seqable r/Type) -> r/Type])
(defn resolve-tapp* [rator rands & {:keys [tapp]}]
  {:pre [(r/TApp? tapp)]}
  (let [unparse-type @(unparse-type-var)
        ^TypeFn rator (-resolve rator)
        _ (assert (r/TypeFn? rator) (unparse-type rator))]
    (when-not (= (count rands) (.nbound rator))
      (binding [vs/*current-env* (-> tapp meta :env)] ;must override env, or clear it
        (u/int-error (str "Wrong number of arguments (" (count rands) ") passed to type function: "
                          (unparse-type tapp) 
                          (when-let [syn (-> tapp meta :syn)]
                            (str " in " (pr-str syn)))))))
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
  (let [resolve-name* (t/var> clojure.core.typed.name-env/resolve-name*)]
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
  (let [v (r/Mu-maker (abstract name body))]
    (r/set-name-table! v name)
    v))

;smart destructor
(t/ann Mu-body* [Symbol Mu -> r/Type])
(defn Mu-body* [name t]
  {:pre [(r/Mu? t)
         (symbol? name)]}
  (instantiate name (p/mu-scope t)))

(t/ann ^:no-check Mu-free-name* [Mu -> (U nil Symbol)])
(defn Mu-free-name* [t]
  {:pre [(r/Mu? t)]
   :post [((some-fn symbol? nil?) %)]}
  (r/lookup-name-table t))

(t/ann ^:no-check Mu-fresh-symbol* [Mu -> Symbol])
(defn Mu-fresh-symbol* [t]
  {:pre [(r/Mu? t)]
   :post [(symbol? %)]}
  (let [s (or (Mu-free-name* t)
              (gensym))]
    (fresh-symbol s)))

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
        sym (Mu-fresh-symbol* t)
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
  (let [t1 (fully-resolve-type t1)
        t2 (fully-resolve-type t2)
        subtype? @(subtype?-var)
        subst-all @(subst-all-var)
        infer @(infer-var)]
    (cond
      (subtype? t1 t2) t1 ;; already a subtype

      (not (overlap t1 t2)) (Un) ;there's no overlap, so the restriction is empty

      (r/Union? t1) (apply Un (map (fn [e] (restrict e t2)) (:types t1)))
      (r/Union? t2) (apply Un (map (fn [e] (restrict t1 e)) (:types t2)))

      (r/Poly? t2)
      (let [names (Poly-fresh-symbols* t2)
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
                                       (as body)))))

(f/add-fold-case ::abstract-many
                 Poly
                 (fn [{bbnds* :bbnds n :nbound body* :scope :as poly} {{:keys [name count type outer name-to]} :locals}]
                   (let [rs #(remove-scopes n %)
                         body (rs body*)
                         bbnds (mapv #(r/visit-bounds % rs) bbnds*)
                         as #(add-scopes n (name-to name count type (+ n outer) %))]
                     (r/Poly-maker n 
                             (mapv #(r/visit-bounds % as) bbnds)
                             (as body)))))

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
                                     (as body)))))

(f/add-fold-case ::instantiate-many
               Poly
               (fn [{bbnds* :bbnds n :nbound body* :scope :as poly} {{:keys [replace count outer image sb type]} :locals}]
                 (let [rs #(remove-scopes n %)
                       body (rs body*)
                       bbnds (mapv #(r/visit-bounds % rs) bbnds*)
                       as #(add-scopes n (replace image count type (+ n outer) %))]
                   (r/Poly-maker n 
                           (mapv #(r/visit-bounds % as) bbnds)
                           (as body)))))

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

(t/ann ^:no-check instantiate-many [(Seqable Symbol) p/IScope -> r/Type])
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
             :object (or/->Path [(path/->KeyPE kw)] 0)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Heterogenous type ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; utility functions

(t/tc-ignore
(defn type-into-vector [x] (if (r/Union? x) (:types x) [x]))

(defn resolved-type-vector [t]
  {:post [(every? r/Type? %)]}
  (cond
   (r/TCResult? t)
   (doall
     (map fully-resolve-type
          (type-into-vector (-> t :t fully-resolve-type))))
   
   (r/Type? t)
   (doall 
     (map fully-resolve-type (type-into-vector (fully-resolve-type t))))
   
   :else
   [t]))

(defn union-or-nil [ts]
  (if (some nil? ts) nil (apply Un ts)))

(defn reduce-type-transform
  "Given a function f, left hand type t, and arguments, reduce the function
  over the left hand types with each argument in turn.
  
  Arguments will not be touched, it is up to f to resolve TCResults as needed.
  However, unions returned by f will be expanded, so the left hand type argument
  will not be a (raw) Union.
  
  Reduction is skipped once nil is returned, or optional predicate :when
  returns false."
  [func t args & {pred :when}]
  {:post [((some-fn nil? r/Type?) %)]}
  (let [ok? #(and % (if pred (pred %) true))]
    (union-or-nil
     (reduce
      (fn [left-types arg]
        (if (every? ok? left-types)
          (for [left left-types
                res (type-into-vector (func left arg))]
            res)
          [nil]))
      (resolved-type-vector t)
      args))))

;supporting assoc functionality

(defprotocol AssocableType
  (-assoc-pair [left kv]))

(extend-protocol AssocableType
  ; use the upper bound if bounds below (Map Any Any)
  F
  (-assoc-pair
    [{:keys [name] :as f} assoc-entry]
    (let [bnd (free-ops/free-with-name-bnds name)
          _ (when-not bnd
              (u/int-error (str "No bounds for type variable: " name bnds/*current-tvar-bnds*)))]
      (when (subtype? (:upper-bound bnd) (RClass-of IPersistentMap [r/-any r/-any]))
        (r/AssocType-maker f [(mapv r/ret-t assoc-entry)] nil))))

  Value
  (-assoc-pair
   [v [kt vt]]
   (when (r/Nil? v)
     (let [rkt (-> kt :t fully-resolve-type)]
       (if (keyword-value? rkt)
         (-complete-hmap {rkt (:t vt)})
         (RClass-of IPersistentMap [rkt (:t vt)])
         ))))
  
  RClass
  (-assoc-pair
   [rc [kt vt]]
   (let [rkt (-> kt :t fully-resolve-type)]
     (cond
      (= (:the-class rc) 'clojure.lang.IPersistentMap)
      (RClass-of IPersistentMap [(Un (:t kt) (nth (:poly? rc) 0))
                                   (Un (:t vt) (nth (:poly? rc) 1))])
      
      (and (= (:the-class rc) 'clojure.lang.IPersistentVector)
           (r/Value? rkt))
      (let [kt ^Value rkt]
        (when (integer? (.val kt))
          (RClass-of IPersistentVector [(Un (:t vt) (nth (:poly? rc) 0))])))
      )))
  
  HeterogeneousMap
  (-assoc-pair
   [hmap [kt vt]]
   (let [rkt (-> kt :t fully-resolve-type)]
     (if (keyword-value? rkt)
       (-> (assoc-in hmap [:types rkt] (:t vt))
           (update-in [:absent-keys] disj rkt))
       ; devolve the map
       ;; todo: probably some machinery I can reuse here?
       (RClass-of IPersistentMap [(apply Un (concat [rkt] (keys (:types hmap))))
                                  (apply Un (concat [(:t vt)] (vals (:types hmap))))])
       )))
  
  HeterogeneousVector
  (-assoc-pair
   [v [kt vt]]
   (let [rkt (-> kt :t fully-resolve-type)]
     (when (r/Value? rkt)
       (let [^Value kt rkt
             k (.val kt)] 
         (when (and (integer? k) (<= k (count (:types v))))
           (r/-hvec (assoc (:types v) k (:t vt))
                    :filters (assoc (:fs v) k (:fl vt))
                    :objects (assoc (:objects v) k (:o vt))))))))
  
  DataType
  (-assoc-pair
   [dt [kt vt]]
   (let [rkt (-> kt :t fully-resolve-type)]
     (when (and (r/Record? dt) (keyword-value? rkt))
       (let [^Value kt rkt
             field-type (when (keyword-value? kt)
                          (get (.fields dt) (symbol (name (.val kt)))))]
         (when (and field-type (subtype? (:t vt) field-type))
           dt))))))

(defn assoc-type-pairs [t & pairs]
  {:pre [(r/Type? t)
         (every? (fn [[k v :as kv]]
                   (and (= 2 (count kv))
                        (r/TCResult? k)
                        (r/TCResult? v)))
                 pairs)]
   :post [((some-fn nil? r/Type?) %)]}
  (reduce-type-transform -assoc-pair t pairs
                         :when #(satisfies? AssocableType %)))

(defn assoc-pairs-noret [t & pairs]
  {:pre [(r/Type? t)
         (every? (fn [[k v :as kv]]
                   (and (= 2 (count kv))
                        (r/Type? k)
                        (r/Type? v)))
                 pairs)]
   :post [((some-fn nil? r/Type?) %)]}
  (apply assoc-type-pairs t (map (fn [[k v]] [(r/ret k) (r/ret v)]) pairs)))

; dissoc support functions
(defn- -dissoc-key [t k]
  {:pre [(r/Type? t)
         (r/TCResult? k)]
   :post [((some-fn nil? r/Type?) %)]}
  (union-or-nil
   (for [rtype (resolved-type-vector k)]
     (cond
      (r/Nil? t)
      t
      
      (and (r/HeterogeneousMap? t) (keyword-value? rtype))
      (if (:other-keys? t)
        (-> (update-in t [:types] dissoc rtype)
            (update-in [:absent-keys] conj rtype))
        (update-in t [:types] dissoc rtype))
      
      (subtype? t (RClass-of IPersistentMap [r/-any r/-any]))
      t
      ))))

(defn dissoc-keys [t ks]
  {:post [((some-fn nil? r/Type?) %)]}
  (reduce-type-transform -dissoc-key t ks))

; merge support functions
(defn- merge-hmaps
  "Merges two HMaps into one, right into left.
  
  Preserves all key information where possible, missing keys in a right hand incomplete
  map will erase type information for those keys in the left.
  
  This strategy allows a merge of HMaps to always stay an HMap, without having to drop
  down to an IPersistentMap.
  
  For example:
  (merge {:a 4 :b 6} '{:b 5}) -> '{:a Any :b 5}"
  [left right]
  {:pre [(r/HeterogeneousMap? left)
         (r/HeterogeneousMap? right)]}
  
  (let [; update lhs with known types
        first-pass (apply assoc-type-pairs left (map (fn [[k t]]
                                                       [(r/ret k) (r/ret t)])
                                                     (:types right)))
        ; clear missing types when incomplete rhs and lhs still hmap
        second-pass (if (and (r/HeterogeneousMap? first-pass) (:other-keys? right))
                      (reduce
                       (fn [t [lk lv]]
                         (if (and t
                                  ; left type not in right and not absent
                                  (not (get (:types right) lk))
                                  (not (get (:absent-keys right) lk)))
                           (assoc-type-pairs t [(r/ret lk)
                                                (r/ret r/-any)])
                           t))
                       first-pass
                       (:types left))
                      first-pass)
        ; ensure :other-keys? updated appropriately
        final-pass (when (r/HeterogeneousMap? second-pass)
                     (update-in second-pass [:other-keys?]
                                #(or % (:other-keys? right))))]
    final-pass))

(defn- merge-pair
  [left right]
  {:pre [(r/Type? left)
         (r/TCResult? right)]
   :post [((some-fn nil? r/Type?) %)]}
  (let [sub-class? #(subtype? %1 (RClass-of %2 %3))
        left-map (sub-class? left IPersistentMap [r/-any r/-any])
        right-map (sub-class? (ret-t right) IPersistentMap [r/-any r/-any])]
    (cond
     ; preserve the rhand alias when possible
     (and (r/Nil? left) right-map)
     (ret-t right)
     
     :else
     (union-or-nil
      (for [rtype (resolved-type-vector (ret-t right))]
        (cond
         (and (or left-map (r/Nil? left))
              (r/Nil? rtype))
         left
         
         (and (r/Nil? left) (sub-class? rtype IPersistentMap [r/-any r/-any]))
         rtype
         
         (and (r/HeterogeneousMap? left) (r/HeterogeneousMap? rtype))
         (merge-hmaps left rtype)
         
         (and (not (sub-class? left IPersistentVector [r/-any]))
              (satisfies? AssocableType left)
              (r/HeterogeneousMap? rtype))
         (apply assoc-type-pairs left (map (fn [[k t]]
                                             [(r/ret k) (r/ret t)])
                                           (:types rtype)))
         ))))))

(defn merge-types [left & r-tcresults]
  {:pre [(r/Type? left)
         (every? r/TCResult? r-tcresults)]
   :post [((some-fn nil? r/Type?) %)]}
  (reduce-type-transform merge-pair left r-tcresults))

; conj helper

(defn- conj-pair [left right]
  {:pre [(r/Type? left)
         (r/TCResult? right)]
   :post [((some-fn nil? r/TCResult?) right)]}
  (cond
   (r/HeterogeneousVector? left)
   (assoc-type-pairs left [(r/ret (r/-val (count (:types left))))
                           right])
   
   (r/Nil? left)
   (r/-hvec [(:t right)]
            :filters [(:fl right)]
            :objects [(:o right)])
   
   ; other rules need to unwrap the rhs
   :else
   (union-or-nil
    (for [rtype (resolved-type-vector right)]
      (cond
       (and (r/HeterogeneousMap? left)
            (r/HeterogeneousVector? rtype))
       (if (= (count (:types rtype)) 2)
         (assoc-type-pairs left (map r/ret (:types rtype)))
         (u/int-error "Need vector of length 2 to conj to map"))
       
       (and (r/HeterogeneousMap? left)
            (r/Nil? rtype))
       left
       )))))

(defn conj-types [left & rtypes]
  {:pre [(r/Type? left)
         (every? r/TCResult? rtypes)]
   :post [((some-fn nil? r/Type?) %)]}
  (reduce-type-transform conj-pair left rtypes))
)
