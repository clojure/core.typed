;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.annotator.join
  (:require [clojure.core.typed.annotator.rep :as r
             :refer [HMap? unknown? -class?
                     -class nothing? -val HVec?
                     type? val? Any? -any
                     union?]]
            [clojure.set :as set]
            [clojure.core.typed.annotator.pprint
             :refer [unp-str]]
            [clojure.core.typed.annotator.debug-macros
             :refer [debug-flat time-if-slow]]
            [clojure.core.typed.annotator.util
             :refer [HMap-common-req-keys
                     HMap-likely-tag-key
                     HMap-req-keyset
                     map-key-set
                     *preserve-unknown*
                     unparse-type]]
  ))

; default-should-join-HMaps? : HMap HMap -> Bool
(defn default-should-join-HMaps? [t1 t2]
  {:pre [(HMap? t1)
         (HMap? t2)]}
  ;; join if the required keys are the same,
  ;; and there is not common key mapped to keywords.
  ;; TODO and if 75% of the keys are the same
  ;; TODO and if common keys are not always different keywords
  (let [ts [t1 t2]
        t1-map (:clojure.core.typed.annotator.rep/HMap-req t1)
        t2-map (:clojure.core.typed.annotator.rep/HMap-req t2)
        res 
        (and
          (<= 2
              (count
                (set/intersection
                  (map-key-set t1-map)
                  (map-key-set t2-map))))
          (not
            (some
              #(HMap-likely-tag-key ts %)
              (HMap-common-req-keys ts)))
          ;; TODO
          #_
          (every?
            ;; should return true if we should merge
            ;; this entry
            (fn [[k left]]
              (let [right (t2-map k)]
                (or (= left right)
                    (not ((every-pred (comp #{:val} :op)
                                      (comp keyword? :val))
                          left
                          right)))))
            t1-map))
        ]
    res
    ))

(declare HMap-join-strategies)

(def current-HMap-join-strategy :default)

; should-join-HMaps? : HMap HMap -> Bool
(defn should-join-HMaps? [t1 t2]
  ((get-in HMap-join-strategies [current-HMap-join-strategy :should-join-HMaps?])
   t1 t2))

(declare join* join)

(defn upcast-HVec [h]
  {:pre [(#{:HVec} (:op h))]
   :post [(type? %)]}
  (-class :vector 
          [(apply join* (:vec h))]))

(defn default-join-HMaps [t1 t2]
  {:pre [(HMap? t1)
         (HMap? t2)
         ;(should-join-HMaps? t1 t2)
         ]
   :post [(HMap? %)]}
  ;(prn "joining HMaps"
  ;     (unparse-type t1)
  ;     (unparse-type t2))
  (let [t1-req (:clojure.core.typed.annotator.rep/HMap-req t1)
        t2-req (:clojure.core.typed.annotator.rep/HMap-req t2)
        t1-opt (:clojure.core.typed.annotator.rep/HMap-opt t1)
        t2-opt (:clojure.core.typed.annotator.rep/HMap-opt t2)
        all-reqs (set/union
                   (map-key-set t1-req)
                   (map-key-set t2-req))
        common-reqs (set/intersection
                      (map-key-set t1-req)
                      (map-key-set t2-req))
        ;; optional keys
        new-opts (set/union
                   (map-key-set t1-opt)
                   (map-key-set t2-opt)
                   (set/difference
                     all-reqs
                     common-reqs))
        ;; required if not optional in either
        new-reqs (set/difference
                   common-reqs
                   new-opts)
        res ;(debug
            ;  (println "Joining HMaps:")
              {:op :HMap
               :clojure.core.typed.annotator.rep/HMap-req (into {}
                                (map (fn [k]
                                       {:pre [(keyword? k)]}
                                       (let [ts (keep k [t1-req t2-req])]
                                         ;(prn "req k" k)
                                         ;(prn "ts" ts)
                                         (assert (seq ts))
                                         [k (apply join* ts)])))
                                new-reqs)
               :clojure.core.typed.annotator.rep/HMap-opt (into {}
                                (map (fn [k]
                                       {:pre [(keyword? k)]}
                                       (let [ts (keep k [t1-req t2-req
                                                         t1-opt t2-opt])]
                                         (assert (seq ts))
                                         [k (apply join* ts)])))
                                new-opts)}
              ;)
        ]
    (debug-flat 
      (println "joint HMaps:"
               (unp-str res)))
    res
    ))

(def HMap-join-strategies
  {:default {:should-join-HMaps? #'default-should-join-HMaps?
             :join-HMaps #'default-join-HMaps}})

(defn join-HMaps [t1 t2]
  ((get-in HMap-join-strategies [current-HMap-join-strategy :join-HMaps])
   t1 t2))

(defn merge-HMaps [ms]
  {:post [(HMap? %)]}
  (reduce join-HMaps (first ms) (rest ms)))

(defn flatten-union [t]
  {:pre [(r/type? t)]
   :post [(set? %)]}
  (if (#{:union} (:op t))
    (into #{}
          (mapcat flatten-union)
          (:types t))
    #{t}))

(defn flatten-unions [ts]
  {:pre [;; very slow
         #_(every? r/type? ts)]
   :post [(set? %)]}
  (into #{} 
        (mapcat flatten-union)
        ts))

(defn make-Union [args]
  ;(debug (println "make-Union")
  (let [ts (flatten-unions args)
        {hmaps true non-hmaps false} (group-by HMap? ts)
        hmaps (set hmaps)
        ;_ (debug-flat (println "hmaps:" (mapv unp-str hmaps)))
        common-keys (or (when (seq hmaps)
                          (HMap-common-req-keys hmaps))
                        #{})
        ;_ (when (seq hmaps)
        ;    (debug-flat (println "common-keys:"
        ;                         (pr-str common-keys))))
        likely-tag
        (some #(HMap-likely-tag-key hmaps %) common-keys)
        ;_ (prn "likely-tag" likely-tag)
        ;_ (when (seq hmaps)
        ;    (debug-flat (println "likely-tag:"
        ;                         (pr-str likely-tag))))
        hmaps-merged (let [hmap-by-keys (when (and (seq hmaps)
                                                   (not likely-tag))
                                          (group-by HMap-req-keyset hmaps))]
                       ;(prn "hmap-by-keys" hmap-by-keys)
                       ;; if we don't have common keys, collapse everything.
                       (if hmap-by-keys
                         ;(debug
                         ;  (println "make-Union: No common key, merging by keys")
                           (into #{}
                                 (map merge-HMaps)
                                 (vals hmap-by-keys)
                           )
                           ;)
                         hmaps))
        likely-tag-for-union (atom nil)
        ;_ (prn "merged" (mapv unp hmaps-merged))
        ;; if we have more than one keyset, then we must
        ;; have "tagged" maps with a common keyword entry (eg. :op, :type).
        ;; This ensures we don't keep too much information about generic maps.
        hmaps-merged (if (> (count hmaps-merged) 1)
                       (let [_ (assert (every? HMap? hmaps-merged))
                             ]
                         ;(prn "common-keys" common-keys)
                         (cond
                           ;; no keys in common, upcast all maps
                           (empty? common-keys)
                           (do
                             ;(debug-flat (println "no common keys, upcasting to (Map Any Any)"))
                             ;#{(-class :map [-any -any])}
                             #{{:op :HMap
                                :clojure.core.typed.annotator.rep/HMap-req {}
                                :clojure.core.typed.annotator.rep/HMap-opt (apply merge-with join (mapcat (juxt :clojure.core.typed.annotator.rep/HMap-req :clojure.core.typed.annotator.rep/HMap-opt) hmaps-merged))}})

                           ;; if one of the common keys is always mapped to a singleton keyword,
                           ;; merge by the value of this key
                           likely-tag
                           (let [by-tag (group-by (fn [m]
                                                    (get (:clojure.core.typed.annotator.rep/HMap-req m) likely-tag))
                                                  hmaps-merged)
                                 new-maps (into #{}
                                                (map merge-HMaps)
                                                (vals by-tag))]
                             ;(debug-flat
                             ;  (println "combined HMaps:"
                             ;           (mapv unp-str new-maps)))
                             (reset! likely-tag-for-union likely-tag)
                             ;(prn "likely-tag" likely-tag)
                             ;(prn "by-tag" by-tag)
                             ;(prn "new-maps" new-maps)
                             new-maps)

                           ;; merge common keys as required, rest are optional
                           ;; FIXME this is too aggressive for maps that have
                           ;; clashing dispatch keys.
                           :else
                           (let [has-unknown? (atom false)
                                 res
                                 #{{:op :HMap
                                    ;; put all the common required keys as required
                                    :clojure.core.typed.annotator.rep/HMap-req (apply merge-with join
                                                      (map (fn [m]
                                                             {:pre [(HMap? m)]}
                                                             (let [es (select-keys (:clojure.core.typed.annotator.rep/HMap-req m) 
                                                                                   common-keys)]
                                                               (doseq [[_ v] es]
                                                                 (when (unknown? v)
                                                                   (reset! has-unknown? true)))
                                                               es))
                                                           hmaps-merged))
                                    ;; all the rest are optional
                                    :clojure.core.typed.annotator.rep/HMap-opt (apply merge-with join
                                                      (map (fn [m]
                                                             {:pre [(HMap? m)]}
                                                             (let [es 
                                                                   (merge-with join
                                                                               (:clojure.core.typed.annotator.rep/HMap-opt m)
                                                                               (apply dissoc (:clojure.core.typed.annotator.rep/HMap-req m)
                                                                                      common-keys))]
                                                               (doseq [[_ v] es]
                                                                 (when (unknown? v)
                                                                   (reset! has-unknown? true)))
                                                               es))
                                                           hmaps-merged))
                                    }}]
                             (if @has-unknown?
                               hmaps-merged
                               res))))
                       hmaps-merged)
        ;_ (prn "merged" hmaps-merged)
        ;_ (prn "hmaps-merged" (map unparse-type hmaps-merged))
        ;; join all common classes by their arguments, regardless of their variance
        non-hmaps (let [{classes true non-classes false} (group-by -class? non-hmaps)
                        ;; important invariant: all these classes take 1 argument. This is used in
                        ;; the upcasting logic below.
                        relevant-seqables #{:seq :coll :list :vector}
                        ;; upcast seqables if appropriate
                        classes (let [{seqable-classes true 
                                       non-seqable-classes false} 
                                      (group-by #(contains? relevant-seqables (:clojure.core.typed.annotator.rep/class-instance %)) classes)
                                      seqable-classes
                                      (if (some (comp #{:list :seq :coll}
                                                      :clojure.core.typed.annotator.rep/class-instance)
                                                seqable-classes)
                                        ;; upcast all to Coll since we've probably lost too much type information
                                        ;; to bother keeping seqable-classes around.
                                        [(-class :coll 
                                                 [(apply join* 
                                                         ;; assume all seqable-classes take a collection
                                                         ;; member type parameter
                                                         (map (comp first :args) seqable-classes))])]
                                        seqable-classes)]
                                  (concat seqable-classes non-seqable-classes))
                        classes (into #{}
                                      (map (fn [cs]
                                             {:pre [(seq cs)
                                                    (every? -class? cs)
                                                    (apply = (map (comp count :args) cs))]}
                                             (-class (-> cs first :clojure.core.typed.annotator.rep/class-instance)
                                                     (apply mapv join* (map :args cs)))))
                                     (vals (group-by :clojure.core.typed.annotator.rep/class-instance classes)))]
                    (into classes non-classes))

        ;; delete HMaps if there's already a Map in this union,
        ;; unless it's a (Map Nothing Nothing)
        hmaps-merged (if (some (fn [m]
                                 (and (-class? m)
                                      (#{:map} (:clojure.core.typed.annotator.rep/class-instance m))
                                      (not-every? nothing? (:args m))))
                               non-hmaps)
                       #{}
                       hmaps-merged)

        ts (into hmaps-merged non-hmaps)
        
        ;; upcast true/false singletons to Boolean if Boolean is present
        ts (if (contains? ts (-class :boolean []))
             (disj ts (-val true) (-val false))
             ts)

        _ (assert (set? ts))
        ;; upcast Long and Double combination to t/Num
        ts (cond
             (or (and (or (contains? ts (-class :int []))
                          (contains? ts (-class :integer [])))
                      (or (contains? ts (-class :double []))
                          (contains? ts (-class :decimal []))))
                 (contains? ts (-class :number [])))
             (-> (disj ts 
                       (-class :int [])
                       (-class :integer [])
                       (-class :double [])
                       (-class :decimal []))
                 (conj (-class :number [])))

             :else ts)

        ;; simplify HVec's
        ts (let [merge-same-length-HVecs (fn [hvs]
                                           {:pre [(apply = (map (comp count :vec) hvs))]
                                            :post [(HVec? %)]}
                                           {:op :HVec
                                            :vec (apply mapv join* (map :vec hvs))})
                 {HVecs true non-HVecs false} (group-by (comp boolean #{:HVec} :op) ts)
                 by-count (group-by (comp count :vec) HVecs)
                 ;; erase HVec's if we have two different length HVec's
                 should-collapse-HVecs? (< 1 (count by-count))
                 [HVecs non-HVecs] (if should-collapse-HVecs?
                                     ;; upcast HVecs
                                     [[] (concat non-HVecs (map upcast-HVec HVecs))]
                                     [(mapv merge-same-length-HVecs (vals by-count))
                                      non-HVecs])
                 _ (assert (every? HVec? HVecs))
                 ;; if needed, upcast all HVec's
                 [HVecs non-HVecs]
                 (let [;; at this point, collection classes are normalized to either IPC or IPV.
                       {vec-classes true non-vecs false}
                       (group-by
                         (every-pred
                           -class?
                           (comp boolean #{:vector :coll} :clojure.core.typed.annotator.rep/class-instance))
                         non-HVecs)
                       _ (assert (= (count non-HVecs) (+ (count vec-classes) (count non-vecs))))
                       ;; erase HVec's if we have a IPV class
                       [HVecs vec-classes]
                       (if (seq vec-classes)
                         [[]
                          (cons 
                            (let [class-name (if (every? (comp boolean #{:vector} :clojure.core.typed.annotator.rep/class-instance) vec-classes)
                                               :vector
                                               :coll)
                                  upcasted-HVecs (map upcast-HVec HVecs)]
                              (-class class-name
                                      [(apply join*
                                              (concat
                                                (map (comp first :args) (concat vec-classes upcasted-HVecs))))]))
                            vec-classes)]
                         [HVecs vec-classes])]
                   [HVecs (concat vec-classes non-vecs)])
                 ]
             (into (set non-HVecs) HVecs))

        
        ;; simplify multiple keywords to Kw if
        ;ts (let [{kws true non-kws false} (group-by kw-val? ts)]
        ;     (if (>= (count kws) 2)  ;; tweak simplification threshold here
        ;       (conj (set non-kws) (-class :keyword []))
        ;       ts))
        seqable-t? (fn [m]
                     (boolean
                       (when (-class? m)
                         ;; while string is "seqable", it mixes fine with named things.
                         ;; we don't include :string here so (U Str Sym) is preserved
                         ;; and not upcast to Any.
                         (#{:vector :map :coll :seqable} (:clojure.core.typed.annotator.rep/class-instance m)))))
        atomic-type? (fn [v]
                       (boolean
                         (or
                           (and (val? v)
                                (some? (:val v)))
                           (and (-class? v)
                                (#{:symbol :string :keyword}
                                  (:clojure.core.typed.annotator.rep/class-instance v))))))
        ]
    ;(prn "union ts" ts)
    (assert (set? ts))
    (assert (every? (complement #{:union}) (map :op ts)))
    (cond
      (= 1 (count ts)) (first ts)

      ;; simplify to Any
      (some Any? ts) -any

      ;; if there's a mix of collections and non-collection values,
      ;; return Any
      ;; Allow `nil` liberally.
      (let []
        (and ;; either a map or seqable with an atom
             (or (seq hmaps-merged)
                 (seq (filter seqable-t? non-hmaps)))
             (seq (filter atomic-type? non-hmaps))))
      (do
        ;(prn "simplifying mix of collections and singleton values")
        ;(prn "hmaps" (seq hmaps-merged))
        ;(prn "seqables" (filter seqable-t? non-hmaps))
        ;(prn "atomics" (filter atomic-type? non-hmaps))
        -any)

      :else 
      (let [k @likely-tag-for-union]
        (merge
          {:op :union
           :types (mapv (fn [t]
                          (if (and (HMap? t)
                                   k)
                            (vary-meta t assoc ::union-likely-tag k)
                            t))
                        ts)}
          #_
          (when-let [k @likely-tag-for-union]
            {::union-likely-tag k})))))
;)
)

;; `as` is a list of :IFn1 nodes with the same arity
(defn join-IFn1 [as]
  {:pre [(seq as)
         (every? #{[:IFn1 (-> as first :dom count)]}
                 (map (juxt :op (comp count :dom))
                      as))]
   :post [(#{:IFn1} (:op %))]}
  (merge 
    {:op :IFn1
     :dom (apply mapv
                 (fn [& [dom & doms]]
                   {:pre [dom]}
                   ;(prn "join IFn IFn dom" (map :op (cons dom doms)))
                   (apply join* dom doms))
                 (map :dom as))
     :rng (let [[rng & rngs] (map :rng as)]
            (assert rng)
            (apply join* rng rngs))
     :rest (let [all-rests (keep :rest as)]
             (when (seq all-rests)
               (apply join* all-rests)))}))

;; How to choose if we have kwargs.
;;
;; - after some fixed number of arguments, the # arguments
;;   need to be multiples of 2
;; - after the fixed arguments, for each pair [k v]
;;   - k must be a keyword
;;   - v can be anything
(defn group-arities [t1 t2]
  {:pre [(#{:IFn} (:op t1))
         (#{:IFn} (:op t2))]}
  (vals
    (group-by (comp count :dom)
              (concat (:arities t1)
                      (:arities t2)))))

(defn join-IFn [t1 t2]
  {:pre [(#{:IFn} (:op t1))
         (#{:IFn} (:op t2))]
   :post [(type? %)]}
  (let [grouped (group-arities t1 t2)
        arities (mapv join-IFn1 grouped)]
    {:op :IFn
     :arities arities}))

; join : Type Type -> Type
(defn join [t1 t2]
  {:pre [(type? t1)
         (type? t2)]
   :post [(type? %)]}
  (time-if-slow
    (binding [*preserve-unknown* true]
      (str "Join was slow on arguments:" 
           (:op t1) (:op t2)
           (unparse-type t1) (unparse-type t2)))
  (let [;id (gensym (apply str (map :op [t1 t2])))
        ;_ (prn "join" id (unparse-type t1) (unparse-type t2))
        res (cond
              (= t1 t2) t1

              ;; annihilate unknown
              (unknown? t1) t2
              (unknown? t2) t1

              (or (union? t1) (union? t2))
              (apply join* (flatten-unions [t1 t2]))

              (and (#{:poly} (:op t1))
                   (#{:poly} (:op t2)))
              {:op :poly
               :known-params (into (:known-params t1)
                                   (:known-params t2))
               :params (merge-with (fn [{w1 :weight
                                         v1 :name
                                         t1 :types}
                                        {w2 :weight
                                         v2 :name
                                         t2 :types}]
                                     ;; throw away v2
                                     ;(prn "Merging:" w1 w2)
                                     {:weight (+ w1 w2)
                                      :name v1
                                      :types (into t1 t2)})
                                   (:params t1) (:params t2))
               :types (join (:type t1) (:type t2))}

              (#{:poly} (:op t1))
              (update t1 :type join t2)
              (#{:poly} (:op t2))
              (update t2 :type join t1)

              (and (#{:class} (:op t1))
                   (#{:class} (:op t2))
                   (= (:clojure.core.typed.annotator.rep/class-instance t1)
                      (:clojure.core.typed.annotator.rep/class-instance t2))
                   (= (count (:args t1))
                      (count (:args t2))))
              (-class (:clojure.core.typed.annotator.rep/class-instance t1) (mapv join (:args t1) (:args t2)))

              (and (#{:class} (:op t1))
                   (= :ifn
                      (:clojure.core.typed.annotator.rep/class-instance t1))
                   (#{:IFn} (:op t2)))
              t2

              (and (#{:class} (:op t2))
                   (= :ifn
                      (:clojure.core.typed.annotator.rep/class-instance t2))
                   (#{:IFn} (:op t1)))
              t1

              (and (HMap? t1)
                   (HMap? t2)
                   (should-join-HMaps? t1 t2))
              (join-HMaps t1 t2)

              (and (#{:IFn} (:op t1))
                   (#{:IFn} (:op t2)))
              (join-IFn t1 t2)

              (and (#{:HVec} (:op t1))
                   (#{:HVec} (:op t2))
                   (= (count (:vec t1)) (count (:vec t2))))
              {:op :HVec
               :vec (mapv join (:vec t1) (:vec t2))}

              :else 
              (let []
                ;(prn "join union fall through")
                (make-Union [t1 t2])))]
    ;(prn "join result" id (unparse-type res))
    res)))

(defn join* [& args]
  #_(when (< 5 (count args))
    (prn "join* large arguments" (count args)))
  (letfn [(merge-type [t as]
            {:pre [(type? t)
                   (not (union? t))
                   (set? as)]
             :post [(set? %)]}
            ;(prn "merge-type" (unparse-type t) (mapv unparse-type as))
            (let [ms (into #{}
                           (comp
                             (map #(join t %))
                             ;(mapcat flatten-union)
                             )
                           (flatten-unions as))
                  res (cond
                        (empty? ms) #{t}
                        :else ms)]
              ;(prn "merge-type result" (map unparse-type res))
              res))]
    (make-Union
      (reduce (fn [as t]
                (merge-type t as))
              #{}
              (flatten-unions args)))))

