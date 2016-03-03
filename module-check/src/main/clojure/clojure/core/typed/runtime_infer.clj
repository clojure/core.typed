(ns clojure.core.typed.runtime-infer
  (:require [clojure.pprint :refer [pprint]]
            [clojure.core.typed :as t]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.typed.ast-utils :as ast]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.debug :refer [dbg]]
            [clojure.tools.reader.reader-types :as rdrt]
            [clojure.tools.namespace.parse :as nprs]
            [clojure.core.typed.coerce-utils :as coerce]))

#_
(defalias Type
  (U '{:op :val :val Any}
     '{:op :HMap :map (Map Kw Type)}
     '{:op :HVec :vec (Vec Type)}
     '{:op :union :types (Set Type)}
     '{:op :class 
       :class Class
       :args (Vec Type)}
     '{:op :IFn 
       :arities (Vec '{:op :IFn1
                       :dom (Vec Type)
                       :rng Type})}
     '{:op :unknown}
     '{:op :poly
       :known-params (Vec Sym)
       :params (Map (Set Path) 
                    {:weight Int
                     :name Sym
                     :types (Set Kw)})
       :type Type}
     '{:op :alias
       :name Sym}
     '{:op :free
       :name Sym}
     '{:op :Top}))

#_
(defalias PathElem
  (U '{:op :var
       ;; namespace for which we're inferring.
       ;; If nil, infer globally (probably for testing
       ;; purposes only.
       :ns (U nil Sym)
       ;; qualified var name
       :name Sym}
     '{:op :fn-domain
       :arity Int
       :position Int}
     '{:op :fn-range
       :arity Int}
     '{:op :set-entry}
     '{:op :seq-entry}
     '{:op :transient-vector-entry}
     '{:op :atom-contents}
     '{:op :key
       :keys (Set Kw)
       :key Kw}
     '{:op :index
       :count Int
       :nth Int}))

#_
(defalias Path
  (Vec Path)) ; path extends to the right, ie. [x :dom :rng] is x's domain's range.

#_
(defalias InferResult
  '{:op ':path-type
    :path Path
    :type Type})

#_
(defalias Equiv
  '{:op ':equiv
    := (Vec Path)
    :type (U ':nil
             ':keyword
             ':fn
             ':number
             ':symbol
             ':string
             ':bool
             ':coll
             ':other)})

#_
(defalias TypeEnv
  (Map Sym Type))

#_
(defalias AliasEnv
  (Map Sym Type))

#_
(defalias AliasTypeEnv
  '{:alias-env AliasEnv
    :type-env TypeEnv})

#_
(defalias InferResultEnv
  '{:infer-results (Set InferResult)
    :equivs (Vec Equiv)
    :path-occ (Map Path Int)})

#_
(defalias Envs
  (Map Sym AliasTypeEnv))

(def ^:dynamic *envs*
  (atom {}))

(declare current-ns)

(defn get-env [env] 
  {:pre [(map? env)]}
  (get env (current-ns)))

(defn type-env 
  ;([] (type-env @*envs*))
  ([env] (get (get-env env) :type-env)))

(defn alias-env 
  ;([] (alias-env @*envs*))
  ([env] (get (get-env env) :alias-env)))

(defn init-config []
  {})

(defn init-env []
  {}
  #_{(current-ns)
   {:type-env {}
    :alias-env {}}})

(defn update-env [env & args]
  (apply update env (current-ns) args))

(defn update-type-env-in-ns [env ns & args]
  (apply update-in env [(ns-name ns) :type-env] args))

(defn update-type-env [env & args]
  (apply update-type-env-in-ns env (current-ns) args))

(defn update-alias-env [env & args]
  (apply update-in env [(current-ns) :alias-env] args))

(defn initial-results 
  ([] (initial-results nil []))
  ([parent base]
   {:infer-results #{}
    :equivs []
    ;; TODO
    :path-occ {}}))

; results-atom : (Atom InferResultEnv)
(def results-atom (atom (initial-results)
                        :validator
                        (fn [m]
                          (and (map? m)
                               (-> m :infer-results set?)
                               (-> m :path-occ map?)
                               (-> m :equivs vector?)))))

(defn add-infer-result! [results-atom r]
  (swap! results-atom update :infer-results conj r))

(defn get-infer-results [results-atom]
  (get @results-atom :infer-results))

(defn infer-result [path type]
  {:op :path-type
   :type type
   :path path})

(defn equiv-result [ps cls]
  {:pre [(set? ps)
         (= 2 (count ps))
         (keyword? cls)]}
  {:op :equiv
   := ps
   :type cls})

(defn var-path 
  ([name] (var-path nil name))
  ([ns name]
   {:pre [((some-fn symbol? nil?) ns)
          (symbol? name)]}
   {:op :var
    :ns ns
    :name name}))

(defn -alias [name]
  {:pre [(symbol? name)]}
  {:op :alias
   :name name})

(defn -unknown []
  {:op :unknown})

(defn fn-dom-path [arity pos]
  (assert (< pos arity)
          (str "Arity: " arity
               "Position:" pos))
  {:op :fn-domain
   :arity arity :position pos})

(defn fn-rng-path [arity]
  {:op :fn-range
   :arity arity})

(defn key-path [keys key]
  {:op :key
   :keys keys
   :key key})

(defn index-path [count nth]
  {:op :index
   :count count
   :nth nth})

(defn seq-entry []
  {:op :seq-entry})

(defn set-entry []
  {:op :set-entry})

(defn transient-vector-entry []
  {:op :transient-vector-entry})

(defn atom-contents []
  {:op :atom-contents})

(defn type? [t]
  (and (map? t)
       (keyword? (:op t))))

(defn -class [cls args]
  {:pre [(class? cls)
         (vector? args)
         (every? type? args)]}
  {:op :class
   :class cls
   :args args})

(defn -val [v]
  {:op :val
   :val v})

(defn make-free [name]
  {:pre [(symbol? name)]}
  {:op :free
   :name name})

(declare parse-path-elem parse-type)

(defn parse-infer-result [[p _ t]]
  {:path (mapv parse-path-elem p)
   :type (parse-type t)})

(declare unparse-path-elem unparse-type)

(defn unparse-path [ps]
  (mapv unparse-path-elem ps))

(defn unparse-infer-result [p]
  [(unparse-path (:path p) :- (unparse-type (:type p)))])

(defn parse-path-elem [p]
  (case (first p)
    val (-val (second p))
    class (-class (resolve (second p)) (mapv parse-type (nth p 2)))
    key (key-path (second p) (nth p 2))
    rng (fn-rng-path (second p))
    dom (fn-dom-path (second p) (nth p 2))
    var (var-path (second p))
    index (index-path (second p) (nth p 2))))

(defn unparse-path-elem [p]
  (case (:op p)
    :val (list 'val (:val p))
    :class (list 'class (symbol (.getName ^Class (:class p)))
                 (mapv unparse-type (:args p)))
    :key (list 'key (:keys p) (:key p))
    :fn-range (list 'rng (:arity p))
    :fn-domain (list 'dom (:arity p) (:position p))
    :var (list 'var (:name p))
    :index (list 'index (:count p) (:nth p))
    :set-entry (list 'set-entry)
    :seq-entry (list 'seq-entry)
    :transient-vector-entry (list 'transient-vector-entry)
    :atom-contents (list 'atom-contents)))


(defn HMap? [t]
  (= :HMap (:op t)))

(defn alias? [t]
  (= :alias (:op t)))

(defn union? [t]
  (= :union (:op t)))

(declare parse-type)

(defn parse-arity [a]
  (let [[doms [_->_ rng :as rng-arrow]] (split-with (complement #{:->}) a)
        _ (assert (= 2 (count rng-arrow)))]
    {:op :IFn1
     :dom (mapv parse-type doms)
     :rng (parse-type rng)}))

(defn parse-HVec [v]
  {:op :HVec 
   :vec (mapv parse-type v)})

(defn parse-HMap [m]
  {:op :HMap
   :map (into {}
              (map (fn [[k v]]
                     [k (parse-type v)]))
              m)})

(declare make-Union resolve-alias postwalk)

(def ^:dynamic *type-var-scope* #{})

(defn HMap-keyset [t]
  {:pre [(HMap? t)]
   :post [(set? %)]}
  (set (keys (:map t))))

; keysets : Env Type (Set Type) -> Keysets
(defn keysets
  ([env t] (keysets env t #{}))
  ([env t seen]
   {:post [(set? %)
           (every? set? %)]}
   ;(prn "keysets" (unparse-type t))
   (let [keysets
         (fn 
           ([t] (keysets env t seen))
           ([t seen] (keysets env t seen)))]
     (case (:op t)
       :HMap #{(HMap-keyset t)}
       :union (into #{}
                    (mapcat keysets)
                    (:types t))
       :alias (if (seen t)
                #{}
                (keysets (resolve-alias env t)
                         (conj seen t)))
       #{}))))

(defn subst-alias [t old new]
  {:pre [(type? t)
         (alias? old)
         (type? new)]
   :post [(type? %)]}
  ;(prn "subst-alias t" (unparse-type t))
  ;(prn "old" (unparse-type old))
  ;(prn "new" (unparse-type new))
  (postwalk t
            (fn [c]
              (case (:op c)
                :alias (if (= c old)
                         new
                         c)
                c))))

(defn parse-type [m]
  (cond
    (= 'Any m) {:op :Top}
    (= '? m) {:op :unknown}

    (or (= nil m)
        (= false m)
        (keyword? m)) {:op :val :val m}

    (vector? m) {:op :IFn
                 :arities [(parse-arity m)]}

    (symbol? m) (case m
                  Nothing {:op :union :types #{}}
                  Sym (-class clojure.lang.Symbol [])
                  (cond
                    (contains? *type-var-scope* m)
                    {:op :var
                     :name m}

                    (contains? (alias-env @*envs*) m)
                    (-alias m)

                    :else
                    (do
                      (assert (class? (resolve m)) m)
                      {:op :class
                       :class (resolve m)
                       :args []})))
    (list? m) (case (first m)
                All (let [[vs t :as rst] (second m)
                          _ (assert (= 2 (count rst)))]
                      {:op :poly
                       :known-params (into []
                                           (map (fn [m]
                                                  {:pre [(symbol? m)]}
                                                  m))
                                           vs)
                       :params {}
                       :type (binding [*type-var-scope* (into *type-var-scope* vs)]
                               (parse-type t))})
                quote (let [in (second m)]
                        (cond
                          (vector? in) (parse-HVec in)
                          (map? in) (parse-HMap in)
                          (keyword? in) {:op :val :val in}
                          :else (assert nil (str "Bad quote: " m))))

                IFn {:op :IFn
                     :arities (mapv parse-arity (rest m))}
                U (make-Union
                    (into #{}
                          (map parse-type)
                          (rest m)))
                Vec (-class clojure.lang.IPersistentVector
                            [(parse-type (second m))])
                Set (-class clojure.lang.IPersistentSet
                            [(parse-type (second m))])
                Nothing (make-Union #{})
                (let [res (resolve (first m))]
                  (cond ;(contains? (alias-env @*envs*) (:name (first m)))
                        ;(-alias (first m))

                        (class? res) (-class res (mapv parse-type (drop 1 m)))
                        
                        :else (assert nil (str "What is this?" m)))))


    :else (assert nil (str "bad type " m))))

(defmacro prs [t]
  `(parse-type '~t))

(def ^:dynamic *unparse-abbrev-alias* false)
(def ^:dynamic *unparse-abbrev-class* false)

(def ^:dynamic *ann-for-ns* (fn [] *ns*))

(defn current-ns [] (ns-name (*ann-for-ns*)))

(defn qualify-symbol-in [ns s]
  {:pre [(symbol? s)
         (not (namespace s))]
   :post [(symbol? %)]}
  (let [talias (some
                 (fn [[k v]]
                   (when (= ns v)
                     k))
                 (ns-aliases (the-ns (current-ns))))]
    (symbol (or (when talias
                  (str talias))
                (str (ns-name ns)))
            (str s))))

(defn qualify-typed-symbol [s]
  {:pre [(symbol? s)]
   :post [(symbol? %)]}
  (qualify-symbol-in (the-ns 'clojure.core.typed) s))

(defn resolve-class [^Class c]
  {:pre [(class? c)]
   :post [(symbol? %)]}
  (symbol
    (if (= (ns-resolve (current-ns) (symbol (.getSimpleName c)))
           c)
      (.getSimpleName c)
      (.getName c))))

; [Node :-> Any]
(defn unparse-type' [{:as m}]
  (assert (type? m) m)
  (case (:op m)
    :alias (if *unparse-abbrev-alias*
             (-> (:name m) name symbol)
             (if (= (some-> (namespace (:name m)) symbol)
                    (current-ns))
               (symbol (name (:name m)))
               (:name m)))
    :val (let [t (:val m)]
           (cond
             ((some-fn nil? false?) t) t
             (keyword? t) `'~t
             :else (qualify-typed-symbol 'Any)))
    :union (if (empty? (:types m))
             (qualify-typed-symbol 'Nothing)
             (list* (qualify-typed-symbol 'U) (set (map unparse-type (:types m)))))
    :HVec `'~(mapv unparse-type (:vec m))
    :HMap `'~(into {}
                   (map (fn [[k v]]
                          [k (unparse-type v)]))
                   (:map m))
    :IFn (let [as (map
                    (fn [{:keys [dom rng]}]
                      {:pre [dom rng]}
                      (conj (mapv unparse-type dom)
                            :->
                            (unparse-type rng)))
                    (:arities m))]
           (if (== 1 (count as))
             (first as)
             (list* (qualify-typed-symbol 'IFn) as)))
    :class (letfn [(unparse-class [^Class c args]
                     {:pre [(class? c)]}
                     (cond
                       (.isArray c) (list 'Array (unparse-class (.getComponentType c) args))
                       :else
                       (let [cls (condp = c
                                   clojure.lang.IPersistentMap (qualify-typed-symbol 'Map)
                                   clojure.lang.IPersistentVector (qualify-typed-symbol 'Vec)
                                   clojure.lang.IPersistentSet (qualify-typed-symbol 'Set)
                                   clojure.lang.Symbol (qualify-typed-symbol 'Sym)
                                   clojure.lang.Keyword (qualify-typed-symbol 'Kw)
                                   clojure.lang.IAtom (qualify-typed-symbol 'Atom1)
                                   clojure.lang.ISeq (qualify-typed-symbol 'Seqable)
                                   clojure.lang.IPersistentList (qualify-typed-symbol 'Seqable)
                                   clojure.lang.IFn 'AnyFunction
                                   java.lang.String (qualify-typed-symbol 'Str)
                                   (resolve-class c))
                             _ (assert (symbol? cls))]
                         (if (seq args)
                           (list* cls (map unparse-type args))
                           cls))))]
             (unparse-class (:class m) (:args m)))
    :Top (qualify-typed-symbol 'Any)
    :unknown '?
    :free (:name m)
    :poly (list 'All (into (mapv (fn [[ps {:keys [weight name types]}]]
                                   {:pre [(= 2 (count ps))]}
                                   [name 
                                    types
                                    weight :of 
                                    [(get-in @results-atom [:path-occ (first ps)] 0)
                                     (get-in @results-atom [:path-occ (second ps)] 0)]
                                    '<- (mapv unparse-path ps)])
                                 (:params m))
                           (:known-params m))
                (unparse-type (:type m)))
    (assert nil (str "No unparse-type case: " m))))

(defn unp [t]
  (unparse-type t))

(def ^:dynamic unparse-type unparse-type')

(defn flatten-union [t]
  {:pre [(type? t)]
   :post [(set? %)]}
  (if (#{:union} (:op t))
    (into #{}
          (mapcat flatten-union)
          (:types t))
    #{t}))

(defn flatten-unions [ts]
  {:pre [(every? type? ts)]
   :post [(set? %)]}
  (into #{} 
        (mapcat flatten-union)
        ts))

(declare join-HMaps join*)

(defn make-Union [args]
  (let [ts (flatten-unions args)
        {hmaps true non-hmaps false} (group-by (comp boolean #{:HMap} :op) ts)
        hmap-by-keys (group-by (comp set keys :map) hmaps)
        hmaps-merged (into #{}
                           (map (fn [ms]
                                  {:post [(HMap? %)]}
                                  (reduce join-HMaps (first ms) (rest ms))))
                           (vals hmap-by-keys))
        ;_ (prn "hmaps-merged" (map unparse-type hmaps-merged))
        ts (into hmaps-merged non-hmaps)]
    (assert (every? (complement #{:union}) (map :op ts)))
    (cond
      (= 1 (count ts)) (first ts)
      :else 
      {:op :union
       :types ts})))

(declare join)

(defn group-arities [t1 t2]
  {:pre [(#{:IFn} (:op t1))
         (#{:IFn} (:op t2))]}
  (vals
    (group-by (comp count :dom)
              (concat (:arities t1)
                      (:arities t2)))))

; should-join-HMaps? : HMap HMap -> Bool
(defn should-join-HMaps? [t1 t2]
  {:pre [(HMap? t1)
         (HMap? t2)]}
  ;; join if the required keys are the same, 
  ;; TODO and if 75% of the keys are the same
  ;; TODO and if common keys are not always different keywords
  (let [t1-map (:map t1)
        t2-map (:map t2)]
    (and (= (set (keys t1-map))
            (set (keys t2-map)))
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
           t1-map))))


(defn join-HMaps [t1 t2]
  {:pre [(HMap? t1)
         (HMap? t2)
         (should-join-HMaps? t1 t2)]
   :post [(HMap? %)]}
  (let [t2-map (:map t2)]
    ;(prn "join HMaps")
    {:op :HMap
     :map (into {}
                (map (fn [[k1 t1]]
                       (let [left t1
                             right (get t2-map k1)]
                         ;(prn "key" k1)
                         ;(prn "left" (unparse-type left))
                         ;(prn "right" (unparse-type right))
                         [k1 (join left right)])))
                (:map t1))}))

; join : Type Type -> Type
(defn join [t1 t2]
  {:pre [(type? t1)
         (type? t2)]
   :post [(type? %)]}
  (let [id (gensym (apply str (map :op [t1 t2])))
        ;_ (prn "join" id (unparse-type t1) (unparse-type t2))
        res (cond
              (= t1 t2) t1

              ;; annihilate unknown
              (#{:unknown} (:op t1)) t2
              (#{:unknown} (:op t2)) t1

              ((some-fn #{:union})
               (:op t1)
               (:op t2))
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
                   (= (:class t1)
                      (:class t2))
                   (= (count (:args t1))
                      (count (:args t2))))
              (-class (:class t1) (mapv join (:args t1) (:args t2)))

              (and (#{:class} (:op t1))
                   (= clojure.lang.IFn
                      (:class t1))
                   (#{:IFn} (:op t2)))
              t2

              (and (#{:class} (:op t2))
                   (= clojure.lang.IFn
                      (:class t2))
                   (#{:IFn} (:op t1)))
              t1

              (and (#{:HMap} (:op t1))
                   (#{:HMap} (:op t2))
                   (should-join-HMaps? t1 t2))
              (join-HMaps t1 t2)

              (and (#{:IFn} (:op t1))
                   (#{:IFn} (:op t2)))
              (let [;_ (apply prn "join IFn" (map unparse-type [t1 t2]))
                    grouped (group-arities t1 t2)
                    ;_ (prn "grouped" grouped)
                    arities
                    (mapv
                      ;; each `as` is a list of :IFn1 nodes
                      ;; with the same arity
                      (fn [as]
                        {:pre [(every? #{[:IFn1 (-> as first :dom count)]}
                                       (map (juxt :op (comp count :dom))
                                            as))]
                         :post [(#{:IFn1} (:op %))]}
                        {:op :IFn1
                         :dom (apply mapv
                                     (fn [& [dom & doms]]
                                       {:pre [dom]}
                                       ;(prn "join IFn IFn dom" (map :op (cons dom doms)))
                                       (apply join* dom doms))
                                     (map :dom as))
                         :rng (let [[rng & rngs] (map :rng as)]
                                (assert rng)
                                (apply join* rng rngs))})
                      grouped)]
                {:op :IFn
                 :arities arities})

              :else 
              (let []
                ;(prn "join union fall through")
                (make-Union [t1 t2])))]
    ;(prn "join result" id (unparse-type res))
    res))

(defn join* [& args]
  (letfn [(merge-type [t as]
            {:pre [(type? t)
                   (not= :union (:op t))
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


(declare update-path)

(defn update-var-down-paths [envs ps new-var]
  {:pre [(map? envs)]
   :post [(map? %)]}
  #_
  (doseq [p ps]
    (update-path p new-var))
  envs)

(defn update-equiv [env ps tpe]
  {:pre [(map? env)
         (set? ps)
         (= 2 (count ps))
         (every? vector? ps)
         (every? (comp #{:var} :op first) ps)
         ; vars must be the same
         (apply = (map (comp :name :op first) ps))
         (keyword? tpe)]
   :post [(map? %)]}
  ;(prn "update-equiv")
  ;(pprint (mapv unparse-path ps))
  (let [nme (-> ps first first :name)
        _ (assert (symbol? nme))
        t (get (type-env env) nme)
        _ (assert (type? t) (pr-str nme))]
    (case (:op t)
      :poly (let [; find existing path that overlaps with any
                  ; of the current paths
                  vs (remove (fn [[s _]]
                               (assert (set? s) s)
                               (empty? 
                                 (set/intersection s ps)))
                             (:params t))]
              (case (count vs)
                ;; no matches
                0 (let [new-sym (gensym "var")
                        new-var (make-free new-sym)
                        env ;(prn "Found no matching poly, extending with " new-sym)
                        ;; first add the variable to the parameter list
                        (-> env
                            (update-type-env assoc-in [nme :params ps] 
                                             ; weight of 1
                                             {:weight 1 
                                              :name new-sym
                                              :types #{tpe}})
                            ;; then update the variable down each path
                            (update-var-down-paths ps new-var))]
                    env)
                ;; process results
                (reduce 
                  (fn [env [vps {vsym :name}]]
                    (let [var (make-free vsym)
                          env 
                          (-> env
                              (update-type-env (fn [m]
                                                 (-> m
                                                     (update-in [nme :params vps :weight] 
                                                                (fn [i]
                                                                  ;(prn "adding i" i)
                                                                  (inc i)))
                                                     (update-in [nme :params vps :types] conj tpe))))
                              ;(prn "Found many matching poly, " vsym)
                              ;; update var down each path
                              (update-var-down-paths ps var))]
                      env))
                  env
                  vs))
      (let [new-sym (gensym "var")
            new-var (make-free new-sym)
            env (->
                  ;(prn "No polymorphic type found, creating new poly with " new-sym)
                  ;; first construct a poly type that wraps the original type
                  (update-type-env assoc nme
                                   {:op :poly
                                    :params {ps {:weight 1 
                                                 :name new-sym
                                                 :types #{tpe}}} ; weight of 1
                                    :type t})
                  ;; then update the var down each path
                  (update-var-down-paths ps new-var))]
        env)))))

; update-path : Env Config Path Type -> AliasTypeEnv
(defn update-path [env config path type]
  {:pre [(map? env)
         (vector? path)]}
  (cond 
    (empty? path) (throw (Exception. "Cannot update empty path"))
    (= 1 (count path)) (let [x (nth path 0)
                             n (:name x)
                             t (if-let [t (get (type-env env) n)]
                                 (do
                                   #_(prn "update-path join"
                                          (map :op [t type]))
                                   (join t type))
                                 type)]
                         (assert (#{:var} (:op x))
                                 (str "First element of path must be a variable " x))
                         (update-type-env-in-ns env (or (:ns x) (current-ns))
                                                assoc n t))
    :else 
    (let [last-pos (dec (count path))
          cur-pth (nth path last-pos)
          nxt-pth (subvec path 0 last-pos)]
      (assert (:op cur-pth) (str "What is this? " cur-pth
                                 " full path: " path))
      (case (:op cur-pth)
        :var (throw (Exception. "Var path element must only be first path element"))
        :key (let [{:keys [keys key]} cur-pth]
               (recur env config
                      nxt-pth
                      {:op :HMap
                       :map (assoc (zipmap keys (repeat {:op :unknown}))
                                   key type)}))
        :set-entry (recur env config nxt-pth (-class clojure.lang.IPersistentSet [type]))
        :seq-entry (recur env config nxt-pth (-class clojure.lang.ISeq [type]))
        :transient-vector-entry (recur env config nxt-pth (-class clojure.lang.ITransientVector [type]))
        :atom-contents (recur env config nxt-pth (-class clojure.lang.IAtom [type]))
        :index (recur env config nxt-pth (-class clojure.lang.IPersistentVector [type]))
        :fn-domain (let [{:keys [arity position]} cur-pth]
                     (recur env config nxt-pth
                            {:op :IFn
                             :arities [{:op :IFn1
                                        :dom (assoc (into [] 
                                                          (repeat (:arity cur-pth) {:op :unknown}))
                                                    position type)
                                        :rng {:op :unknown}}]}))
        :fn-range (let [{:keys [arity]} cur-pth]
                    (recur env config nxt-pth
                           {:op :IFn
                            :arities [{:op :IFn1
                                       :dom (into [] (repeat (:arity cur-pth) {:op :unknown}))
                                       :rng type}]}))))))

(defn walk-type-children [v f]
  {:pre [(type? v)]
   :post [(type? %)]}
  (case (:op v)
    (:val :alias :unknown :Top) v
    :HMap (update v :map (fn [m]
                           (reduce-kv
                             (fn [m k v]
                               (assoc m k (f v)))
                             {}
                             m)))
    :class (update v :args (fn [m]
                             (reduce-kv
                               (fn [m k v]
                                 (assoc m k (f v)))
                               []
                               m)))
    :HVec (update v :vec (fn [m]
                           (reduce-kv
                             (fn [m k v]
                               (assoc m k (f v)))
                             []
                             m)))
    :union (apply join* (into #{}
                              (map f)
                              (:types v)))
    :IFn (update v :arities
                 (fn [as]
                   (mapv f as)))
    :IFn1 (-> v
              (update :dom
                      (fn [ds]
                        (mapv f ds)))
              (update :rng f))))

; tools.analyzer
(defn walk
  "Walk the ast applying `pre` when entering the nodes, and `post` when exiting.
   Both functions must return a valid node since the returned value will replace
   the node in the AST which was given as input to the function.
   Short-circuits on reduced."
  ([ast pre post]
     (unreduced
      ((fn walk [ast pre post]
         (let [walk #(walk % pre post)]
           (if (reduced? ast)
             ast
             (let [ret (walk-type-children (pre ast) walk)]
               (if (reduced? ret)
                 ret
                 (post ret))))))
       ast pre post))))

; tools.analyzer
(defn prewalk
  "Shorthand for (walk ast f identity)"
  [ast f]
  (walk ast f identity))

; tools.analyzer
(defn postwalk
  "Shorthand for (walk ast identity f reversed?)"
  ([ast f] (walk ast identity f)))

(defn register-alias [env config name t]
  {:pre [(map? env)
         (symbol? name)
         (type? t)]
   :post [(map? %)]}
  ;(prn "register" name)
  (update-alias-env env assoc name t))

(defn register-unique-alias [env config sym t]
  (let [sym (if (contains? (alias-env env) sym)
              (gensym (str sym "__"))
              sym)]
    [sym (register-alias env config sym t)]))

(defn resolve-alias [env {:keys [name] :as a}]
  {:pre [(map? env)
         (alias? a)
         (symbol? name)
         (type? a)]
   :post [(type? %)]}
  ;(prn "resolve-alias" name (keys (alias-env env)))
  (get (alias-env env) name))

(def val? (comp #{:val} :op))

(def kw-val? (every-pred val?  (comp keyword? :val)))

#_ 
(ann likely-HMap-dispatch [HMap -> (U nil '[Kw (Set Kw)])])
(defn likely-HMap-dispatch
  "Given a HMap type, returns a vector tuple
  of the best guess for the dispatch entry for this HMap.
  The first entry contains the keyword key, and the second
  contains a set of keys that dispatch to this type.
  
  eg. (likely-HMap-dispatch (prs '{:op ':val})) 
      ;=> [:op #{:val}]

  eg. (likely-HMap-dispatch (prs '{:T (U ':intersection :union)}))
      ;=> [:T #{:intersection :union}]
  "
  [t]
  {:pre [(HMap? t)]
   :post [((some-fn nil? vector?) %)]}
  (let [singles (filter (comp (some-fn
                                ; either a literal keyword
                                kw-val?
                                ; or a union of literal keywords
                                (every-pred (comp #{:union} :op)
                                            #(every? kw-val? (:types %))))
                              val)
                        (:map t))]
    (when-let [[k t] (and (= (count singles) 1)
                          (first singles))]
      [k (case (:op t)
           :val #{(:val t)}
           :union (into #{}
                        (comp
                          (filter val?)
                          (map :val))
                        (:types t)))])))

(defn alias-single-HMaps
  "Traverse the type and alias environments
  and ensure all HMaps are aliased"
  [env config]
  (letfn [(do-alias [env-atom t]
            (let [n (symbol (-> (current-ns) str) "alias")
                  a-atom (atom nil)
                  _ (swap! env-atom 
                           (fn [env]
                             (let [[a env] (register-unique-alias env config n t)]
                               ;; don't care if retried
                               (reset! a-atom a)
                               env)))
                  a @a-atom
                  _ (assert a)]
              (-alias a)))
          (single-HMap [env t]
            {:pre [(type? t)]}
            (let [env-atom (atom env)]
              [(postwalk t
                         (fn [t]
                           (case (:op t)
                             :HMap (do-alias env-atom t)
                             t)))
               @env-atom]))]
    (let [env (reduce
                (fn [env [v t]]
                  (let [[t env] (single-HMap env t)]
                    (update-type-env env assoc v t)))
                env
                (type-env env))
          env (reduce
                (fn [env [a t]]
                  (let [[t env] (if (HMap? t)
                                  ;; don't re-alias top-level HMap
                                  (let [env-atm (atom env)]
                                    [(walk-type-children
                                       t
                                       (fn [t]
                                         (let [t-atom (atom nil)
                                               _ (swap! env-atm
                                                        (fn [env]
                                                          (let [[t env] (single-HMap env t)]
                                                            ;; don't care if this is retried
                                                            (reset! t-atom t)
                                                            env)))
                                               t @t-atom]
                                           (type? t)
                                           t)))
                                     @env-atm])
                                  (single-HMap env t))]
                    (update-alias-env env assoc a t)))
                env
                (alias-env env))]
      env)))

(defn alias-hmap-type
  "Recur up from the leaves of a type and
  replace HMaps and unions with fresh type
  aliases. Also registers these type aliases
  in alias-env.
  
  Does not traverse existing type aliases."
  [env' config t]
  (let [env-atom (atom env')]
    (letfn [(do-alias [t]
              (let [t (case (:op t)
                        :union
                        ;; we want every level of types to be an alias,
                        ;; since all members of a union are at the same
                        ;; level, call them the same thing.
                        (make-Union
                          (map (fn [t]
                                 (if (alias? t)
                                   (resolve-alias @env-atom t)
                                   t))
                               (:types t)))
                        t)
                    n (symbol (-> (current-ns) str) "alias")
                    a-atom (atom nil)
                    _ (swap! env-atom 
                             (fn [env]
                               (let [[a env] (register-unique-alias env config n t)]
                                 (reset! a-atom a)
                                 env)))
                    a @a-atom
                    _ (assert a)]
                (-alias a)))]
      [(postwalk t
         (fn [t]
           (case (:op t)
             :HMap (do-alias t)
             :union (if (and (seq (:types t))
                             (not-every?
                               (fn [t]
                                 (case (:op t)
                                   :val true
                                   :class (empty? (:args t))
                                   false))
                               (:types t)))
                      (do-alias t)
                      t)
             :IFn1 (-> t
                       (update :dom #(mapv do-alias %))
                       (update :rng do-alias))
             t)))
       @env-atom])))

(declare fv)

; try-merge-aliases : Env Config Sym Alias -> Env
(defn try-merge-aliases [env config f t]
  {:pre [(map? env)
         (symbol? f)
         (alias? t)]
   :post [(map? env)]}
  #_
  (prn "Try merging" f
       "with" (:name t))
  (let [tks (keysets env t)
        fks (keysets env (-alias f))]
    (cond
      ;; if there's some subset of keysets that are
      ;; identical in both, collapse the entire thing.
      ;; TODO is this too aggresssive? Shouldn't the keysets
      ;; be exactly indentical?
      (and (seq (set/intersection tks fks))
           (not (alias? (resolve-alias env (-alias f))))
           (not (alias? (resolve-alias env t))))
      (let [;_ (prn "Merging" f
            ;       "with" (:name t))
            ]
        (update-alias-env env
                          (fn [m]
                            (-> m 
                                (assoc f t)
                                (update (:name t)
                                        (fn [oldt]
                                          {:pre [(type? oldt)]}
                                          (join
                                            (get m f)
                                            (subst-alias oldt (-alias f) t))))))))

      :else env)))

; squash : Env Config Alias -> Env
(defn squash
  "Recur down an alias and
  merge types based on their keysets.
  Also merge back up if possible."
  [env config t]
  {:pre [(map? env)
         (map? config)
         (alias? t)]
   :post [(map? %)]}
  ;(prn "top squash aliases" (keys (alias-env env))
  ;     #_(fv env t true))
  ;; config is constant
  (loop [env env
         worklist [t]
         ;; aliases we're done with
         done #{}]
    (assert (vector? worklist))
    (assert (set? done))
    ;(prn "worklist" (mapv unp worklist))
    ;(prn "done" done)
    (if (empty? worklist)
      env
      (let [t (nth worklist 0)
            _ (assert (alias? t) [t (class t)])
            ;_ (prn "squash" (unp t))
            fvs (fv env (resolve-alias env t))
            ;; find all keysets for downstream (and upstream) aliases
            ;; and merge.
            env (if-not (done t)
                  (reduce 
                    (fn [env f]
                      (try-merge-aliases env config f t))
                    env
                    (concat
                      fvs
                      ;; also try and merge with parents
                      (map :name (disj done t))))
                  env)]
        (recur env
               (into (subvec worklist 1)
                     (set/difference
                       (into #{}
                             (map -alias)
                             (fv env (resolve-alias env t)))
                       done
                       #{t}))
               (conj done t))))))

; simple-alias? : Env Config Alias -> Bool
(defn simple-alias? [env config a]
  (let [a-res (resolve-alias env a)
        res (not (contains? (set (fv env a-res true)) (:name a)))]
    ;(prn "simple-alias?" (:name a) res)
    res))

; follow-aliases-in-type : Env Config Type -> Type
(defn follow-aliases-in-type [env config t]
  (reduce
    (fn [t f]
      ;(prn "Follow" f)
      (loop [real (-alias f)
             seen #{}]
        (let [real-res (resolve-alias env real)]
          ;(prn "real" real)
          ;(prn "seen" seen)
          ;(prn "real-res" (unp real-res))
          (assert (alias? real))
          (cond
            ;; infinite loop, give up
            (seen real) t

            (alias? real-res)
            (recur real-res (conj seen real))

            :else (subst-alias t (-alias f) 
                               ;; if the new alias is simple, just inline.
                               (if (and (:simplify? config)
                                        (simple-alias? env config real))
                                 real-res
                                 real))))))
    t
    (fv env t)))

; follow-aliases-in-alias-env : Env Config -> Env
(defn follow-aliases-in-alias-env [env config]
  (reduce 
    (fn [env f]
      ;; start at root f and rename
      ;; each alias in f to the non-redundant
      ;; alias.
      (reduce 
        (fn [env inner]
          (loop [real (-alias inner)
                 seen #{}]
            (let [real-res (resolve-alias env real)]
              ;(prn "following" inner)
              ;(prn "real" real)
              ;(prn "res real" (resolve-alias env real))
              (assert (alias? real))
              (cond
                ;; infinite loop, give up
                (seen real) env

                (alias? real-res)
                (recur real-res (conj seen real))

                :else (register-alias env config
                                      f
                                      (subst-alias
                                        (resolve-alias env (-alias f))
                                        (-alias inner)
                                        ;; if the new alias is simple, just inline.
                                        (if (and (:simplify? config)
                                                 (simple-alias? env config real))
                                          real-res
                                          real)))))))
        env
        (fv env (resolve-alias env (-alias f)))))
    env
    (keys (alias-env env))))

; follow-aliases : Env Config Type -> '[Type Env]
(defn follow-aliases
  "Rename aliases to avoid redundant paths.
  Also delete unnecessary aliases for simple types.
  Also inline aliases if they are simple enough."
  [env config t]
  {:pre [(type? t)]
   :post [(let [[t env] %]
            (and (type? t)
                 (map? env)))]}
  ;; rename aliases directly in type we are
  ;; returning. Never changes env.
  (let [t (follow-aliases-in-type env config t)
        ;; follow downstream aliases
        ;_ (prn "Follow downstream aliases")
        env (follow-aliases-in-alias-env env config)]
    ;(prn "Finish follow-aliases")
    [t env]))

; follow-all : Env Config -> Env
(defn follow-all
  "Squash all aliases referenced by a type environment."
  [env config]
  (let [env (reduce (fn [env [v t]]
                      ;(prn "Follow" v)
                      (let [t (follow-aliases-in-type env config t)]
                        ;(prn "Now update type env")
                        (update-type-env env assoc v t)))
                    env
                    (type-env env))
        env (follow-aliases-in-alias-env env config)]
    env))

; squash-all : Env Config Type -> '[Type Env]
(defn squash-all 
  "Make recursive types when possible."
  [env config t]
  {:pre [(map? env)]
   :post [(let [[t env] %]
            (and (type? t)
                 (map? env)))]}
  ;(prn "squash-all start" (unparse-type t))
  (let [fvs (set (fv env t))
        ;_ (prn "free aliases" (unp t) fvs
        ;       (keys (alias-env env)))
        env (reduce (fn [env a]
                      (squash env config a))
                    env (map -alias fvs))]
    [t env]))

(defn add-tmp-aliases [env as]
  (update-alias-env env merge (zipmap as (repeat nil))))

(declare generate-tenv)

; ppenv : Env -> nil
(defn ppenv [env]
  (pprint (into {}
                (map (fn [[k v]]
                       [k (unparse-type v)]))
                env)))

; type-of : Any -> Kw
(defn type-of [v]
  (cond
    (nil? v) :nil
    (keyword? v) :keyword
    (symbol? v) :symbol
    (string? v) :string
    (instance? Boolean v) :bool
    (coll? v) :coll
    (number? v) :number
    (fn? v) :fn
    (instance? clojure.lang.ITransientCollection v) :transient
    :else :other))

; track : (Atom InferResultEnv) Value Path -> Value
(defn track 
  ([results-atom v path]
   {:pre [(vector? path)]}
   (let []
     (cond
       ;; only accurate up to 20 arguments.
       ;; all arities 21 and over will collapse into one.
       (fn? v) (let [;; if this is never called, remember it is actually a function
                     ir (infer-result path (-class clojure.lang.IFn []))
                     _ (add-infer-result! results-atom ir)]
                 (with-meta
                   (fn [& args]
                     (let [limit 20
                           blen (impl/bounded-length args limit) ;; apply only realises 20 places
                           args (map-indexed
                                  (fn [n v]
                                    (if (< n blen)
                                      (track results-atom v (conj path (fn-dom-path blen n)))
                                      v))
                                  args)]
                       (track results-atom (apply v args) (conj path (fn-rng-path blen)))))
                   (meta v)))

       (list? v)
       (let []
         (when (empty? v)
           (add-infer-result!
             results-atom
             (infer-result path 
                           (-class clojure.lang.IPersistentList [(make-Union #{})]))))
         (let [res 
               (with-meta
                 (apply list
                        (map (fn [e]
                               (track results-atom e (conj path (seq-entry))))
                             v))
                 (meta v))]
           (assert (list? res))
           res))

       (and (seq? v)
            (not (list? v)))
       (letfn [(wrap-lseq [v could-be-empty?]
                 (lazy-seq
                   (if (empty? v)
                     (let []
                       (when could-be-empty?
                         (add-infer-result!
                           results-atom
                           (infer-result 
                             path 
                             (-class clojure.lang.ISeq [(make-Union #{})]))))
                       v)
                     (cons (track results-atom
                                  (first v)
                                  (conj path (seq-entry)))
                           (wrap-lseq (rest v)
                                      false)))))]
         (with-meta
           (wrap-lseq v true)
           (meta v)))

       (instance? clojure.lang.ITransientVector v)
       (let [cnt (count v)]
         (reduce
           (fn [v i]
             (let [e (nth v i)
                   e' (track results-atom e
                             (conj path (transient-vector-entry)))]
               (if (identical? e e')
                 v
                 (assoc! v i e'))))
           v
           (range cnt)))

       (and (vector? v) 
            (satisfies? clojure.core.protocols/IKVReduce v)) ; MapEntry's are not IKVReduce
       (let [len (count v)]
         (when (= 0 len)
           (add-infer-result! results-atom (infer-result path (-class clojure.lang.IPersistentVector [{:op :union :types #{}}]))))
         (reduce-kv
           (fn [e k v]
             (let [v' (track results-atom v (conj path (index-path len k)))]
               (if (identical? v v')
                 e
                 (assoc e k v'))))
           v
           v))

       (set? v)
       (do
         (when (empty? v)
           (add-infer-result!
             results-atom
             (infer-result path
                           (-class clojure.lang.IPersistentSet
                                   [{:op :union :types #{}}]))))
         ;; preserve sorted sets
         (into (empty v)
               (map (fn [e]
                      (track results-atom e (conj path (set-entry)))))
               v))

       ;; maps with keyword keys
       (and (or (instance? clojure.lang.PersistentHashMap v)
                (instance? clojure.lang.PersistentArrayMap v))
            (every? keyword? (keys v)))
       (let [ks (set (keys v))]
         (when (empty? v)
           (add-infer-result!
             results-atom
             (infer-result path
                           (-class clojure.lang.IPersistentMap
                                   [{:op :union :types #{}}
                                    {:op :union :types #{}}]))))
         (reduce
           (fn [m k]
             (let [orig-v (get m k)
                   v (track results-atom orig-v
                            (conj path (key-path ks k)))]
               ;; only assoc if needed
               (if (identical? v orig-v)
                 m
                 (assoc m k v))))
           v
           ks))

        (instance? clojure.lang.IAtom v)
        (let [new-path (conj path (atom-contents))
              _ (track results-atom @v new-path)
              _ (add-watch
                  v
                  new-path
                  (fn [_ _ _ new]
                    (future
                      (track results-atom new new-path))))]
          v)

       ((some-fn keyword? nil? false?) v)
       (do
         (add-infer-result! results-atom (infer-result path (-val v)))
         v)

       :else (do
               (add-infer-result! results-atom (infer-result path (-class (class v) [])))
               v)))))

; track-var : (IFn [Var -> Value] [(Atom Result) Var Sym -> Value])
(defn track-var'
  ([vr] (track-var' results-atom vr *ns*))
  ([results-atom vr ns]
   {:pre [(var? vr)
          (instance? clojure.lang.IAtom results-atom)]}
   ;(prn "tracking" vr "in ns" ns)
   (track results-atom @vr [(var-path
                              (ns-name ns)
                              (impl/var->symbol vr))])))

(defmacro track-var [v]
  `(track-var' (var ~v)))

; track-def-init : Sym Sym Value -> Value
(defn track-def-init [vsym ns val]
  {:pre [(symbol? vsym)
         (namespace vsym)]}
  (track results-atom val [{:op :var
                            :ns (ns-name ns)
                            :name vsym}]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Analysis compiler pass
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ns-exclusions : (Set Sym)
(def ns-exclusions
  '#{clojure.core
     clojure.core.typed
     clojure.test
     clojure.string})

; dummy-sym : Env Sym -> TAExpr
(defn dummy-sym [env vsym]
  {:op :const
   :type :symbol
   :form `'~vsym
   :env env
   :val vsym})

; wrap-var-deref : TAExpr Sym Namespace -> TAExpr
(defn wrap-var-deref [expr vsym *ns*]
  (do
    (println (str "Instrumenting " vsym " in " (ns-name *ns*) 
                  #_":" 
                  #_(-> expr :env :line)
                  #_(when-let [col (-> expr :env :column)]
                      ":" col)))
    {:op :invoke 
     :children [:fn :args]
     :form `(track-var' (var ~vsym))
     :env (:env expr)
     :fn {:op :var
          :var #'track-var'
          :form `track-var'
          :env (:env expr)}
     :args [{:op :var
             :form `results-atom
             :env (:env expr)
             :var #'results-atom}
            {:op :the-var
             :form `(var ~vsym)
             :env (:env expr)
             :var (:var expr)}
            (dummy-sym (:env expr) *ns*)]}))

; wrap-var-deref : TAExpr Sym Namespace -> TAExpr
(defn wrap-def-init [expr vsym *ns*]
  ;(prn ((juxt identity class) (-> expr :env :ns)))
  (do
    #_
    (println (str "Instrumenting def init " vsym " in " (ns-name *ns*) 
                  #_":" 
                  #_(-> expr :env :line)
                  #_(when-let [col (-> expr :env :column)]
                      ":" col)))
    {:op :invoke
     :children [:fn :args]
     :form `(track-def-init '~vsym ~(:form expr))
     :env (:env expr)
     :fn {:op :var
          :var #'track-def-init
          :form `track-def-init
          :env (:env expr)}
     :args [(dummy-sym (:env expr) vsym)
            (dummy-sym (:env expr) (:ns (:env expr)))
            expr]}))

; check : (IFn [TAExpr -> TAExpr] [TAExpr CTType -> TAExpr]
(defn check
  "Assumes collect-expr is already called on this AST."
  ([expr] (check expr nil))
  ([expr expected]
   (letfn []
     (case (:op expr)
       ;; Wrap def's so we can instrument their usages outside this
       ;; namespace.
       :def (if (:init expr)
              (update expr :init 
                      (fn [init]
                        (-> init
                            check
                            (wrap-def-init (ast/def-var-name expr) *ns*))))
              expr)
       ;; Only wrap library imports so we can infer how they are used.
       :var (let [vsym (impl/var->symbol (:var expr))
                  vns (symbol (namespace vsym))]
              ;(prn "var" vsym)
              (if-not (contains? (conj ns-exclusions (ns-name *ns*)) vns)
                (wrap-var-deref expr vsym *ns*)
                expr))
       (ast/walk-children check expr)))))

(def runtime-infer-expr check)

; generate : InferResultEnv -> TypeEnv
(defn generate [is]
  (let [env (update-type-env (init-env) generate-tenv is)]
    (pprint
      (into {}
            (map (fn [[name t]]
                   ;(prn "generate" t)
                   [name (unparse-type t)]))
            (type-env env)))))

; generate-tenv : InferResultEnv -> AliasTypeEnv
(defn generate-tenv
  "Reset and populate global type environment."
  [env config {:keys [infer-results equivs] :as is}]
  (as-> (init-env) env 
    (reduce 
      (fn [env i] 
        (let [{:keys [path type]} i] 
          (update-path env config path type)))
      env
      infer-results)
    (reduce 
      (fn [env i]
        (update-equiv env config (:= i) (:type i)))
      env
      equivs)))

(defn gen-current1
  "Print the currently inferred type environment"
  []
  (generate @results-atom))

(defn gen-current2 
  "Turn the currently inferred type environment
  into type aliases. Also print the alias environment."
  []
  (let [env (generate-tenv (init-env) @results-atom)
        _ (assert false)
        env (reduce
              (fn [env [v t]]
                (let [[t' env] (alias-hmap-type env t)]
                  (update-type-env env assoc v t')))
              env
              (type-env env))]
    (ppenv (type-env env))
    (ppenv (alias-env env))))

(declare visualize unmunge
         view-graph
         graph->svg)

(def viz-configs
  {:vfn {:spit (fn [& args]
                 (spit "visualize.svg" (apply graph->svg args)))
         :viz view-graph}
   :options {:small {:dpi 50
                     ;:fixedsize false
                     :fontname "Courier"
                     :vertical? true}
             :projector {:dpi 80
                         ;:fixedsize false
                         :fontname "Courier"
                         :vertical? true}}
   :node->cluster {:unmunge (fn [n]
                              (when-not (namespace n)
                                (unmunge n))
                              #_
                              (let [t (-> n meta :type)]
                                (case (:op t)
                                  :HMap
                                  (let [singles (filter (comp #{:val} :op val) (:map t))]
                                    (when-let [[k v] (and (= (count singles) 1)
                                                          (first singles))]
                                      (str k "-" (pr-str (:val v)))))
                                  nil)))
                   :HMaps
                   (fn [n]
                     (let [t (-> n meta :type)]
                       (case (:op t)
                         :HMap (apply str (interpose "-" (sort (map name (keys (:map t))))))
                         nil)))}
   :cluster->descriptor 
   {:random-color (fn [c]
                    {:color (rand-nth [:red :blue :green :yellow :black])})
    :known-colors (fn [c]
                    {:color 
                     #_(case (first c)
                         \P "#EDA985"
                         \E "#D0E84A"
                         \T :lightblue
                         :black)
                     (rand-nth [;:lightgrey 
                                "#44C7F2"
                                ;:red 
                                "#129AC7"
                                "#126E8C"
                                "#7AD7F5"
                                "#5CA1B8"
                                "#9DBBC4"
                                ;:turquoise 
                                #_:blue 
                                #_:green 
                                ;:yellow
                                ])
                     :style :filled
                     :label c
                     })
    }
   :edge->descriptor {:bold (fn [n1 n2]
                              {:style :bold})}
   :node->descriptor 
   {:just-label 
    (fn [n]
      (let [t (-> n meta :type)]
        {:label (str n ":\n" 
                     (with-out-str 
                       (binding [*unparse-abbrev-alias* true
                                 *unparse-abbrev-class* true]
                         (pprint (unparse-type t)))))}))
    :label-with-keyset
    (fn [n]
      (let [t (-> n meta :type)]
        {:label (str n ":\n" 
                     (keysets @*envs* t)
                     "\n" 
                     (with-out-str 
                       (binding [*unparse-abbrev-alias* true
                                 *unparse-abbrev-class* true]
                         (pprint (unparse-type t)))))}))
    :default
    (fn [n] 
      (let [t (-> n meta :type)]
        {;:color (case (:op t)
         ;         :union :blue
         ;         :HMap :red
         ;         :IFn :yellow
         ;         :black)
         :color (if (namespace n)
                  :green
                  #_(case (:op t)
                      :union :blue
                      :HMap :red
                      :IFn :yellow
                      :black))
         :style (when (namespace n)
                  :filled)

         ;:shape :box
         :label (cond true #_(namespace n) (str n ":\n" 
                                                (with-out-str 
                                                  (binding [*unparse-abbrev-alias* true
                                                            *unparse-abbrev-class* true]
                                                    (pprint (unparse-type t)))))
                      :else (unmunge n))}))}})

(defn relevant-aliases 
  "Returns all referenced aliases from the type
  environment."
  [env]
  {:pre [(map? env)]
   :post [(set? %)
          (every? symbol? %)]}
  (reduce
    (fn [as [v t]]
      (into as (fv env t true)))
    #{}
    (type-env env)))

(defn recursive-alias? [env a]
  {:pre [(symbol? a)]}
  (contains?
    (fv env
        (resolve-alias env (-alias a))
        true)
    a))

(defn squash-horizonally
  "Join aliases that refer to exactly
  one HMap with the same keyset.
  If maps are recursively defined, don't
  merge them.
  
  eg. {a1 '{:a Int}
       a2 '{:a Bool}}
      => 
      {a1 '{:a (U Int Bool)}
       a2 a1}

  eg. {a1 '{:a a2}
       a2 '{:a (U nil a1)}}
      => 
      {a1 '{:a a2}
       a2 '{:a (U nil a1)}}
  "
  [env config]
  (let [as (relevant-aliases env)
        _ (assert (set? as))
        ksets (apply merge-with into
                     (->> (alias-env env)
                          (filter (fn [[k t]]
                                    (and (contains? as k)
                                         (HMap? t))))
                          (map (fn [[a t]]
                                 {:pre [(HMap? t)]}
                                 {(HMap-keyset t) #{a}}))))
        env (reduce
              (fn [env [kset as]]
                (if (< 1 (count as))
                  (let [;_ (prn "Merging" kset as)
                        ;; join all aliases non-recursive aliases
                        as (remove #(recursive-alias? env %) as)
                        ;_ (prn "Removed recursive aliases, merging:" as)
                        atyp (apply join* (map #(resolve-alias env (-alias %)) as))
                        [anew arst] [(first as) (rest as)]
                        ;; rewrite anew to joined type
                        env (update-alias-env env assoc anew atyp)
                        ;; arst all point to a
                        env (reduce (fn [env aold]
                                      (update-alias-env env assoc aold (-alias anew)))
                                    env
                                    arst)]
                    env)
                  env))
              env
              ksets)]
    env))

(defn populate-envs [env config]
  #_(prn "populating")
  (let [;; create recursive types
        env (reduce
              (fn [env [v t]]
                (let [;; create graph nodes from HMap types
                      [t env] (alias-hmap-type env config t)
                      ;; squash local recursive types
                      [t env] (squash-all env config t)
                      ;; trim redundant aliases in local types
                      [t env] (follow-aliases env (assoc config :simplify? true) t)
                      ]
                  (update-type-env env assoc v t)))
              env
              (type-env env))
        ;; ensure all HMaps correspond to an alias
        env (alias-single-HMaps env config)
        ;; merge aliases that point to HMaps
        ;; with the same keys (they must point to *exactly*
        ;; one top-level HMap, not a union etc.)
        env (squash-horizonally env config)
        ;_ (prn "finished squash-horizonally")
        ;; Clean up redundant aliases and inline simple
        ;; type aliases.
        ;_ (prn "Start follow-all")
        env (follow-all env (assoc config :simplify? false))
        ;_ (prn "end follow-all")
        ]
    #_(prn "done populating")
    env))

;(defn order-defaliases [env as]
;  (let [afvs (into {}
;                   (map (fn [[k v]]
;                          [k (set (fv env v))]))
;                   as)]
;    ))

(defn envs-to-annotations [env config]
  (let [tenv (type-env env)
        tfvs (into #{}
                   (mapcat
                     (fn [t]
                       (fv env t true)))
                   (vals tenv))
        as (into {}
                 (filter (comp tfvs key))
                 (alias-env env))]
    (into
      (into [(list* (if (= (ns-resolve (current-ns) 'declare)
                           #'clojure.core/declare)
                      'declare
                      'clojure.core/declare)
                    (map (comp symbol name key) as))]
            (map (fn [[k v]]
                   (list (qualify-typed-symbol 'defalias)
                         (symbol (name k))
                         (unparse-type v)))
                 as))
      (map (fn [[k v]]
             (list (qualify-typed-symbol 'ann)
                   (if (= (namespace k)
                          (str (ns-name (current-ns))))
                     ;; defs
                     (symbol (name k))
                     ;; imports
                     k)
                   (unparse-type v))))
      tenv)))


(defn gen-current3 
  "Turn the currently inferred type environment
  into type aliases. Also print the alias environment."
  []
  (let [ns 'clojure.core.typed.test.mini-occ]
    (binding [*ann-for-ns* (fn [] ns)]
      (let [env (populate-envs (init-env) (init-config))]
        (visualize
          env
          {:current-ns ns
           :top-levels 
           #{;`take-map
             ; `a-b-nested
             ;'clojure.core.typed.test.mini-occ/parse-exp
             'clojure.core.typed.test.mini-occ/parse-prop
             ;`mymapv
             ;`pprint
             }
           :viz-args {:options (-> viz-configs :options :projector)
                      :node->descriptor (-> viz-configs
                                            :node->descriptor
                                            :label-with-keyset)}})))))

(defn fv
  "Returns the aliases referred in this type, in order of
  discovery. If recur? is true, also find aliases
  referred by other aliases found."
  ([env v] (fv env v false #{}))
  ([env v recur?] (fv env v recur? #{}))
  ([env v recur? seen-alias]
   {:pre [(map? env)
          (type? v)]
    :post [(vector? %)]}
   ;(prn "fv" v)
   (let [fv (fn 
              ([v] (fv env v recur? seen-alias))
              ([v recur? seen-alias]
               (fv env v recur? seen-alias)))]
     (case (:op v)
       (:Top :unknown :val) []
       :HMap (into []
                   (mapcat fv)
                   (-> v :map vals))
       :HVec (into []
                   (mapcat fv)
                   (-> v :vec))
       :union (into []
                    (mapcat fv)
                    (-> v :types))
       :class (into []
                    (mapcat fv)
                    (-> v :args))
       :alias (if (seen-alias v)
                []
                (conj
                  (if recur?
                    (fv (resolve-alias env v)
                        recur?
                        (conj seen-alias v))
                    [])
                  (:name v)))
       :IFn (into []
                  (mapcat (fn [f']
                            (into (into [] 
                                        (mapcat fv) 
                                        (:dom f'))
                                  (fv (:rng f')))))
                  (:arities v))))))

(defn unmunge [n]
  (when-let [s (first (partition-by #{\_} (str n)))]
    (apply str s)))

;https://github.com/ToBeReplaced/mapply/blob/master/src/org/tobereplaced/mapply.cl
(defn mapply
  "Applies a function f to the argument list formed by concatenating
  everything but the last element of args with the last element of
  args.  This is useful for applying a function that accepts keyword
  arguments to a map."
  [f & args]
  (apply f (apply concat (butlast args) (last args))))

(defn view-graph [& args]
  (require '[rhizome.viz])
  (apply (impl/v 'rhizome.viz/view-graph) args))

(defn graph->svg [& args]
  (require '[rhizome.viz])
  (apply (impl/v 'rhizome.viz/graph->svg) args))

(defn visualize [env {:keys [top-levels current-ns] 
                      :or {current-ns (#'current-ns)}
                      :as config}]
  (binding [*ann-for-ns* (fn [] current-ns)]
    ;(prn (#'current-ns))
    (let [type-env-edge-map 
          (reduce
            (fn [g [v t]]
              (if (or (= :all top-levels)
                      (contains? top-levels v))
                (let [fvs (fv env t)]
                  ;(prn (unparse-type t))
                  ;(prn fvs)
                  (assoc g v fvs))
                g))
            {}
            (type-env env))

          ;_ (prn "type-env-edge-map" type-env-edge-map)

          alias-env-edge-map
          (loop [g {}
                 wl (select-keys (alias-env env) 
                                 (apply concat (vals type-env-edge-map)))]
            (if (empty? wl)
              g
              (let [[v t] (first wl)
                    ;_ (prn "get fv of" t)
                    fvs (fv env t)]
                (recur (assoc g v fvs)
                       (merge
                         (dissoc wl v)
                         (select-keys (alias-env env)
                                      (set/difference
                                        (set fvs)
                                        (set (keys g))
                                        (set (keys wl))
                                        #{v})))))))

          edge-map (merge-with 
                     (fn [& args]
                       (assert nil "Overlap?"))
                     type-env-edge-map
                     alias-env-edge-map)

          ;_ (prn edge-map)

          nodes (into #{}
                      (map (fn [[v t]]
                             (with-meta v {:type t})))
                      (select-keys (merge (alias-env env)
                                          (type-env env))
                                   (keys edge-map)))]
      (mapply view-graph
              nodes
              edge-map
              (:viz-args config)))))


(defn ppresults []
  (pprint
    (into #{}
          (map (fn [a]
                 (update a :type unparse-type)))
          (get-infer-results results-atom))))


(defn var-constraints 
  "Return the bag of constraints in the current results-atom
  for the given fully qualified var.
  
  eg. (var-constraints 'clojure.core.typed.test.mini-occ/parse-exp)
  "

  [vsym]
  (pprint (mapv unparse-infer-result 
                (-> (->> (get-infer-results results-atom) (group-by (comp :name first :path))) 
                    (get vsym)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting/deleting annotations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; from tools.namespace
(defn update-file
  "Reads file as a string, calls f on the string plus any args, then
  writes out return value of f as the new contents of file. Does not
  modify file if the content is unchanged."
  [file f & args]
  (let [old (slurp file)
        new (str (apply f old args))]
    (when-not (= old new)
      (spit file new))))

(defn ns-file-name [sym]
  (io/resource
    (coerce/ns->file sym)))

(def generate-ann-start ";; Start: Generated by clojure.core.typed - DO NOT EDIT")
(def generate-ann-end ";; End: Generated by clojure.core.typed - DO NOT EDIT")

(defn delete-generated-annotations-in-str 
  "Delete lines between generate-ann-start and generate-ann-end."
  [old]
  {:pre [(string? old)]
   :post [(string? %)]}
  (with-open [rdr (java.io.BufferedReader.
                    (java.io.StringReader. old))]
    (loop [current-open false
           lines (line-seq rdr)
           out []]
      (if (seq lines)
        (if current-open
          (if (= (first lines)
                 generate-ann-end)
            (recur false
                   (next lines)
                   out)
            (recur current-open
                   (next lines)
                   out))
          (if (= (first lines)
                 generate-ann-start)
            (recur true
                   (next lines)
                   out)
            (recur current-open
                   (next lines)
                   (conj out (first lines)))))
        (str/join "\n" out)))))

(defn ns-end-line 
  "Returns the last line of the ns form."
  [s]
  {:pre [(string? s)]
   :post [(integer? %)]}
  (let [ns-form (with-open [pbr (rdrt/indexing-push-back-reader
                                  (rdrt/string-push-back-reader s))]
                  (nprs/read-ns-decl pbr))
        _ (assert ns-form "No namespace form found")
        end-line (-> ns-form meta :end-line)
        _ (assert (integer? end-line) 
                  (str "No end-line found for ns form"
                       (meta ns-form)))]
    end-line))

(defn insert-generated-annotations-in-str
  "Insert annotations after ns form."
  [old ann-str config]
  {:pre [(string? old)
         (string? ann-str)]
   :post [(string? %)]}
  (let [insert-after (ns-end-line old)]
    (with-open [pbr (java.io.BufferedReader.
                      (java.io.StringReader. old))]
      (loop [ls (line-seq pbr)
             current-line 0
             out []]
        (if (= current-line insert-after)
          (str/join "\n" (concat out 
                                 [(first ls)
                                  ""
                                  ann-str]
                                 (rest ls)))
          (if (seq ls)
            (recur (next ls)
                   (inc current-line)
                   (conj out (first ls)))
            (str/join "\n" (concat out 
                                   [""
                                    ann-str]))))))))
    


(defn delete-generated-annotations [ns config]
  (impl/with-clojure-impl
    (update-file (ns-file-name (ns-name ns)) delete-generated-annotations-in-str)))

(declare infer-anns)

(defn prepare-ann [ns config]
  {:post [(string? %)]}
  (let [as (infer-anns ns config)]
    (binding [*print-length* nil
              *print-level* nil]
      (with-out-str
        (println generate-ann-start)
        (doseq [a as]
          (pprint a))
        (println generate-ann-end)))))

(defn insert-generated-annotations [ns config]
  (impl/with-clojure-impl
    (update-file (ns-file-name (ns-name ns))
                 insert-generated-annotations-in-str
                 (prepare-ann ns config)
                 config)))

(defn replace-generated-annotations [ns config]
  (impl/with-clojure-impl
    (update-file (ns-file-name (ns-name ns))
                 (fn [s]
                   (-> s
                       delete-generated-annotations-in-str
                       (insert-generated-annotations-in-str
                         (prepare-ann ns config)
                         config))))))

(defn infer-anns 
  ([ns {:keys [output] :as config}]
   {:pre [(or (instance? clojure.lang.Namespace ns)
              (symbol? ns))]}
   (binding [*ann-for-ns* #(or (some-> ns the-ns) *ns*)]
     (-> (init-env)
         (generate-tenv config @results-atom)
         (populate-envs config)
         (envs-to-annotations config)))))

(defn runtime-infer
  ([{:keys [ns output]}]
   (replace-generated-annotations ns 
                                  (assoc (init-config)
                                         :output output))))

;; TESTS

(comment

  (binding [*ns* (the-ns 'clojure.core.typed.test.mini-occ)]
    (-> (generate-tenv (init-env) (init-config) @results-atom)
        (envs-to-annotations (init-config))
        pprint)
    )

(defmacro defntrack [n & args]
  `(def ~n (track-def-init
             '~(symbol (str (ns-name *ns*)) (str n))
             '~(ns-name *ns*)
             (fn ~@args))))

(defntrack foo 
  [a]
  (+ a 2))

(defntrack bar
  [f]
  (f 1))

(bar foo)
(bar foo)

(defntrack use-map [m]
  (merge m {:b ((:f m) foo)}))

(use-map {:a 1, :f bar})

(use-map {:a 1, :f bar})

(use-map {:f bar})

(use-map {:f bar})

(defntrack multi-arg 
  ([a] (inc a))
  ([s1 s2] (str s1 s2)))

(multi-arg 1)
(multi-arg "a" "a")
(multi-arg "b" "c")
(multi-arg "d" "e")

(defntrack take-map [m]
  {:a m})


(take-map nil)
(take-map {:a nil})
(take-map {:a {:a nil}})
(take-map {:a {:a {:a {:a nil}}}})

(defntrack postfix [& words]
  (reduce (fn [stack t]
            (if (fn? t)
              (let [[l r & m] stack]
                (cons (t r l) m))
              (cons t stack)))
          [] 
          words))

(postfix 1 2)

(defntrack mymapv [f c]
  (mapv f c))

(mymapv inc [1 2 3 4])
(mymapv str '[a b c d])
(mymapv (juxt first second) {'a 'b 'c 'd 'e 'f})

(defntrack iden [x] x)
;(defntrack f [x] (add1 x))

(defntrack invthunk [f]
  (f)
  f)

(defntrack invnested [f g]
  (f g)
  [f g])

(invnested (fn [g] (g))
           (fn [] (+ 1 2)))

#_
((track
   results-atom
   (fn [g] (g))
   [#'invnested (dom 2 0)])
 (track
   results-atom
   (fn [] (+ 1 2))
   [#'invnested (dom 2 0)]))

#_
((let [atm (atom #{})]
   (fn [g]
     (track
       results-atom
       ((fn [g] (g))
        (track results-atom g [#'invnested (dom 2 0) (dom 1 0)]))
       [#'invnested (dom 2 0) (rng 1)])))
 (let [atm (atom #{})]
   (fn []
     (track
       results-atom
       ((fn [] (+ 1 2)))
       [#'invnested (dom 2 1) (rng 0)]))))


; (Rec [f] [f -> f])
; (All [a] [a -> a])
(iden iden)
;(iden 1)
;(iden 'a)
;(iden [1])
;(iden [2 3])
;(iden [5 8])

(invthunk #(+ 2 3))

#_
(fn [f] ; f = #(+ 2 3)
  (let [f (let [app-info (atom {:infer-results #{}
                                :base-path '[#'invthunk]
                                :alias-paths #{}
                                :parent results-atom})]
            (with-meta
              (fn []
                (track
                  app-info
                  (f)
                  '[#'invthunk (dom 1 0) (rng 0)]))
              {:inferred (atom )}))]
    (f) ; [#'invthunk (dom 1 0) (rng 0)] : Long

    (swap! app-info update :equiv-paths conj '[#'invthunk (rng 1)])
    (track
      f   
    ; [#'invthunk (dom 1 0) (rng 0)] = [#'invthunk (rng 1)]
    )))

(defntrack maptrans [f]
  (map f))

(into []
      (maptrans identity)
      [1 2 3])

(into []
      (maptrans identity)
      ['a 'b 'c])

(into []
      (maptrans str)
      ['a 'b 'c])

(defntrack a-b-nested []
  (rand-nth
    [nil
     {:a nil}
     {:a {:b {:a nil}}}
     {:a {:b {:b nil}}}
     {:b {:a nil}}
     {:b {:a {:b {:a nil}}}}]))

(dotimes [_ 100]
  (a-b-nested))

(ppres)

)
