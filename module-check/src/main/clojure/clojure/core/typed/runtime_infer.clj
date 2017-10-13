(ns clojure.core.typed.runtime-infer
  (:refer-clojure :exclude [any?])
  (:require [clojure.pprint :as pp]
            [clojure.core :as core]
            [clojure.core.typed :as t]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.typed.ast-utils :as ast]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.debug :as d :refer [dbg]]
            [clojure.tools.reader.reader-types :as rdrt]
            [clojure.tools.namespace.parse :as nprs]
            [clojure.math.combinatorics :as comb]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.utils :as u]
            [clojure.walk :as walk]
            [clojure.tools.analyzer.passes.jvm.emit-form :as emit-form]
            [clojure.core.typed.dep.potemkin.collections :as pot]
            ))

(def spec-ns (or u/spec-ns 'clojure.spec.alpha))
(def core-specs-ns (or u/core-specs-ns 'clojure.core.specs.alpha))

;; https://github.com/r0man/inflections-clj/blob/master/src/inflections/core.cljc
(defn str-name
  "Same as `clojure.core/name`, but keeps the namespace for keywords
  and symbols."
  [x]
  (cond
    (nil? x)
    x
    (string? x)
    x
    (or (keyword? x)
        (symbol? x))
    (if-let [ns (namespace x)]
      (str ns "/" (name x))
      (name x))))
(defn coerce
  "Coerce the string `s` to the type of `obj`."
  [obj s]
  (cond
    (keyword? obj)
    (keyword s)
    (symbol? obj)
    (symbol s)
    :else s))
(defn camel-case
  "Convert `word` to camel case. By default, camel-case converts to
  UpperCamelCase. If the argument to camel-case is set to :lower then
  camel-case produces lowerCamelCase. The camel-case fn will also
  convert \"/\" to \"::\" which is useful for converting paths to
  namespaces.
  Examples:
    (camel-case \"active_record\")
    ;=> \"ActiveRecord\"
    (camel-case \"active_record\" :lower)
    ;=> \"activeRecord\"
    (camel-case \"active_record/errors\")
    ;=> \"ActiveRecord::Errors\"
    (camel-case \"active_record/errors\" :lower)
    ;=> \"activeRecord::Errors\""
  [word & [mode]]
  (when word
    (->> (let [word (str-name word)]
           (cond
             (= mode :lower) (camel-case word str/lower-case)
             (= mode :upper) (camel-case word str/upper-case)
             (fn? mode) (str (mode (str (first word)))
                             (apply str (rest (camel-case word nil))))
             :else (-> (str/replace word #"/(.?)" #(str "::" (str/upper-case (nth % 1))))
                       (str/replace #"(^|_|-)(.)"
                                #(str (if (#{\_ \-} (nth % 1))
                                        (nth % 1))
                                      (str/upper-case (nth % 2)))))))
         (coerce word))))


(def ^:dynamic *debug* nil)
(def ^:dynamic *debug-depth* 0)

(defmacro debug-flat
  ([msg]
   `(when (= :all *debug*)
      (print (str (apply str (repeat *debug-depth* "  ")) *debug-depth* ": "))
      ~msg)))

(defmacro debug 
  ([msg body]
   `(do
      (debug-flat ~msg)
      (binding [*debug-depth* (when (= :all *debug*)
                                (inc *debug-depth*))]
        ~body))))

#_
(defalias Type
  (U '{:op :val :val Any}
     (HMap
       :mandatory
       {:op :HMap 
        ::HMap-req (Map Kw Type)}
       :optional
       {::HMap-opt (Map Kw Type)})

     '{:op :HVec :vec (Vec Type)}
     '{:op :union :types (Set Type)}
     '{:op :unresolved-class 
       ::class-string String
       :args (Vec Type)}
     '{:op :class 
       ::class-instance Class
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

(def -any {:op :Top})

(def -nothing {:op :union :types #{}})

(declare union?)

(defn nothing? [t]
  (boolean
    (when (union? t)
      (empty? (:types t)))))

(defn Any? [m]
  {:pre [(map? m)]
   :post [(boolean? %)]}
  (= :Top (:op m)))

(defn intersection-or-empty [[& args]]
  (if args
    (apply set/intersection args)
    #{}))

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
       :kw-entries (Map Kw Type)
       :keys (Set Kw)
       :key Kw}
     '{:op :vec-entry}
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

(def list*-force (comp doall list*))

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

(defn infer-results? [m]
  (and (map? m)
       (-> m :infer-results set?)
       (-> m :path-occ map?)
       (-> m :equivs vector?)))

; results-atom : (Atom InferResultEnv)
(def results-atom (atom (initial-results) :validator infer-results?))

(declare pprint get-infer-results unparse-infer-result)

(defn ppresults 
  ([] (ppresults (get-infer-results results-atom)))
  ([infer-results]
   (pprint (mapv unparse-infer-result infer-results))))

(defn swap-infer-results! [results-atom f & args]
  (apply swap! results-atom update :infer-results f args))

(defn add-infer-result! [results-atom r]
  (swap-infer-results! results-atom (fnil conj #{}) r))

(defn add-infer-results! [results-atom r]
  (swap-infer-results! results-atom (fnil into #{}) r))

(defn get-infer-results [results-atom]
  (get @results-atom :infer-results))

(defn infer-result [path type]
  {:op :path-type
   :type type
   :path path})

(defn infer-results [paths type]
  (map #(infer-result % type) paths))

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

(def -unknown {:op :unknown})

(defn unknown? [m]
  (= :unknown
     (:op m)))

;; for zero arity, use (fn-dom-path 0 -1)
(defn fn-dom-path [arity pos]
  (assert (< pos arity)
          (str "Arity: " arity
               "Position:" pos))
  {:op :fn-domain
   :arity arity :position pos})

(defn fn-rng-path [arity]
  {:op :fn-range
   :arity arity})

(defn map-keys-path []
  {:op :map-keys})

(defn map-vals-path []
  {:op :map-vals})

(defn key-path 
  ([keys key] (key-path {} keys key))
  ([kw-entries keys key]
   {:pre [(keyword? key)]}
   {:op :key
    ;; (Map Kw (ValType Kw)) for constant keyword entries
    :kw-entries kw-entries
    :keys keys
    :key key}))

(defn index-path [count nth]
  {:op :index
   :count count
   :nth nth})

(defn vec-entry-path []
  {:op :vec-entry})

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
   ::class-instance cls
   :args args})

(defn -class? [m]
  (boolean (#{:class} (:op m))))

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
  [(unparse-path (:path p)) :- (unparse-type (:type p))])

(defn parse-path-elem [p]
  (case (first p)
    val (-val (second p))
    key (do
          (assert (== 3 (count p))
                  "extra argument not supported to key-path")
          (key-path (second p) (nth p 2)))
    rng (fn-rng-path (second p))
    dom (fn-dom-path (second p) (nth p 2))
    var (var-path (second p))
    index (index-path (second p) (nth p 2))))

(defn unparse-path-elem [p]
  (case (:op p)
    :val (list 'val (:val p))
    :key (list 'key (:kw-entries p) (:keys p) (:key p))
    :fn-range (list 'rng (:arity p))
    :fn-domain (list 'dom (:arity p) (:position p))
    :var (list 'var (:name p))
    :index (list 'index (:count p) (:nth p))
    :vec-entry (list 'vec-entry)
    :set-entry (list 'set-entry)
    :map-keys (list 'map-keys)
    :map-vals (list 'map-vals)
    :seq-entry (list 'seq-entry)
    :transient-vector-entry (list 'transient-vector-entry)
    :atom-contents (list 'atom-contents)))

(defn HMap? [t]
  (= :HMap (:op t)))
(defn HVec? [t]
  (= :HVec (:op t)))

(defn alias? [t]
  (= :alias (:op t)))

(defn union? [t]
  (= :union (:op t)))

(declare parse-type ^:dynamic *new-aliases*)

(defn parse-arity [a]
  (let [[doms [_->_ rng :as rng-arrow]] (split-with (complement #{:->}) a)
        _ (assert (= 2 (count rng-arrow)))]
    {:op :IFn1
     :dom (mapv parse-type doms)
     :rng (parse-type rng)}))

(defn parse-HVec [v]
  {:op :HVec 
   :vec (mapv parse-type v)})

(defn parse-literal-HMap [m]
  {:op :HMap
   ::HMap-req (into {}
                    (map (fn [[k v]]
                           [k (parse-type v)]))
                    m)
   ::HMap-opt {}})

(defn parse-HMap [[_ & {:keys [mandatory optional]}]]
  (let [prs-map (fn [m]
                  (into {}
                        (map (fn [[k v]]
                               [k (parse-type v)]))
                        m))]
    {:op :HMap
     ::HMap-req (prs-map mandatory)
     ::HMap-opt (prs-map optional)}))

(declare make-Union resolve-alias postwalk)

(def ^:dynamic *type-var-scope* #{})

(defn map-key-set [m]
  (set (keys m)))

(defn HMap-req-keyset [t]
  {:pre [(HMap? t)]
   :post [(set? %)
          (every? keyword? %)]}
  (let [m (map-key-set (::HMap-req t))]
    ;(when (not (every? keyword? m))
    ;  (prn "bad HMap-req-keyset" m))
    m))

(defn HMap-req-opt-keysets [t]
  {:pre [(HMap? t)]
   :post [(set? %)]}
  #{{:req-keyset (map-key-set (::HMap-req t))
     :opt-keyset (map-key-set (::HMap-opt t))}})

(declare unp)

; gather-HMap-info : (All [a] [Env Type [HMap -> (Set a)] (Set Type) -> (Set a)])
(defn gather-HMap-info
  ([env t f] (gather-HMap-info env t f #{}))
  ([env t f seen]
   {:pre [(map? env)
          (type? t)
          (set? seen)]
    :post [(set? %)]}
   ;(prn "gather-HMap-info" (unp t))
   (let [gather-HMap-info
         (fn 
           ([t] (gather-HMap-info env t f seen))
           ([t seen] (gather-HMap-info env t f seen)))]
     (case (:op t)
       :HMap (f t)
       :union (into #{}
                    (mapcat gather-HMap-info)
                    (:types t))
       :alias (if (seen t)
                #{}
                (gather-HMap-info 
                  (resolve-alias env t)
                  (conj seen t)))
       #{}))))

(defn keysets [env t]
  (gather-HMap-info env t HMap-req-opt-keysets))

(defn HMap-deep-reqs [env t]
  {:pre [(type? t)]
   :post [(set? %)
          (every? map? %)]}
  (gather-HMap-info env t (fn [m]
                            #{(::HMap-req m)})))

(defn subst-alias [t old new]
  {:pre [(type? t)
         (alias? old)
         (type? new)]
   :post [(type? %)]}
  ;(prn "subst-alias t" (unparse-type t))
  ;(prn "old" (unparse-type old))
  ;(prn "new" (unparse-type new))
  (if (= old new)
    t
    (do 
      ;(apply prn "renaming" (:name old) 
      ;       (when (alias? new)
      ;         [(str "to " (:name new))]))
      (postwalk t
                (fn [c]
                  (case (:op c)
                    :alias (if (= c old)
                             new
                             c)
                    c))))))

(defn rename-alias [env old new]
  {:pre [(symbol? old)
         (symbol? new)]}
  ;(prn "rename-alias" old "->" new)
  (let [tenv (into {}
                   (map (fn [[k t]]
                          [k (subst-alias t (-alias old) (-alias new))]))
                   (type-env env))
        aenv (into {}
                   (map (fn [[a t]]
                          [(if (= old a) new a)
                           (subst-alias t (-alias old) (-alias new))]))
                   (alias-env env))]
    (-> env
        (update-type-env (constantly tenv))
        (update-alias-env (constantly aenv)))))

(defn parse-type [m]
  (cond
    (#{'Any 'clojure.core.typed/Any} m) -any
    (= '? m) {:op :unknown}

    (or (= nil m)
        (= false m)
        (keyword? m)) {:op :val :val m}

    (vector? m) {:op :IFn
                 :arities [(parse-arity m)]}

    (symbol? m) (case m
                  (clojure.core.typed/Nothing Nothing) -nothing
                  (clojure.core.typed/Sym Sym) (-class clojure.lang.Symbol [])
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
                       ::class-instance (resolve m)
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
                          (map? in) (parse-literal-HMap in)
                          (keyword? in) {:op :val :val in}
                          :else (assert nil (str "Bad quote: " m))))

                IFn {:op :IFn
                     :arities (mapv parse-arity (rest m))}
                U (make-Union
                    (into #{}
                          (map parse-type)
                          (rest m)))
                HMap (parse-HMap m)
                Vec (-class clojure.lang.IPersistentVector
                            [(parse-type (second m))])
                (clojure.core.typed/Map) (let [[_ k v] m]
                                               (-class clojure.lang.IPersistentMap
                                                       [(parse-type k)
                                                        (parse-type v)]))
                Set (-class clojure.lang.IPersistentSet
                            [(parse-type (second m))])
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

(defn namespace-alias-in [ns maybe-aliased-ns]
  {:pre [((some-fn nil? #(instance? clojure.lang.Namespace %)) maybe-aliased-ns)]
   :post [((some-fn nil? symbol) %)]}
  (get (set/map-invert (ns-aliases ns)) maybe-aliased-ns))

(def ^:dynamic *verbose-specs* nil)

(defn qualify-symbol-in [nsym s]
  {:pre [(symbol? nsym)
         (symbol? s)
         (not (namespace s))]
   :post [(symbol? %)]}
  (let [ns (find-ns nsym)
        talias (namespace-alias-in (the-ns (current-ns)) ns)
        already-referred? (let [actual (let [v (ns-resolve (the-ns (current-ns)) s)]
                                         (when (var? v)
                                           (coerce/var->symbol v)))
                                desired (symbol (name nsym)
                                                (name s))]
                            ;(prn actual desired)
                            (= actual desired))]
    (symbol (if *verbose-specs*
              (str nsym)
              (when-not already-referred?
                (or (when talias
                      (str talias))
                    (when ns
                      (str (ns-name ns)))
                    (name nsym))))
            (str s))))

(defn qualify-spec-symbol [s]
  {:pre [(symbol? s)]
   :post [(symbol? %)]}
  (qualify-symbol-in spec-ns s))

(defn qualify-typed-symbol [s]
  {:pre [(symbol? s)]
   :post [(symbol? %)]}
  (qualify-symbol-in 'clojure.core.typed s))

(defn qualify-core-symbol [s]
  {:pre [(symbol? s)]
   :post [(symbol? %)]}
  (qualify-symbol-in 'clojure.core s))

(defn resolve-class [^Class c]
  {:pre [(class? c)]
   :post [(symbol? %)]}
  (symbol
    (if (= (ns-resolve (current-ns) (symbol (.getSimpleName c)))
           c)
      (.getSimpleName c)
      (.getName c))))

(def ^:dynamic *unparse-spec* nil)

(defn unparse-spec [m]
  (binding [*unparse-spec* true]
    (unparse-type m)))

(defn kw->sym [k]
  {:pre [(keyword? k)]
   :post [(symbol? %)]}
  (symbol (namespace k)
          (name k)))

(defn alias->spec-kw [s]
  {:pre [(symbol? s)]
   :post [(keyword? %)]}
  (if (namespace s)
    (keyword s)
    (keyword (name (current-ns)) (name s))))

(declare resolve-alias-or-nil)

(def ^:dynamic *spec* false)

(defn simplify-spec-alias [a]
  {:pre [(type? a)]
   :post [(type? %)]}
  (if (alias? a)
    (let [a-res (resolve-alias @*envs* a)]
      (if (and a-res (#{:class} (:op a-res)))
        a-res
        a))
    a))

(defn nil-val? [t]
  (boolean
    (and (#{:val} (:op t))
         (nil? (:val t)))))

(declare uniquify kw-val?)

(defn or-spec [alts]
  {:pre [(every? type? alts)]}
  (let [;; put instance comparisons at the front of the disjunction.
        ;; avoid errors in checking specs that have both records
        ;; and collections, since records do not implement `empty`.
        {inst? true other false} (group-by (fn [t]
                                             (boolean
                                               (or (#{:val} (:op t))
                                                   (and (#{:class} (:op t))
                                                        (not
                                                          (#{clojure.lang.IPersistentSet
                                                             clojure.lang.IPersistentMap
                                                             clojure.lang.IPersistentVector
                                                             clojure.lang.IPersistentCollection
                                                             clojure.lang.ISeq}
                                                            (::class-instance t)))))))
                                           alts)]
    ;(prn "or-spec" alts)
    (cond
      (and (seq alts)
           (every? kw-val? alts))
      (into #{} (map :val alts))

      (and (= 2 (count (set alts)))
           (some nil-val? alts)
           (some (complement nil-val?) alts))
      (list (qualify-spec-symbol 'nilable)
            (unparse-spec (first (remove nil-val? alts))))

      :else
      (let [specs (into {}
                        (map (fn [alt]
                               [(unparse-spec alt) alt]))
                        (concat (set inst?) (set other)))]
        (if (= 1 (count specs))
          (unparse-spec (simplify-spec-alias (second (first specs))))
          (list*-force (qualify-spec-symbol 'or)
                       (let [names (map 
                                     (fn [[s orig]]
                                       {:pre [(core/any? s)
                                              (type? orig)]
                                        :post [(keyword? %)]}
                                       ;; we can enhance the naming for s/or tags here
                                       (or ;; probably a predicate
                                           (when (symbol? s)
                                             (keyword (name s)))
                                           ;; a spec alias
                                           (when (keyword? s)
                                             (keyword (name s)))
                                           ;; literal keywords
                                           (when (and (set? s)
                                                      (every? keyword? s))
                                             :kw)
                                           ;; an instance check
                                           (when (and (seq? s)
                                                      (= (count s) 3)
                                                      (= (first s) (qualify-core-symbol 'partial))
                                                      (= (second s) (qualify-core-symbol 'instance?))
                                                      (symbol? (nth s 2)))
                                             (keyword (name (nth s 2))))
                                           ;; a coll-of
                                           (when (and (seq? s)
                                                      (>= (count s) 2)
                                                      (= (first s) (qualify-spec-symbol 'coll-of)))
                                             :coll)
                                           ;; a map-of
                                           (when (and (seq? s)
                                                      (>= (count s) 3)
                                                      (= (first s) (qualify-spec-symbol 'map-of)))
                                             :map)
                                           ;; a tuple
                                           (when (and (seq? s)
                                                      (>= (count s) 1)
                                                      (= (first s) (qualify-spec-symbol 'tuple)))
                                             :tuple)
                                           ;; an empty thing
                                           (when (and (seq? s)
                                                      (>= (count s) 3)
                                                      (let [[c1 c2 c3] s]
                                                        (= c1 (qualify-spec-symbol 'and))
                                                        (#{(qualify-core-symbol 'empty?)} c3)
                                                        (#{(qualify-core-symbol 'coll?)
                                                           (qualify-core-symbol 'map?)}
                                                                                c2)))
                                             (keyword (str "empty"
                                                           (case (symbol (name (nth s 1)))
                                                             coll? (or (let [[_ _ _ & args] s]
                                                                         (when (even? (count args))
                                                                           (let [opts (apply hash-map args)]
                                                                             (when (#{(qualify-core-symbol 'vector?)} (:into opts))
                                                                               "-vector"))))
                                                                       "-coll")
                                                             map? "-map"
                                                             nil))))
                                           ;; give up
                                           (keyword (name (gensym)))))
                                     specs)
                             names (uniquify names)
                             ;; FIXME sort by key, but preserve instance checks first
                             names+specs (map vector names (map first specs))]
                         (apply concat names+specs))))))))

(def ^:dynamic *qualified-spec-aliases* nil) ;; for just-in-time generated spec aliases where namespaces matter
(def ^:dynamic *spec-aliases* nil)
(def ^:dynamic *used-aliases* nil)
(def ^:dynamic *multispecs-needed* nil)

(defn should-gen-just-in-time-alias? [t]
  (or (HMap? t)
      (alias? t)))

(defn spec-cat [args]
  (assert (even? (count args)))
  (list*-force (qualify-spec-symbol 'cat) args))

(declare fully-resolve-alias HMap-likely-tag-key
         register-just-in-time-alias)

(defn HMap-has-tag-key? [m k]
  (kw-val? (get (::HMap-req m) k)))

(defn uniquify [ss]
  {:pre [(every? keyword? ss)]
   :post [(every? keyword? %)]}
  (cond
    (or (empty? ss)
        (apply distinct? ss)) ss
    :else
    (let [repeats (into {}
                        (remove (comp #{1} val))
                        (frequencies ss))
          ;; first, let's try and just append an index
          ;; to each repeated entry. If that fails, just gensym.
          optimistic-attempt (map-indexed
                               (fn [i s]
                                 {:pre [(keyword? s)]
                                  :post [(keyword? %)]}
                                 (if (contains? repeats s)
                                   (keyword (str (name s) "-" i))
                                   s))
                               ss)]
      (if (apply distinct? optimistic-attempt)
        optimistic-attempt
        (map (fn [s]
               {:pre [(keyword? s)]
                :post [(keyword? %)]}
               (if (contains? repeats s)
                 (keyword (str (gensym (name s))))
                 s))
             ss)))))

(def ^:dynamic *preserve-unknown* nil)
(def ^:dynamic *higher-order-fspec* nil)

(declare join alias-matches-key-for-spec-keys? alternative-arglists kw-vals?)

(def resolve-JVM-primitive
  {"B" Byte/TYPE
   "C" Character/TYPE
   "D" Double/TYPE
   "F" Float/TYPE
   "I" Integer/TYPE
   "J" Long/TYPE
   "S" Short/TYPE
   "Z" Boolean/TYPE})

; [Node :-> Any]
(defn unparse-type' [{:as m}]
  (assert (type? m) m)
  (case (:op m)
    :alias (do
             (when-let [used-aliases *used-aliases*]
               (swap! used-aliases conj (:name m)))
             (cond
               *unparse-spec* (alias->spec-kw (:name m))
               :else
               (if *unparse-abbrev-alias*
                 (-> (:name m) name symbol)
                 (if (= (some-> (namespace (:name m)) symbol)
                        (current-ns))
                   (symbol (name (:name m)))
                   (:name m)))))
    :val (let [t (:val m)]
           (cond
             *unparse-spec* (cond
                              (nil? t) (qualify-core-symbol 'nil?)
                              (false? t) (qualify-core-symbol 'false?)
                              (keyword? t) #{t} #_(qualify-core-symbol 'keyword?)
                              (string? t) (qualify-core-symbol 'string?)
                              :else (qualify-core-symbol 'any?))
             :else
             (cond
               ((some-fn nil? false?) t) t
               (keyword? t) `'~t
               :else (qualify-typed-symbol 'Any))))
    :union (cond
             *unparse-spec* 
             (let [env *envs*
                   fully-res (if env
                               #(fully-resolve-alias @env %)
                               identity)
                   ts (map fully-res (:types m))]
               (if-let [tag (and env
                                 (every? HMap? ts)
                                 (HMap-likely-tag-key ts))]
                 ;; if we have a bunch of maps with a similar key,
                 ;; generate a multispec
                 (let [multispecs *multispecs-needed*
                       nme (gensym (str (when-let [nstr (namespace tag)]
                                          (str nstr "-"))
                                        (name tag) "-multi-spec"))
                       dmulti (list
                                (qualify-core-symbol 'defmulti)
                                (with-meta nme
                                           {::generated true})
                                tag)
                       dmethods (mapv (fn [t]
                                        {:pre [(HMap? t)]}
                                        (let [this-tag (get (::HMap-req t) tag)
                                              _ (assert (kw-val? this-tag)
                                                        (unparse-type this-tag))]
                                          (list (qualify-core-symbol 'defmethod) 
                                                nme 
                                                (:val this-tag)
                                                ['_]
                                                (unparse-type t))))
                                      ts)
                       _ (when multispecs
                           (swap! multispecs conj (vec (cons dmulti dmethods))))]
                   (list (qualify-spec-symbol 'multi-spec)
                         nme
                         tag))
                 (or-spec (:types m))))
             :else
             ;; core.typed
             (cond
               (empty? (:types m))
               (qualify-typed-symbol 'Nothing)

               ;; deterministic print order based on dispatch key
               (every? HMap? (:types m))
               (let [ts (:types m)
                     k (HMap-likely-tag-key ts)
                     ts (if (and k (every? #(HMap-has-tag-key? % k) ts))
                          (sort-by (fn [m]
                                     {:post [(keyword? %)]}
                                     (:val (get (::HMap-req m) k)))
                                   ts)
                          ts)
                     ts (distinct (mapv unparse-type ts))]
                 (cond
                   (= (count ts) 1) (first ts)
                   :else (list* (qualify-typed-symbol 'U) 
                                ts)))

               :else
               (let [ts (into #{}
                              (map unparse-type) 
                              (:types m))]
                 (cond
                   (= 1 (count ts)) (first ts)
                   :else
                   (list* (qualify-typed-symbol 'U) 
                          ts)))))
    :HVec (cond
            *unparse-spec* (list* (qualify-spec-symbol 'tuple)
                                  (mapv unparse-type (:vec m)))
            :else `'~(mapv unparse-type (:vec m)))
    :HMap (cond
            *unparse-spec* (let [specify-keys 
                                 (fn [entries]
                                   (-> (let [kns (name (gensym "keys"))]
                                           (map (fn [[k v]]
                                                  {:pre [(keyword? k)]}
                                                  ;; must be aliases
                                                  (cond
                                                    (or ;; already registered these values in correct spec
                                                        (kw-vals? v)
                                                        (and (alias? v)
                                                             (alias-matches-key-for-spec-keys? v k)))
                                                    (let [_ (when-let [used-aliases *used-aliases*]
																															(when (alias? v)
																																(swap! used-aliases conj (:name v))))
																													ka (cond 
																															 (alias? v) (keyword (:name v))
																															 (namespace k) k
																															 :else (keyword "clojure.core.typed.unqualified-keys" (name k)))]
																											(when-let [used-aliases *used-aliases*]
																												(swap! used-aliases conj (kw->sym ka)))
                                                      ka)

                                                    (namespace k)
                                                    (cond
                                                      (and *qualified-spec-aliases* *envs*)
                                                      ;; generate aliases just in time
                                                      (let [_ (println (str "WARNING: Generating spec alias for " k " just-in-time."))
                                                            spec-aliases *qualified-spec-aliases*
                                                            _ (assert spec-aliases)
                                                            envs *envs*
                                                            _ (assert envs)
                                                            _ (swap! spec-aliases update k 
                                                                     (fn [t1 t2]
                                                                       (if t1
                                                                         (join t1 t2)
                                                                         t2))
                                                                     v)
                                                            _ (swap! envs update-alias-env assoc k (get @spec-aliases k))]
                                                        k)

                                                      :else (do (println (str "WARNING: Could not generate "
                                                                              "spec alias for " k))
                                                                k))
                                                    ;; for unqualified keyword map entries, we can just gensym
                                                    ;; an appropriate alias if it's not already mapped
                                                    ;; to one.
                                                    :else
                                                    ;; punt?
                                                    (do
                                                      (when-let [used-aliases *used-aliases*]
                                                        (swap! used-aliases conj 
                                                               (symbol "clojure.core.typed.unqualified-keys" (name k))))
                                                      (keyword "clojure.core.typed.unqualified-keys" (name k)))
                                                    ;; TODO idea to reduce alias names: combine like-named
                                                    ;; keys that are reachable from the root of a type.
                                                    ;; ie. (U (HMap :mandatory {:foo Int})
                                                    ;;        (HMap :mandatory {:foo Bar}))
                                                    ;;  both share ::foo, but
                                                    ;; an unrelated map has its own.
                                                    ;; Might not work that well with multi-specs that share
                                                    ;; key names that are otherwise unrelated.
                                                    #_
                                                    (let [maybe-kw
                                                          (when (or (not (alias? v))
                                                                    (let [n (:name v)]
                                                                      ;; it's an alias, but the name isn't correct
                                                                      (or (not= (name k) (name n))
                                                                          ;; it's an alias, but the namespace isn't 
                                                                          ;; correct either
                                                                          (when (namespace k)
                                                                            (not= k (keyword n))))))
                                                            ;; generate aliases just in time
                                                            (when-let [spec-aliases *spec-aliases*]
                                                              (when-let [envs *envs*]
                                                                (let [kw (keyword kns
                                                                                  (name k))
                                                                      _ (swap! spec-aliases assoc kw v)
                                                                      _ (swap! envs update-alias-env
                                                                               assoc kw v)]
                                                                  kw))))]
                                                      (or
                                                        maybe-kw
                                                        (let [gk (keyword (gensym "clojure.core.typed.unqualified-keys")
                                                                          (name k))
                                                              _ (when-not maybe-kw
                                                                  (println "WARNING: Cannot generate just-in-time alias "
                                                                           "for unqualified key " k ", using "
                                                                           gk " as a placeholder"))]
                                                          gk)))))
                                                entries))
                                       sort
                                       vec))
                                 {req true req-un false}
                                 (group-by (comp boolean namespace key) (::HMap-req m))
                                 {opt true opt-un false}
                                 (group-by (comp boolean namespace key) (::HMap-opt m))
                                 ]
                             (list* (qualify-spec-symbol 'keys)
                                    (concat
                                      (when (seq req)
                                        [:req (specify-keys req)])
                                      (when (seq opt)
                                        [:opt (specify-keys opt)])
                                      (when (seq req-un)
                                        [:req-un (specify-keys req-un)])
                                      (when (seq opt-un)
                                        [:opt-un (specify-keys opt-un)]))))
            :else
            (let [{:keys [::HMap-req ::HMap-opt]} m
                  unp-map (fn [m]
                            (into {}
                                  (map (fn [[k v]]
                                         [k (unparse-type v)]))
                                  m))
                  req (unp-map HMap-req)]
              (cond
                (seq HMap-opt)
                (list* (qualify-typed-symbol 'HMap)
                       (concat
                         (when (seq req)
                           [:mandatory req])
                         [:optional (unp-map HMap-opt)]))
                :else `'~req)))
    :IFn1 (let [{:keys [dom rng fixed-name-lookup]} m
                these-names (get fixed-name-lookup (count dom))
                these-names (when (= (count dom) (count these-names))
                              these-names)
                ;; we could extract better names from destructuring here.
                these-names (when (every? symbol? these-names)
                              these-names)]
            (assert (every? identity [dom rng]))
            (assert (or (nil? these-names)
                        (and (vector? these-names)
                             (= (count dom) (count these-names)))))
            (conj (mapv (fn [t nme]
                          (if (and nme *new-aliases*
                                   (should-gen-just-in-time-alias? t))
                            (let [sym (register-just-in-time-alias (camel-case nme) t)]
                              (unparse-type (-alias sym)))
                            (unparse-type t)))
                        dom
                        (or these-names
                            (repeat (count dom) nil)))
                  :->
                  (unparse-type rng)))
    :IFn (let [{:keys [arities top-level-def]} m
               top-level-var (when (and (symbol? top-level-def)
                                        (namespace top-level-def)) ;; testing purposes
                               (some-> top-level-def find-var))
               ;_ (prn "top-level-var" top-level-def top-level-var)
               arglists (when top-level-var
                          (or (-> top-level-var meta :arglists)
                              (->> top-level-var coerce/var->symbol (get @alternative-arglists))))
               ;_ (prn "arglists" arglists)
               macro? (some-> top-level-var meta :macro)
               {fixed-arglists :fixed [rest-arglist] :rest}
               (group-by (fn [v]
                           {:pre [(vector? v)]}
                           (if (and (<= 2 (count v))
                                    (#{'&} (get v (- (count v) 2))))
                             :rest
                             :fixed))
                         arglists)
               _ (assert ((some-fn nil? (every-pred vector? #(<= 2 (count %)))) rest-arglist))
               ;; expand varargs into extra fixed arguments
               fixed-arglists (into (or fixed-arglists [])
                                    (when rest-arglist
                                      (let [fixed-arg-nums (into #{} (map count) fixed-arglists)
                                            fixed (subvec rest-arglist 0 (- (count rest-arglist) 2))
                                            rst-arg (peek rest-arglist)
                                            extra-fixed (if (vector? rst-arg)
                                                          (vec (take-while (complement #{'& :as}) rst-arg))
                                                          [])]
                                        (->> (map #(into fixed (subvec extra-fixed 0 %)) (range (inc (count extra-fixed))))
                                             ;; prefer actual fixed arguments over derived ones
                                             (remove (comp fixed-arg-nums count))))))
               ;_ (prn "fixed-arglists" fixed-arglists)
               ;_ (prn "rest-arglist" rest-arglist)
               ;; map from arity length to vector of fixed arguments
               fixed-name-lookup (into {}
                                       (map (fn [v]
                                              [(count v) v]))
                                       fixed-arglists)]
           ;(prn "fixed-name-lookup" fixed-name-lookup)
           (cond
             *unparse-spec* 
             (cond
               ;; erase higher-order function arguments by default,
               ;; use *higher-order-fspec* to leave as fspecs.
               ; It's also important that we don't unparse specs
               ; we don't use so we don't create garbage aliases, so
               ; this must go first.
               (not (or top-level-var *higher-order-fspec*))
               (qualify-core-symbol 'ifn?)

               :else
               (let [;; if we have a macro, ignore the first two arguments
                     ;; in each arity (&env and &form)
                     arities (if macro?
                               (map (fn [a]
                                      (update a :dom (fn [dom]
                                                       (if (<= 2 (count dom))
                                                         (subvec dom 2)
                                                         dom))))
                                    arities)
                               arities)
                     doms (cond
                            macro?
                            [(spec-cat
                               (concat
                                 ;; macros are very likely to having binding
                                 ;; forms as the first argument if it's always
                                 ;; a vector.
                                 (when (every? (fn [{:keys [dom]}]
                                                 ;; every first argument is a vector
                                                 (let [[d] dom]
                                                   (when d
                                                     (and
                                                       (#{:class} (:op d))
                                                       (= clojure.lang.IPersistentVector
                                                          (::class-instance d))))))
                                               arities)
                                   [:bindings (keyword (str core-specs-ns) "bindings")])
                                 ;; if there is more than one arity,
                                 ;; default to a rest argument.
                                 [:body
                                  (if (<= 2 (count arities))
                                    (list (qualify-spec-symbol '*)
                                          (qualify-core-symbol 'any?))
                                    (qualify-core-symbol 'any?))]))]
                            :else
                            (mapv
                              (fn [{:keys [dom]}]
                                {:pre [dom]}
                                ;(prn "doms" (count dom) (keyword (get fixed-name-lookup (count dom))))
                                (let [knames (let [[matching-fixed-names rest-arg-name]
                                                   (or (when-let [f (get fixed-name-lookup (count dom))]
                                                         [f nil])
                                                       (when rest-arglist
                                                         (assert (vector? rest-arglist))
                                                         (when (>= (count dom) (dec (count rest-arglist)))
                                                           [(subvec rest-arglist 0 (- (count rest-arglist) 2))
                                                            (peek rest-arglist)])))
                                                   keywordify-arg 
                                                   (fn [arg]
                                                     ;; here we can improve naming by examining destructuring
                                                     (cond
                                                       ;; simple argument name
                                                       (symbol? arg) (keyword (namespace arg) (name arg))

                                                       ;; {:as foo} map destructuring
                                                       (and (map? arg)
                                                            (symbol? (:as arg)))
                                                       (keyword (namespace (:as arg)) 
                                                                (name (:as arg)))

                                                       ;; [:as foo] vector destructuring
                                                       (and (vector? arg)
                                                            (<= 2 (count arg))
                                                            (#{:as} (nth arg (- (count arg) 2)))
                                                            (symbol? (peek arg)))
                                                       (keyword (namespace (peek arg))
                                                                (name (peek arg)))))
                                                   combined-kws 
                                                   (let [fixed-kws (map-indexed (fn [n arg]
                                                                                  (or (keywordify-arg arg)
                                                                                      (let [s (or #_(some-> top-level-def name)
                                                                                                  "arg")]
                                                                                        (keyword (str s "-" n)))))
                                                                                matching-fixed-names)
                                                         rest-kws (when rest-arg-name
                                                                    (let [dom-remain (- (count dom) (count fixed-kws))
                                                                          kw-arg (keywordify-arg rest-arg-name)
                                                                          prefix (if kw-arg
                                                                                   (str (when-let [n (namespace kw-arg)]
                                                                                          (str n "/"))
                                                                                        (name kw-arg))
                                                                                   (str "rest-arg"))]
                                                                      (map (fn [n]
                                                                             (keyword (str prefix "-" n)))
                                                                           (range dom-remain))))
                                                         combined-kws (vec (uniquify (concat fixed-kws rest-kws)))]
                                                     (if (= (count dom) (count combined-kws))
                                                       combined-kws
                                                       (mapv (fn [n] (keyword (str "arg-" n)))
                                                             (range (count dom)))))]
                                               (assert (= (count dom) (count combined-kws)))
                                               combined-kws)]
                                  (spec-cat
                                    (mapcat (fn [n k d]
                                              {:pre [(keyword? k)]}
                                              (let [spec 
                                                    (cond
                                                      (and (zero? n)
                                                           macro?
                                                           (#{:class} (:op d))
                                                           (= clojure.lang.IPersistentVector
                                                              (::class-instance d)))
                                                      (keyword (str core-specs-ns) "bindings")

                                                      :else (unparse-spec d))]
                                                [k spec]))
                                            (range)
                                            knames
                                            dom))))
                              arities))
                     rngs (if macro?
                            (qualify-core-symbol 'any?)
                            (or-spec (let [u (make-Union (map :rng arities))]
                                       (if (union? u)
                                         (:types u)
                                         [u]))))
                     dom-specs (if (= 1 (count doms))
                                 (first doms)
                                 (list* (qualify-spec-symbol 'alt) ;; use alt to treat args as flat sequences
                                        (let [named-alts (map (fn [alt]
                                                                (let [kw (keyword (let [n (/ (dec (count alt)) 2)]
                                                                                    (str n (or (when (= 1 n)
                                                                                                 "-arg")
                                                                                               "-args"))))]
                                                                  [kw alt]))
                                                              doms)]
                                          (apply concat (sort-by first named-alts)))))]
                  (list* (qualify-spec-symbol 'fspec)
                         [:args dom-specs
                          :ret rngs])))
             :else
             (let [as (mapv (fn [a]
                             (unparse-type
                              (assoc a
                                     :fixed-name-lookup fixed-name-lookup)))
                           arities)]
               (if (== 1 (count as))
                 (first as)
                 (list* (qualify-typed-symbol 'IFn) as)))))
    :unresolved-class (cond
                        *unparse-spec* (let [cls-str (::class-string m)
                                             args (:args m)]
                                         (list (qualify-core-symbol 'partial)
                                               (qualify-core-symbol 'instance?)
                                               (list 'Class/forName cls-str)))
                        :else (letfn [(unparse-class [^String cstr args]
                                        {:pre [(string? cstr)]}
                                        (cond
                                          (.startsWith cstr "[") (list 'Array (unparse-class (.substring cstr 1) args))
                                          :else
                                          (let [cls (if-some [c (resolve-JVM-primitive cstr)]
                                                      (resolve-class c)
                                                      (symbol cstr))
                                                _ (assert (symbol? cls))]
                                            (if (seq args)
                                              (list*-force cls (map unparse-type args))
                                              cls))))]
                                (unparse-class (::class-string m) (:args m))))
    :class (cond
             *unparse-spec* (let [^Class cls (::class-instance m)
                                  args (:args m)]
                              (cond
                                (#{Long Integer Short Byte} cls) (qualify-core-symbol 'int?)
                                (#{BigInteger} cls) (qualify-core-symbol 'integer?)
                                (#{BigDecimal} cls) (qualify-core-symbol 'bigdec?)
                                (.isAssignableFrom Number cls) (qualify-core-symbol 'number?)
                                (#{Character} cls) (qualify-core-symbol 'char?)
                                (#{clojure.lang.Symbol} cls) (qualify-core-symbol 'symbol?)
                                (#{clojure.lang.Keyword} cls) (qualify-core-symbol 'keyword?)
                                (#{String} cls) (qualify-core-symbol 'string?)
                                (#{clojure.lang.IFn} cls) (qualify-core-symbol 'ifn?)
                                (#{Boolean} cls) (qualify-core-symbol 'boolean?)
                                ;; TODO check set elements
                                (#{clojure.lang.IPersistentSet} cls) (qualify-core-symbol 'set?)
                                (#{clojure.lang.IPersistentMap} cls)
                                (if (some nothing? args)
                                  (list (qualify-spec-symbol 'and)
                                        (qualify-core-symbol 'map?)
                                        (qualify-core-symbol 'empty?))
                                  (let [[k v] args]
                                    (list (qualify-spec-symbol 'map-of)
                                          (unparse-spec k)
                                          (unparse-spec v))))
                                (#{clojure.lang.IPersistentVector clojure.lang.IPersistentCollection
                                   clojure.lang.ISeq} cls) 
                                (if (nothing? (first args))
                                  (list (qualify-spec-symbol 'and)
                                        (qualify-core-symbol 'coll?)
                                        (qualify-core-symbol 'empty?))
                                  (list*-force
                                    (qualify-spec-symbol 'coll-of)
                                    (unparse-spec
                                      (first args))
                                    (when (#{clojure.lang.IPersistentVector} cls)
                                      [:into (qualify-core-symbol 'vector?)])))

                                :else (list (qualify-core-symbol 'partial)
                                            (qualify-core-symbol 'instance?)
                                            (if (.isArray cls)
                                              (list 'Class/forName (.getName cls))
                                              (symbol (.getName cls))))))
             :else
             (letfn [(unparse-class [^Class c args]
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
                                     clojure.lang.ISeq (qualify-typed-symbol 'Coll)
                                     clojure.lang.IPersistentList (qualify-typed-symbol 'Coll)
                                     clojure.lang.IPersistentCollection (qualify-typed-symbol 'Coll)
                                     Number  (qualify-typed-symbol 'Num)
                                     Long    (qualify-typed-symbol 'Int)
                                     Integer (qualify-typed-symbol 'Int)
                                     clojure.lang.IFn 'AnyFunction
                                     java.lang.String (qualify-typed-symbol 'Str)
                                     (resolve-class c))
                               _ (assert (symbol? cls))]
                           (if (seq args)
                             (list*-force cls (map unparse-type args))
                             cls))))]
               (unparse-class (::class-instance m) (:args m))))
    :Top (cond 
           *unparse-spec* (qualify-core-symbol 'any?)
           :else (qualify-typed-symbol 'Any))
    :unknown (cond 
               *preserve-unknown* '?
               *unparse-spec* (qualify-core-symbol 'any?)
               :else (qualify-typed-symbol 'Any))
    :free (cond
            *unparse-spec* (alias->spec-kw (:name m))
            :else (:name m))
    :poly (cond
            *unparse-spec* (throw (Exception. "TODO"))
            :else (list 'All (into (mapv (fn [[ps {:keys [weight name types]}]]
                                           {:pre [(= 2 (count ps))]}
                                           [name 
                                            types
                                            weight :of 
                                            [(get-in @results-atom [:path-occ (first ps)] 0)
                                             (get-in @results-atom [:path-occ (second ps)] 0)]
                                            '<- (mapv unparse-path ps)])
                                         (:params m))
                                   (:known-params m))
                        (unparse-type (:type m))))
    (assert nil (str "No unparse-type case: " m))))

(defn unp [t]
  (binding [*spec* false]
    (unparse-type t)))

(declare pprint)

(defn unp-str [t]
  (let [^String s 
        (with-out-str
          (binding [pp/*print-right-margin* nil]
            (pprint (unp t))))]
    (.replaceAll s "\\n" "")))

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
  {:pre [;; very slow
         #_(every? type? ts)]
   :post [(set? %)]}
  (into #{} 
        (mapcat flatten-union)
        ts))

(declare join-HMaps join* join kw-val? kw-vals?)

(def val? (comp boolean #{:val} :op))

(defn merge-HMaps [ms]
  {:post [(HMap? %)]}
  (reduce join-HMaps (first ms) (rest ms)))

(defn HMap-common-req-keys [ms]
  {:pre [(every? HMap? ms)]
   :post [(set? %)
          (every? keyword? %)]}
  (intersection-or-empty
    (map HMap-req-keyset ms)))

(defn HMap-likely-tag-key 
  ([hmaps] (some #(HMap-likely-tag-key hmaps %)
                 (HMap-common-req-keys hmaps)))
  ([hmaps k]
   {:pre [(every? HMap? hmaps)
          (keyword? k)]
    :post [((some-fn nil? keyword?) %)]}
   (when (every? (fn [m]
                   {:pre [(HMap? m)]}
                   (kw-vals? (get (::HMap-req m) k)))
                 hmaps)
     k)))

#_
(defmacro let-debug [bnd & body]
  (let [bnds (partition 2 bnd)
        split (first
                (first
                  (filter
                    #(= :debug (-> % second first))
                    (map-indexed vector bnds))))
        [before-debug
         [the-debug & [:as after-debug]]] 
        (if split
          (split-at split bnds)
          [bnds nil])
        ]
    `(let [~@(apply concat before-debug)]
       ~(if (seq after-debug)
          `(debug ~(second the-debug)
             (let-debug [~@(apply concat after-debug)]
               ~@body))
          `(do ~@body)))))

(defn upcast-HVec [h]
  {:pre [(#{:HVec} (:op h))]
   :post [(type? %)]}
  (-class clojure.lang.IPersistentVector 
          [(apply join* (:vec h))]))


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
                             ;#{(-class clojure.lang.IPersistentMap [-any -any])}
                             #{{:op :HMap
                                ::HMap-req {}
                                ::HMap-opt (apply merge-with join (mapcat (juxt ::HMap-req ::HMap-opt) hmaps-merged))}})

                           ;; if one of the common keys is always mapped to a singleton keyword,
                           ;; merge by the value of this key
                           likely-tag
                           (let [by-tag (group-by (fn [m]
                                                    (get (::HMap-req m) likely-tag))
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
                                    ::HMap-req (apply merge-with join
                                                      (map (fn [m]
                                                             {:pre [(HMap? m)]}
                                                             (let [es (select-keys (::HMap-req m) 
                                                                                   common-keys)]
                                                               (doseq [[_ v] es]
                                                                 (when (unknown? v)
                                                                   (reset! has-unknown? true)))
                                                               es))
                                                           hmaps-merged))
                                    ;; all the rest are optional
                                    ::HMap-opt (apply merge-with join
                                                      (map (fn [m]
                                                             {:pre [(HMap? m)]}
                                                             (let [es 
                                                                   (merge-with join
                                                                               (::HMap-opt m)
                                                                               (apply dissoc (::HMap-req m)
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
                        relevant-seqables #{clojure.lang.ISeq
                                            clojure.lang.IPersistentCollection
                                            clojure.lang.IPersistentList
                                            clojure.lang.IPersistentVector}
                        ;; upcast seqables if appropriate
                        classes (let [{seqable-classes true 
                                       non-seqable-classes false} 
                                      (group-by #(contains? relevant-seqables (::class-instance %)) classes)
                                      seqable-classes
                                      (if (some (comp #{clojure.lang.IPersistentList
                                                        clojure.lang.ISeq
                                                        clojure.lang.IPersistentCollection}
                                                      ::class-instance)
                                                seqable-classes)
                                        ;; upcast all to Coll since we've probably lost too much type information
                                        ;; to bother keeping seqable-classes around.
                                        [(-class clojure.lang.IPersistentCollection 
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
                                             (-class (-> cs first ::class-instance)
                                                     (apply mapv join* (map :args cs)))))
                                     (vals (group-by ::class-instance classes)))]
                    (into classes non-classes))

        ;; delete HMaps if there's already a Map in this union,
        ;; unless it's a (Map Nothing Nothing)
        hmaps-merged (if (some (fn [m]
                                 (and (-class? m)
                                      (#{clojure.lang.IPersistentMap} (::class-instance m))
                                      (not-every? nothing? (:args m))))
                               non-hmaps)
                       #{}
                       hmaps-merged)

        ts (into hmaps-merged non-hmaps)
        
        ;; upcast true/false singletons to Boolean if Boolean is present
        ts (if (contains? ts (-class Boolean []))
             (disj ts (-val true) (-val false))
             ts)

        _ (assert (set? ts))
        ;; upcast Long and Double combination to t/Num
        ts (cond
             (or (and (or (contains? ts (-class Long []))
                          (contains? ts (-class Integer []))
                          (contains? ts (-class Short []))
                          (contains? ts (-class Byte []))
                          (contains? ts (-class BigInteger [])))
                      (or (contains? ts (-class Double []))
                          (contains? ts (-class BigDecimal []))))
                 (contains? ts (-class Number [])))
             (-> (disj ts 
                       (-class Long [])
                       (-class Integer [])
                       (-class Short [])
                       (-class Byte [])
                       (-class BigInteger [])
                       (-class Double [])
                       (-class BigDecimal []))
                 (conj (-class Number [])))

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
                           (comp boolean #{clojure.lang.IPersistentVector
                                           clojure.lang.IPersistentCollection} ::class-instance))
                         non-HVecs)
                       _ (assert (= (count non-HVecs) (+ (count vec-classes) (count non-vecs))))
                       ;; erase HVec's if we have a IPV class
                       [HVecs vec-classes]
                       (if (seq vec-classes)
                         [[]
                          (cons 
                            (let [class-name (if (every? (comp boolean #{clojure.lang.IPersistentVector} ::class-instance) vec-classes)
                                               clojure.lang.IPersistentVector
                                               clojure.lang.IPersistentCollection)
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
        ;       (conj (set non-kws) (-class clojure.lang.Keyword []))
        ;       ts))
        seqable-t? (fn [m]
                     (boolean
                       (when (-class? m)
                         (or (= clojure.lang.Seqable (::class-instance m))
                             (contains? (set (supers (::class-instance m))) 
                                        clojure.lang.Seqable)))))
        atomic-type? (fn [v]
                       (boolean
                         (or
                           (and (val? v)
                                (some? (:val v)))
                           (and (-class? v)
                                (#{clojure.lang.Symbol
                                   String
                                   clojure.lang.Keyword}
                                  (::class-instance v))))))
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

; should-join-HMaps? : HMap HMap -> Bool
(defn should-join-HMaps? [t1 t2]
  {:pre [(HMap? t1)
         (HMap? t2)]}
  ;; join if the required keys are the same,
  ;; and there is not common key mapped to keywords.
  ;; TODO and if 75% of the keys are the same
  ;; TODO and if common keys are not always different keywords
  (let [ts [t1 t2]
        t1-map (::HMap-req t1)
        t2-map (::HMap-req t2)
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


(defn join-HMaps [t1 t2]
  {:pre [(HMap? t1)
         (HMap? t2)
         ;(should-join-HMaps? t1 t2)
         ]
   :post [(HMap? %)]}
  ;(prn "joining HMaps"
  ;     (unparse-type t1)
  ;     (unparse-type t2))
  (let [t1-req (::HMap-req t1)
        t2-req (::HMap-req t2)
        t1-opt (::HMap-opt t1)
        t2-opt (::HMap-opt t2)
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
               ::HMap-req (into {}
                                (map (fn [k]
                                       {:pre [(keyword? k)]}
                                       (let [ts (keep k [t1-req t2-req])]
                                         ;(prn "req k" k)
                                         ;(prn "ts" ts)
                                         (assert (seq ts))
                                         [k (apply join* ts)])))
                                new-reqs)
               ::HMap-opt (into {}
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

(defn join-IFn [t1 t2]
  {:pre [(#{:IFn} (:op t1))
         (#{:IFn} (:op t2))]
   :post [(type? %)]}
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
     :arities arities}))

(defmacro time-if-slow
  "Evaluates expr and prints the time it took.  Returns the value of expr."
  [msg expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr
         msduration# (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]
     (when (< 1000 msduration#)
       (prn (str "Elapsed time: " msduration# " msecs"))
       (prn ~msg))
     ret#))

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
                   (= (::class-instance t1)
                      (::class-instance t2))
                   (= (count (:args t1))
                      (count (:args t2))))
              (-class (::class-instance t1) (mapv join (:args t1) (:args t2)))

              (and (#{:class} (:op t1))
                   (= clojure.lang.IFn
                      (::class-instance t1))
                   (#{:IFn} (:op t2)))
              t2

              (and (#{:class} (:op t2))
                   (= clojure.lang.IFn
                      (::class-instance t2))
                   (#{:IFn} (:op t1)))
              t1

              (and (#{:HMap} (:op t1))
                   (#{:HMap} (:op t2))
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
    (= 1 (count path)) (let [x (nth path 0)]
                         (case (:op x)
                           :var (let [n (:name x)
                                      t (if-let [t (get (type-env env) n)]
                                          (join t type)
                                          type)]
                                  (assert (#{:var} (:op x))
                                          (str "First element of path must be a variable or local-fn " x))
                                  (update-type-env-in-ns env (or (:ns x) (current-ns))
                                                         assoc n t))))
    :else 
    (let [cur-pth (peek path)
          nxt-pth (pop path)]
      (assert (:op cur-pth) (str "What is this? " cur-pth
                                 " full path: " path))
      (case (:op cur-pth)
        :var (throw (Exception. "Var path element must only be first path element"))
        :key (let [{:keys [kw-entries keys key]} cur-pth]
               (recur env config
                      nxt-pth
                      {:op :HMap
                       ::HMap-req (merge (zipmap keys (repeat {:op :unknown}))
                                         ;; immediately associate kw->kw entries
                                         ;; to distinguish in merging algorithm
                                         kw-entries
                                         {key type})}))
        :set-entry (recur env config nxt-pth (-class clojure.lang.IPersistentSet [type]))
        :seq-entry (recur env config nxt-pth (-class clojure.lang.ISeq [type]))
        :vec-entry (recur env config nxt-pth (-class clojure.lang.IPersistentVector [type]))
        :map-keys (recur env config nxt-pth (-class clojure.lang.IPersistentMap [type {:op :unknown}]))
        :map-vals (recur env config nxt-pth (-class clojure.lang.IPersistentMap [{:op :unknown} type]))
        :transient-vector-entry (recur env config nxt-pth (-class clojure.lang.ITransientVector [type]))
        :atom-contents (recur env config nxt-pth (-class clojure.lang.IAtom [type]))
        :index (recur env config nxt-pth
                      (if true #_(= 2 (:count cur-pth))
                        {:op :HVec
                         :vec (assoc (vec (repeat (:count cur-pth) -unknown)) (:nth cur-pth) type)}
                        #_(-class clojure.lang.IPersistentVector [type])))
        :fn-domain (let [{:keys [arity position]} cur-pth]
                     (recur env config nxt-pth
                            {:op :IFn
                             :arities [{:op :IFn1
                                        :dom (let [dom (into [] 
                                                             (repeat (:arity cur-pth) {:op :unknown}))]
                                               (if (zero? arity)
                                                 dom
                                                 (assoc dom position type)))
                                        :rng {:op :unknown}}]}))
        :fn-range (let [{:keys [arity]} cur-pth]
                    (recur env config nxt-pth
                           {:op :IFn
                            :arities [{:op :IFn1
                                       :dom (into [] (repeat (:arity cur-pth) {:op :unknown}))
                                       :rng type}]}))))))

(defn update-grouped-paths [env config {:keys [:current-level :inner-level] :as paths}]
  {:pre [(map? env)
         (set? current-level)
         (map? inner-level)]
   :post [(type? %)]}
  ;(prn "update-grouped-paths current level:" (count current-level))
  ;(prn "update-grouped-paths num next levels:" (count inner-level))
  (apply join*
    (concat current-level
      (->> 
        inner-level
        (map (fn [[cur-pth next-group]]
               (let [type (update-grouped-paths env config next-group)]
                 (case (:op cur-pth)
                   :var (throw (Exception. "Var path element must only be first path element"))
                   :key (let [{:keys [kw-entries keys key]} cur-pth]
                          {:op :HMap
                           ::HMap-req (merge (zipmap keys (repeat {:op :unknown}))
                                             ;; immediately associate kw->kw entries
                                             ;; to distinguish in merging algorithm
                                             kw-entries
                                             {key type})})
                   :set-entry (-class clojure.lang.IPersistentSet [type])
                   :seq-entry (-class clojure.lang.ISeq [type])
                   :vec-entry (-class clojure.lang.IPersistentVector [type])
                   :map-keys (-class clojure.lang.IPersistentMap [type {:op :unknown}])
                   :map-vals (-class clojure.lang.IPersistentMap [{:op :unknown} type])
                   :transient-vector-entry (-class clojure.lang.ITransientVector [type])
                   :atom-contents (-class clojure.lang.IAtom [type])
                   :index (if true #_(= 2 (:count cur-pth))
                            {:op :HVec
                             :vec (assoc (vec (repeat (:count cur-pth) -unknown)) (:nth cur-pth) type)}
                            #_(-class clojure.lang.IPersistentVector [type]))
                   :fn-domain (let [{:keys [arity position]} cur-pth]
                                {:op :IFn
                                 :arities [{:op :IFn1
                                            :dom (let [dom (into [] 
                                                                 (repeat (:arity cur-pth) {:op :unknown}))]
                                                   (if (zero? arity)
                                                     dom
                                                     (assoc dom position type)))
                                            :rng {:op :unknown}}]})
                   :fn-range (let [{:keys [arity]} cur-pth]
                               {:op :IFn
                                :arities [{:op :IFn1
                                           :dom (into [] (repeat (:arity cur-pth) {:op :unknown}))
                                           :rng type}]})))))))))

(defn grouped-paths-to-env [env config {:keys [:current-level :inner-level] :as paths}]
  {:pre [(empty? current-level)
         (map? inner-level)]}
  (into {}
        (map (fn [[pth next-group]]
               {:pre [(#{:var} (:op pth))]}
               ;(prn "grouped-paths-to-env" (:name pth))
               [(:name pth) (update-grouped-paths env config next-group)]))
        inner-level))

(defn walk-type-children [v f]
  {:pre [(type? v)]
   :post [(type? %)]}
  (case (:op v)
    (:val :alias :unknown :Top) v
    :HMap (let [up (fn [m]
                     (reduce-kv
                       (fn [m k v]
                         (assoc m k (f v)))
                       {}
                       m))]
            (-> v
                (update ::HMap-req up)
                (update ::HMap-opt up)))
    (:class :unresolved-class)
           (update v :args (fn [m]
                             (reduce-kv
                               (fn [m k v]
                                 (assoc m k (f v)))
                               []
                               m)))
    :HVec (update v :vec (fn [m]
                           (into []
                                 (map f)
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

(defn register-just-in-time-alias [sym t]
  {:pre [(not (namespace sym))]}
  (assert *new-aliases*)
  (let [sym (if (or (contains? (alias-env @*envs*) sym)
                    (contains? (alias-env @*new-aliases*) sym))
              (gensym (str (name sym) "__"))
              sym)
        _ (swap! *new-aliases* register-alias (init-config) sym t)
        ]
    sym))

(defn gen-unique-alias-name [env config sym]
  (if (contains? (alias-env env) sym)
    (gensym (str (name sym) "__"))
    sym))

(defn register-unique-alias [env config sym t]
  {:pre [(not (namespace sym))]}
  ;(debug (println "register-unique-alias:" sym (unp-str t))
  (let [sym (gen-unique-alias-name env config sym)]
    [sym (register-alias env config sym t)])
  ;)
)

;; generate good alias name for `s/keys`
(defn register-unique-alias-for-spec-keys [env config k t]
  {:pre [(keyword? k)]}
  (let [qualified? (boolean (namespace k))
        sym (if qualified?
              (kw->sym k)
              ;; this namespace is unlikely to have qualified keys we care about,
              ;; so it's unique enough for our purposes.
              (symbol "clojure.core.typed.unqualified-keys"
                      (name k)))]
    ;(prn "register" sym)
    [sym (if true #_qualified?
           (update-alias-env env update sym (fnil join -nothing) t)
           (register-alias env config sym t))]))

(defn resolve-alias [env {:keys [name] :as a}]
  {:pre [(map? env)
         (alias? a)
         (symbol? name)]
   :post [(type? %)]}
  ;(prn "resolve-alias" name (keys (alias-env env)))
  (get (alias-env env) name))

(defn resolve-alias-or-nil [env {:keys [name] :as a}]
  {:pre [(map? env)
         (alias? a)
         (symbol? name)]
   :post []}
  ;(prn "resolve-alias" name (keys (alias-env env)))
  (get (alias-env env) name))

(defn fully-resolve-alias 
  ([env a] (fully-resolve-alias env a #{}))
  ([env a seen]
  (if (alias? a)
    (do (assert (not (contains? seen (:name a))) "Infinite type detected")
      (recur env (resolve-alias env a)
             (conj seen (:name a))))
    a)))

(def kw-val? (every-pred val? (comp keyword? :val)))

(defn kw-vals? [t]
  (boolean
    (or (kw-val? t)
        (when (union? t)
          (every? kw-val? (:types t))))))

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
  (let [singles (into {} (filter (comp kw-vals? val) (::HMap-req t)))]
    (when-let [[k t] (or
                       ;; TODO extensible hints
                       (when-let [e (or (find singles :op)
                                        ;...
                                        )]
                         e)
                       (and (= (count singles) 1)
                            (first singles)))]
      [k (case (:op t)
           :val #{(:val t)}
           :union (into #{}
                        (comp
                          (filter val?)
                          (map :val))
                        (:types t)))])))

(defn kw-vals->str [v]
  {:pre [(kw-vals? v)]}
  (if (val? v)
    (str (name (:val v)))
    (apply str (interpose "-" (map (comp name :val) (:types v))))))

(defn alias-single-HMaps
  "Traverse the type and alias environments
  and ensure all HMaps are aliased"
  [env config]
  (letfn [(do-alias [env-atom t prefix]
            {:pre [((some-fn string? nil?) prefix)]}
            (let [n (symbol (str prefix "-" (gensym "tmp-HMap-alias")))
                  _ (assert (not (namespace n)))
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
            (let [env-atom (atom env)
                  t (postwalk 
                      t
                      (fn [t]
                        (case (:op t)
                          :HMap (let [[k v]
                                      (first
                                        (filter (fn [[k v]]
                                                  (kw-vals? v))
                                                (::HMap-req t)))
                                      ;; information from a union takes precedence
                                      [k v] (or (when-let [upper-k (::union-likely-tag (meta t))]
                                                  (let [e (find (::HMap-req t) upper-k)]
                                                    (when (kw-vals? (val e))
                                                      e)))
                                                [k v])]
                                  (do-alias env-atom t 
                                            (or
                                              ;; try and give a tagged name
                                              (when k
                                                ;(prn "kw-vals" k v)
                                                (str (name k) "-" (kw-vals->str v)))
                                              ;; for small number of keys, spell out the keys
                                              (when (<= (+ (count (::HMap-req t))
                                                           (count (::HMap-opt t)))
                                                        2)
                                                (apply str (interpose "-" (map name (concat (keys (::HMap-req t))
                                                                                            (keys (::HMap-opt t)))))))
                                              ;; otherwise give abbreviated keys
                                              (apply str (interpose "-" 
                                                                    (map (fn [k]
                                                                           (apply str (take 3 (name k))))
                                                                         (concat
                                                                           (keys (::HMap-req t))
                                                                           (keys (::HMap-opt t)))))))))
                          t)))]
              [t @env-atom]))]
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
                                  (let [env-atm (atom env)
                                        t (walk-type-children
                                            t
                                            (fn [t]
                                              {:post [(type? %)]}
                                              (let [t-atom (atom nil)
                                                    _ (swap! env-atm
                                                             (fn [env]
                                                               (let [[t env] (single-HMap env t)]
                                                                 ;; don't care if this is retried
                                                                 (reset! t-atom t)
                                                                 env)))
                                                    t @t-atom]
                                                t)))]
                                    [t @env-atm])
                                  (single-HMap env t))]
                    (update-alias-env env assoc a t)))
                env
                (alias-env env))]
      env)))

;; Answers: is this a good alias to generate `s/keys`?
(defn alias-matches-key-for-spec-keys? [a k]
  {:pre [(alias? a)
         (keyword? k)]}
  (if (namespace k)
    (= (:name a) (kw->sym k))
    (and (= (name (:name a)) (name k))
         (= "clojure.core.typed.unqualified-keys"
            (namespace k)))))

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
                        ;; Since we just created these aliases, we don't need to
                        ;; further simplify via `make-Union`.
                        {:op :union
                         :types (set (map #(fully-resolve-alias @env-atom %) (:types t)))}

                        ;; if we are generating specs, we also want aliases
                        ;; for each HMap entry.
                        :HMap (if (:spec? config)
                                (let [update-entry (fn [[k v]]
                                                     {:pre [(keyword? k)]}
                                                     [k 
                                                      (if (and (alias? v)
                                                               (alias-matches-key-for-spec-keys? v k))
                                                        v
                                                        (let [sym-atom (atom nil)
                                                              _ (swap! env-atom
                                                                       (fn [env]
                                                                         (let [[sym env] (register-unique-alias-for-spec-keys env config k v)]
                                                                           (reset! sym-atom sym)
                                                                           env)))
                                                              sym @sym-atom
                                                              ;_ (prn "created spec s/keys alias" sym (unp v))
                                                              _ (assert (qualified-symbol? sym))]
                                                          (cond
                                                            (kw-vals? v) v
                                                            :else (-alias sym))))])
                                      t (reduce (fn [t k]
                                                  (update t k #(into {} (map update-entry) %)))
                                                t
                                                [::HMap-req ::HMap-opt])]
                                  t)
                                t)
                        t)
                    n (symbol (or
                                (let [ts (if (union? t)
                                           (map #(fully-resolve-alias @env-atom %) 
                                                (:types t))
                                           [(fully-resolve-alias @env-atom t)])]
                                  (when (every? HMap? ts)
                                    (let [common-keys (intersection-or-empty
                                                        (map (comp set keys ::HMap-req) ts))
                                          common-tag (first
                                                       (filter
                                                         (fn [k]
                                                           (every? (fn [m]
                                                                     {:pre [(HMap? m)]}
                                                                     (kw-val? (get (::HMap-req m) k)))
                                                                   ts))
                                                         common-keys))]
                                      (when common-tag
                                        (apply str 
                                               (interpose "-"
                                                          (concat
                                                            (cons (name common-tag)
                                                                  (map (comp name
                                                                             :val
                                                                             common-tag 
                                                                             ::HMap-req)
                                                                       ts))
                                                            ["alias"])))))))
                                "alias"))
                    _ (assert (not (namespace n)))
                    a-atom (atom nil)
                    _ (swap! env-atom 
                             (fn [env]
                               (let [[a env] (register-unique-alias env config n t)]
                                 (reset! a-atom a)
                                 env)))
                    a @a-atom
                    _ (assert a)]
                (-alias a)))]
      (let [t (postwalk t
                        (fn [t]
                          (case (:op t)
                            :HMap ;(debug
                            ;  (println "alias-hmap-type:" (unp-str t))
                            (do-alias t)
                            ;)
                            :union (if (and (seq (:types t))
                                            (not-every?
                                              (fn [t]
                                                (case (:op t)
                                                  :val true
                                                  :class (empty? (:args t))
                                                  false))
                                              (:types t)))
                                     ;(debug
                                     ;  (println "alias-union-type:" (unp-str t))
                                     (do-alias t)
                                     ;)
                                     t)
                            #_#_:IFn1 (if (:spec? config)
                                    (-> t
                                        (update :dom #(mapv do-alias %))
                                        (update :rng do-alias))
                                    t)
                            t)))]
        [t @env-atom]))))

(declare fv)

; try-merge-aliases : Env Config Sym Alias -> Env
(defn try-merge-aliases [env config f t]
  {:pre [(map? env)
         (symbol? f)
         (alias? t)]
   :post [(map? env)]}
  ;(prn "Try merging" f
  ;     "with" (:name t))
  (let [tks (keysets env t)
        fks (keysets env (-alias f))]
    ;(prn "merging keysets?"
    ;     tks fks)
    (cond
      ;; if there's some subset of keysets that are
      ;; identical in both, collapse the entire thing.
      ;; TODO is this too aggresssive? Shouldn't the keysets
      ;; be exactly identical?
      (and (some (fn [fk]
                   (some (fn [tk]
                           (or
                             (seq (set/intersection (set/union (:req-keyset fk)
                                                               (:opt-keyset fk))
                                                    (:req-keyset tk)))
                             (seq (set/intersection (set/union (:req-keyset tk)
                                                               (:opt-keyset tk))
                                                    (:req-keyset fk)))))
                         tks))
                 fks)
           (not (alias? (resolve-alias env (-alias f))))
           (not (alias? (resolve-alias env t))))
      (let [;_ (prn "Merging" f
            ;       "with" (:name t)
            ;       "with intersection" (set/intersection tks fks))
            ]
        (update-alias-env env
                          (fn [m]
                            (-> m 
                                (assoc f t)
                                (update (:name t)
                                        (fn [oldt]
                                          {:pre [(type? oldt)]}
                                          (let [new-type (join
                                                           (get m f)
                                                           (subst-alias oldt (-alias f) t))]
                                            ;(prn "new-type" (unparse-type new-type))
                                            new-type)))))))

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
            ;; find all keysets for downstream (and upstream) aliases
            ;; and merge.
            env (if-not (done t)
                  (reduce 
                    (fn [env f]
                      (try-merge-aliases env config f t))
                    env
                    (set 
                      (concat
                        (fv env (resolve-alias env t))
                        ;; also try and merge with parents
                        (map :name done))))
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

; inline-alias? : Env Config Alias -> Bool
(defn inline-alias? [env config a]
  {:pre [(alias? a)]}
  (boolean
    (and (:simplify? config)
         (if (:spec? config)
           ;; namespaced aliases correspond to `s/keys` entries
           ;; and must be preserved
           (not (namespace (:name a)))
           true)
         (let [a-res (resolve-alias env a)
               res (not (contains? (set (fv env a-res true)) (:name a)))]
           ;(prn "inline-alias?" (:name a) res)
           res))))

; follow-aliases-in-type : Env Config Type -> Type
(defn follow-aliases-in-type [env config t]
  (reduce
    (fn [t f]
      {:pre [(symbol? f)]}
      ;(prn "Follow" f)
      (if (and (:spec? config)
               (namespace f))
        t
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

              :else (let [substt 
                          ;; if the new alias is simple, just inline.
                          (if (inline-alias? env config real)
                            real-res
                            real)]
                      (subst-alias t (-alias f) substt)))))))
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
              (cond
                ;; infinite loop, give up
                (seen real) env

                (alias? real-res)
                (recur real-res (conj seen real))

                :else (let [substt
                            ;; if the new alias is simple, just inline.
                            (if (inline-alias? env config real)
                              real-res
                              real)]
                        (register-alias env config
                                        f
                                        (subst-alias
                                          (resolve-alias env (-alias f))
                                          (-alias inner)
                                          substt)))))))
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
                      ;(prn "squash" a)
                      (squash env config a))
                    env (map -alias fvs))]
    [t env]))

(declare generate-tenv)

(defmulti wrap-dispatch
  "A wrapper for code dispatch that prints local keywords with ::"
  {:arglists '[[object]]}
  class)

(defmethod wrap-dispatch :default
  [o]
  (pp/code-dispatch o))

;;; (def pprint-map (formatter-out "~<{~;~@{~<~w~^ ~_~w~:>~^, ~_~}~;}~:>"))
(defn- pprint-map [amap]
	(pp/pprint-logical-block :prefix "{" :suffix "}"
		(pp/print-length-loop [aseq (seq amap)]
			(when aseq
				(pp/pprint-logical-block
					(pp/write-out (ffirst aseq))
					(.write ^java.io.Writer *out* " ")
					(pp/pprint-newline :linear)
					(.set #'pp/*current-length* 0) ; always print both parts of the [k v] pair
					(pp/write-out (fnext (first aseq))))
				(when (next aseq)
					(.write ^java.io.Writer *out* ", ")
					(pp/pprint-newline :linear)
					(recur (next aseq)))))))

;; deterministic printing of HMaps
(defmethod wrap-dispatch clojure.lang.IPersistentMap
  [o]
  (let [{tagged true untagged false}
        (group-by (fn [[k v]]
                    (and (seq? v)
                         (= 'quote (first v))
                         (keyword? (second v))))
                  o)
        tagged   (sort-by first tagged)
        untagged (sort-by first untagged)
        ordered
        (apply array-map
               (concat
                 (mapcat identity tagged)
                 (mapcat identity untagged)))]
    (pprint-map ordered)))

(defmethod wrap-dispatch clojure.lang.Keyword
  [kw]
  (let [aliases (ns-aliases (current-ns))
        some-alias (delay
                     (some (fn [[k v]]
                             (when (= (namespace kw)
                                      (str (ns-name v)))
                               k))
                           aliases))]
    (cond
      (= (name (current-ns)) (namespace kw))
      (print (str "::" (name kw)))

      @some-alias 
      (print (str "::" @some-alias "/" (name kw)))

      :else
      (print kw))))

(defn pprint [& args]
  (pp/with-pprint-dispatch wrap-dispatch
    (apply pp/pprint args)))

(defn pprint-str-no-line [& args]
  (binding [pp/*print-right-margin* nil]
    ;; remove trailing newline
    (let [s (with-out-str
              (apply pprint args))]
      (subs s 0 (dec (count s))))))

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

(defn path-root [path]
  (-> path first :name))

(defn extend-paths [paths extension]
  (into #{}
        (map (fn [path]
               (conj path extension)))
        paths))

(def ^:dynamic *should-track* true)

(def ^:const apply-realize-limit 20)

(def ^:dynamic *track-depth* nil #_5)
(def ^:dynamic *track-count* nil #_5)
(def ^:dynamic *root-results* nil #_5)

(def stored-call-ids (atom {}))

(defn gen-call-id [paths]
  [paths (swap! stored-call-ids update paths (fnil inc 0))])

(declare track)

(pot/def-map-type PersistentMapProxy [^clojure.lang.IPersistentMap m k-to-track-info config results-atom 
                                      ;; if started as HMap tracking map, map from kw->Type
                                      ;; for all keyword keys with keyword values
                                      current-kw-entries-types
                                      current-ks current-all-kws?]
  Object
  (toString [this] (.toString m))
  (equals [this obj] (.equals m obj))

  clojure.lang.Counted
  (count [this] (count m))

  ;; TODO (.seq this), .iterator, .vals
  java.util.Map
  (size [this] (.size ^java.util.Map m))
  (containsKey [this obj] (.containsKey ^java.util.Map m obj))

  (equiv [this obj]
    (.equiv m obj))

  (get [this key default-value] (if (contains? m key)
                                  (let [v (get m key)
                                        track-infos (get k-to-track-info key)]
                                    (if (empty? track-infos)
                                      ;; this entry has no relation to paths
                                      v
                                      (let [{:keys [paths call-ids]}
                                            (binding [*should-track* false]
                                              (reduce (fn [acc [{:keys [ks kw-entries-types all-kws?] :as track-info}
                                                                {:keys [paths call-ids]}]]
                                                        {:pre [(boolean? all-kws?)
                                                               (set? ks)
                                                               (map? kw-entries-types)
                                                               (set? paths)
                                                               (set? call-ids)]}
                                                        (let [path-extension (if (and (keyword? key)
                                                                                      all-kws?)
                                                                               ;; HMap tracking
                                                                               (key-path kw-entries-types ks key)
                                                                               ;; homogeneous map tracking
                                                                               ;; FIXME what about map-keys-path tracking?
                                                                               (map-vals-path))]
                                                          (-> acc 
                                                              (update :call-ids into call-ids)
                                                              (update :paths into (extend-paths paths path-extension)))))
                                                      {:paths #{}
                                                       :call-ids #{}}
                                                      track-infos))]
                                        (track config results-atom v paths call-ids))))
                                  default-value))
  (assoc [this key value] (PersistentMapProxy. (assoc m key value)
                                               ;; new value has no relation to paths
                                               (dissoc k-to-track-info key)
                                               config
                                               results-atom
                                               (if (and (keyword? key)
                                                        (keyword? value))
                                                 (assoc current-kw-entries-types key (-val value))
                                                 current-kw-entries-types)
                                               (conj current-ks key)
                                               (and current-all-kws?
                                                    (keyword? key))))
  (dissoc [this key] (PersistentMapProxy. (dissoc m key)
                                          ;; new value has no relation to paths
                                          (dissoc k-to-track-info key)
                                          config
                                          results-atom
                                          (dissoc current-kw-entries-types key)
                                          (disj current-ks key)
                                          (or current-all-kws?
                                              ;; might have deleted the last non-keyword key
                                              (every? keyword? (disj current-ks key)))))
  ;; TODO wrap
  (keys [this] (keys m))
  ;; TODO vals
  (meta [this] (meta m))
  (hashCode [this] (.hashCode ^Object m))
  (hasheq [this] (.hasheq ^clojure.lang.IHashEq m))
  (with-meta [this meta] (PersistentMapProxy. (with-meta m meta)
                                              k-to-track-info
                                              config
                                              results-atom
                                              current-kw-entries-types
                                              current-ks
                                              current-all-kws?)))

(defn unwrap-value [v]
  (if-some [[_ u] (or (-> v meta (find ::unwrapped-fn))
                      (-> v meta (find ::unwrapped-seq))
                      (when (instance? PersistentMapProxy v)
                        [nil (.m ^PersistentMapProxy v)]))]
    ;; values are only wrapped one level, no recursion calls needed
    u
    v))

; track : (Atom InferResultEnv) Value Path -> Value
(defn track 
  ([{:keys [track-depth track-count track-strategy] :as config} results-atom v paths call-ids]
   {:pre [((con/set-c? vector?) paths)
          (seq paths)
          ((con/set-c? vector?) call-ids)]}
   (let [_ (let [hs ((juxt 
                       #(System/identityHashCode %)
                       class)
                     (unwrap-value v))]
             ;(prn "call-ids" (map (comp #(map (comp :name first) %) first) call-ids))
             (swap! results-atom update :call-flows
                    (fn [flows]
                      (reduce (fn [flows call-id]
                                (reduce (fn [flows path]
                                          (let [vname (-> path first :name)
                                                _ (assert (symbol? vname))]
                                            (update-in flows [vname call-id]
                                                       (fn [m]
                                                         (-> m
                                                             (update-in [:path-hashes path] (fnil conj #{}) hs)
                                                             (update-in [:hash-occurrences hs] (fnil conj #{}) path))))))
                                        flows
                                        paths))
                              flows
                              call-ids))))]
     (cond
       ((some-fn keyword? nil? false?) v)
       (do
         (add-infer-results! results-atom (infer-results paths (-val v)))
         v)

       ;; cut off path
       (or
         (when-let [track-depth track-depth]
           (> (apply min (map count paths)) track-depth))
         (not *should-track*))
       ;(debug
       ;  (println "Cut off inference at path "
       ;           (unparse-path path)
       ;           "(due to " (if *should-track*
       ;                        (str "track depth of" *track-depth*
       ;                             "being exceeded")
       ;                        (str "disabled tracking of internal ops"))
       ;           ")")
         (let [;; record as unknown so this doesn't
               ;; cut off actually recursive types.
               _ (add-infer-results! results-atom (infer-results paths {:op :unknown}))]
           (unwrap-value v))
       ;)

       ;; only accurate up to 20 arguments.
       ;; all arities 21 and over will collapse into one.
       (fn? v) (let [[paths unwrapped-fn] (if (-> v meta ::wrapped-fn?)
                                            ((juxt ::paths ::unwrapped-fn) 
                                             ;; combine paths
                                             (update (meta v) ::paths into paths))
                                            [paths v])
                     _ (assert (set? paths))
                     ;; Now, remember this value is at least a function, in case it is never invoked.
                     ;; This will get noted redundantly for older paths, if that's
                     ;; some kind of issue, we should remember which paths we've already noted.
                     _ (add-infer-results! results-atom (infer-results paths (-class clojure.lang.IFn [])))
                     call-ids (conj call-ids (gen-call-id paths))
                     ;; space efficient function wrapping
                     wrap-fn (fn [paths unwrapped-fn]
                               (with-meta
                                 (fn [& args]
                                   (let [blen (bounded-count apply-realize-limit args) ;; apply only realises 20 places
                                         _ (when (= 0 blen)
                                             (track config results-atom 
                                                    -any ;ignored, just noting this is called with 0-args
                                                    (extend-paths paths (fn-dom-path 0 -1))
                                                    call-ids))
                                         ;; here we throw away arities after 20 places.
                                         ;; no concrete reason for this other than it feeling like a sensible
                                         ;; compromise.
                                         args (map-indexed
                                                (fn [n arg]
                                                  (if (< n blen)
                                                    (track config results-atom arg
                                                           (extend-paths paths (fn-dom-path blen n))
                                                           call-ids)
                                                    arg))
                                                args)]
                                     (track config results-atom (apply unwrapped-fn args)
                                            (extend-paths paths (fn-rng-path blen))
                                            call-ids)))
                                 (merge (meta unwrapped-fn)
                                        {::wrapped-fn? true
                                         ::paths paths
                                         ::unwrapped-fn unwrapped-fn})))]
                 (wrap-fn paths v))

       (list? v)
       (let []
         (when (empty? v)
           (add-infer-results!
             results-atom
             (infer-results paths
                           (-class clojure.lang.IPersistentList [(make-Union #{})]))))
         (let [res 
               (with-meta
                 (apply list
                        (map (fn [e]
                               (track config results-atom e (extend-paths paths (seq-entry))
                                      call-ids))
                             v))
                 (meta v))]
           (assert (list? res))
           res))

       (and (seq? v)
            (not (list? v)))
       (let [[paths unwrapped-seq paths-where-original-coll-could-be-empty]
             (if (-> v meta ::wrapped-seq?)
               ((juxt ::paths ::unwrapped-seq ::paths-where-original-coll-could-be-empty)
                ;; combine paths
                (-> (meta v)
                    (update ::paths into paths)
                    (update ::paths-where-original-coll-could-be-empty into paths)))
               [paths v paths])
             _ (assert (set? paths))
             ;; space efficient wrapping
             wrap-lseq 
             (fn wrap-lseq [unwrapped-seq paths-where-original-coll-could-be-empty]
               (with-meta
                 (lazy-seq
                   (if (empty? unwrapped-seq)
                     (let []
                       (when (seq paths-where-original-coll-could-be-empty)
                         (add-infer-results!
                           results-atom
                           (infer-results
                             paths-where-original-coll-could-be-empty
                             (-class clojure.lang.ISeq [(make-Union #{})]))))
                       unwrapped-seq)
                     (cons (track config results-atom
                                  (first unwrapped-seq)
                                  (extend-paths paths (seq-entry))
                                  call-ids)
                           (wrap-lseq (rest unwrapped-seq)
                                      ;; collection can no longer be empty for these paths
                                      #{}))))
                 (merge (meta unwrapped-seq)
                        {::wrapped-seq? true
                         ::paths-where-original-coll-could-be-empty paths-where-original-coll-could-be-empty
                         ::paths paths
                         ::unwrapped-seq unwrapped-seq})))]
         (wrap-lseq unwrapped-seq paths-where-original-coll-could-be-empty))

       (instance? clojure.lang.ITransientVector v)
       (let [cnt (count v)]
         (reduce
           (fn [v i]
             (let [e (nth v i)
                   e' (track config results-atom e
                             (extend-paths paths (transient-vector-entry))
                             call-ids)]
               (if (identical? e e')
                 v
                 (binding [*should-track* false]
                   (assoc! v i e')))))
           v
           (range cnt)))

       ;; cover map entries
       (and (vector? v) 
            (= 2 (count v)))
       (let [k  (track config results-atom (nth v 0) (extend-paths paths (index-path 2 0)) call-ids)
             vl (track config results-atom (nth v 1) (extend-paths paths (index-path 2 1)) call-ids)]
         (assoc v 0 k 1 vl))

       (and (vector? v) 
            (satisfies? clojure.core.protocols/IKVReduce v)) ; MapEntry's are not IKVReduce
       (let [heterogeneous? (<= (count v) 4)
             len (count v)
             so-far (atom 0)]
         (when (= 0 len)
           (add-infer-results! results-atom (infer-results paths (-class clojure.lang.IPersistentVector [{:op :union :types #{}}]))))
         (reduce-kv
           (fn [e k v]
             (swap! so-far inc)
             (let [v' (track config results-atom v (extend-paths 
                                                     paths 
                                                     (if heterogeneous?
                                                       (index-path len k)
                                                       (vec-entry-path)))
                             call-ids)]
               (cond
                 (when-let [tc track-count]
                   (< tc @so-far)) 
                 (reduced (binding [*should-track* false]
                            (assoc e k v')))

                 (identical? v v') e
                 :else
                 (binding [*should-track* false]
                   (assoc e k v')))))
           v
           v))

       (set? v)
       (do
         (when (empty? v)
           (add-infer-results!
             results-atom
             (infer-results paths
                           (-class clojure.lang.IPersistentSet
                                   [{:op :union :types #{}}]))))
         ;; preserve sorted sets
         (binding [*should-track* false]
           (into (empty v)
                 (map (fn [e]
                        (binding [*should-track* true]
                          (track config results-atom e (extend-paths paths (set-entry))
                                 call-ids))))
                 v)))

       (instance? PersistentMapProxy v)
       (let [^PersistentMapProxy v v
             ks (.current-ks v)
             _ (assert (set? ks))
             all-kws? (.current-all-kws? v)
             _ (assert (boolean? all-kws?))
             kw-entries-types (.current-kw-entries-types v)
             _ (assert (map? kw-entries-types))
             track-info {:all-kws? all-kws?
                         :ks ks
                         :kw-entries-types kw-entries-types}]
         ;; TODO do we update the config/results-atom? What if they're different than the proxy's?
         (PersistentMapProxy. (.m v)
                              (reduce (fn [m k]
                                        (update-in m [k track-info]
                                                   #(merge-with (fnil into #{})
                                                                %
                                                                {:paths paths
                                                                 :call-ids call-ids})))
                                      (.k-to-track-info v)
                                      ;; FIXME we should remove known kw entries
                                      ks)
                              (.config v)
                              (.results-atom v)
                              (.current-kw-entries-types v)
                              (.current-ks v)
                              (.current-all-kws? v)))

       (or (instance? clojure.lang.PersistentHashMap v)
           (instance? clojure.lang.PersistentArrayMap v)
           (instance? clojure.lang.PersistentTreeMap v))
       (let [ks (set (keys v))]
         (when (empty? v)
           (add-infer-results!
             results-atom
             (infer-results paths
                           (-class clojure.lang.IPersistentMap
                                   [{:op :union :types #{}}
                                    {:op :union :types #{}}]))))
         (cond
           (every? keyword? ks)
           (let [{with-kw-val true
                  no-kw-val false}
                 (binding [*should-track* false]
                   (group-by (fn [e]
                               (keyword? (val e)))
                             v))
                 kw-entries-types
                 (into {}
                       (map (fn [[k v]]
                              {:pre [(keyword? v)]}
                              [k (-val v)]))
                       with-kw-val)
                 ;; we rely on the no-kw-val map to
                 ;; track the simple keyword entries -- if there
                 ;; are none, just pick one of the kw-entries-types
                 ;; and track it.
                 _ (when (and (empty? no-kw-val)
                              (seq kw-entries-types))
                     (let [k (key (first kw-entries-types))]
                       (track config results-atom (get v k)
                              (binding [*should-track* false]
                                (extend-paths paths (key-path kw-entries-types ks k)))
                              call-ids)))
                 ]
             (cond
               (= track-strategy :lazy)
               (PersistentMapProxy. v
                                    (zipmap (apply disj ks with-kw-val)
                                            (repeat {{:all-kws? true
                                                      :kw-entries-types kw-entries-types
                                                      :ks ks}
                                                     {:paths paths
                                                      :call-ids call-ids}}))
                                    config
                                    results-atom
                                    kw-entries-types
                                    ks
                                    true)
               :else
               (reduce
                 (fn [m [k orig-v]]
                   (let [v (track config results-atom orig-v
                                  (binding [*should-track* false]
                                    (extend-paths paths (key-path kw-entries-types ks k)))
                                  call-ids)]
                     (cond
                       ;; only assoc if needed
                       (identical? v orig-v) m

                       :else
                       (binding [*should-track* false]
                         (assoc m k v)))))
                 v
                 no-kw-val)))

           :else
           (let [so-far (atom 0)]
             (cond
               (= track-strategy :lazy)
               (PersistentMapProxy. v
                                    (zipmap ks (repeat {{:all-kws? false
                                                         :kw-entries-types {}
                                                         :ks ks}
                                                        {:paths paths
                                                         :call-ids call-ids}}))
                                    config
                                    results-atom
                                    {}
                                    ks
                                    false)
               :else
               (reduce
                 (fn [m k]
                   (swap! so-far inc)
                   (let [orig-v (get m k)
                         [new-k v] 
                         (cond
                           ;; We don't want to pollute the HMap-req-ks with
                           ;; non keywords (yet), disable.
                           ;(keyword? k)
                           ;[k (track config results-atom orig-v
                           ;          (binding [*should-track* false]
                           ;            (extend-paths paths (key-path {} ks k))))]

                           :else 
                           [(track config results-atom k
                                   (binding [*should-track* false]
                                     (extend-paths paths (map-keys-path)))
                                   call-ids)
                            (track config results-atom orig-v
                                   (binding [*should-track* false]
                                     (extend-paths paths (map-vals-path)))
                                   call-ids)])]
                     (cond
                       ; cut off homogeneous map
                       (when-let [tc *track-count*]
                         (< tc @so-far))
                       (reduced
                         (binding [*should-track* false]
                           (-> m
                               ;; ensure we replace the key
                               (dissoc k)
                               (assoc new-k v))))

                       ;; only assoc if needed
                       (identical? v orig-v) m

                       ;; make sure we replace the key
                       (not (identical? new-k k))
                       (binding [*should-track* false]
                         (-> m
                             (dissoc k)
                             (assoc new-k v)))

                       :else
                       (binding [*should-track* false]
                         (assoc m new-k v)))))
                 v
                 (keys v))))))

        (instance? clojure.lang.IAtom v)
        (let [old-val (-> v meta ::t/old-val)
              new-paths (binding [*should-track* false]
                         (extend-paths paths (atom-contents)))
              should-track? (binding [*should-track* false]
                              (not= @v old-val))
              _ (when should-track?
                  (track config results-atom @v new-paths
                         call-ids))
              #_#_
              _ (binding [*should-track* false]
                  (add-watch
                    v
                    new-paths
                    (fn [_ _ _ new]
                      (binding [*should-track* true]
                        (track config results-atom new new-paths
                               call-ids)))))]
          v)

       :else (do
               (add-infer-results! results-atom (infer-results paths (-class (class v) [])))
               v)))))

(def prim-invoke-interfaces
  (into #{}
        (->>
          (map (fn [ss] (apply str ss))
               (apply concat
                      (for [n (range 1 6)]
                        (apply comb/cartesian-product (repeat n [\D \O \L])))))
          (remove (fn [ss]
                    (every? #{\O} ss))))))

(defn char->tag [c]
  {:pre [(char? c)]
   :post [(symbol? %)]}
  (case c
    \L 'long
    \D 'double
    \O 'java.lang.Object))

(defn tag->char [t]
  {:pre [((some-fn nil? symbol?) t)]
   :post [(char? %)]}
  (case t
    long \L
    double \D
    \O))

(defn gen-prim-invokes [f-this prims]
  ;(prn "gen-prim-invokes" prims)
  (mapcat
    (fn [p]
      {:pre [(string? p)]}
      (let [args (into []
                       (map-indexed
                         (fn [n c]
                           (-> (symbol (str "arg" n))
                               #_(vary-meta 
                                 assoc :tag (char->tag c)))))
                       (butlast p))
            interface (symbol (str "clojure.lang.IFn$" p))
            rettag (char->tag (nth p (dec (count p))))
            ;_ (prn "rettag" rettag)
            this (gensym 'this)
            argvec (-> (vec (cons this args))
                       #_(vary-meta assoc :tag rettag))]
        #_
        (binding [*print-meta* true]
          (prn "argvec" argvec))
        [interface
         (list 'invokePrim argvec
               `(~(f-this this) ~@(map #(with-meta % nil) args)))]))
    prims))

(defn gen-nonvariadic-invokes [f-this]
  (for [arity (range 0 20),
        :let [args (repeatedly arity gensym)
              this (gensym 'this)]]
    `(~'invoke [~this ~@args]
       (~(f-this this) ~@args))))

(defn gen-variadic-invoke [f-this]
  (let [args (repeatedly 21 gensym)
        this (gensym 'this)]
    `(~'invoke [~this ~@args] (apply ~(f-this this) ~@args))))

(defn gen-apply-to [f-this]
  (let [this (gensym 'this)]
    `(~'applyTo [~this args#] (apply ~(f-this this) args#))))

(defn extend-IFn [f-this prims]
  `(clojure.lang.IFn
    ~@(gen-nonvariadic-invokes f-this)
    ~(gen-variadic-invoke f-this)
    ~(gen-apply-to f-this)
    ~@(gen-prim-invokes f-this prims)))

(defmacro deftypefn
  "Like deftype, but accepts a function f before any specs that is
  used to implement clojure.lang.IFn.  f should accept at least one
  argument, 'this'."
  [name prims & opts+specs]
  (let [field 'f
        f-this (fn [this]
                 (list '. this (symbol (str "-" field))))
        source `(deftype ~name [~field]
                  ~@(extend-IFn f-this prims)
                  ~@opts+specs)]
    #_
    (binding [*print-meta* true]
      (pprint source))
    source))

(def this-ns *ns*)

(defn arglist-prim-string [args]
  {:pre [(vector? args)]
   :post [((some-fn nil? string?) %)]}
  (let [s (apply str
            (concat
              (->> args
                   (map (comp :tag meta))
                   (map tag->char))
              [(tag->char (-> args meta :tag))]))]
    (when (prim-invoke-interfaces s)
      s)))

(defn wrap-prim [vr f]
  {:pre [(var? vr)]}
  ;(prn "wrap-prim" vr)
  (let [prim-arglists 
        (sort
          (->> (-> vr meta :arglists)
               (map arglist-prim-string)
               (filter string?)))]
    (cond
      (seq prim-arglists)
      (let [type-name (symbol
                        (str "PrimFn"
                             (apply str
                                    (interpose
                                      "_"
                                      prim-arglists))))
            ;_ (prn "type-name" type-name)
            cls (or #_(ns-resolve this-ns type-name)
                    (binding [*ns* this-ns]
                      (eval
                        `(deftypefn ~type-name ~prim-arglists))))
            _ (assert (class? cls))
            ctor (ns-resolve this-ns 
                             (symbol
                               (str "->" type-name)))
            _ (assert (var? ctor))]
        (ctor f))
      
      :else f)))

(defn gen-track-config []
  (merge 
    {:track-strategy :lazy
     :track-depth *track-depth*
     :track-count *track-count*
     :root-results *root-results*}
    vs/*instrument-infer-config*))

; track-var : (IFn [Var -> Value] [(Atom Result) Var Sym -> Value])
(defn track-var'
  ([vr] (track-var' (gen-track-config) results-atom vr *ns*))
  ([config vr] (track-var' config results-atom vr *ns*))
  ([config results-atom vr ns]
   {:pre [(var? vr)
          (instance? clojure.lang.IAtom results-atom)]}
   ;(prn "tracking" vr "in ns" ns)
   (wrap-prim
     vr
     (track config
            results-atom @vr #{[(var-path
                                  (ns-name (the-ns ns))
                                  (impl/var->symbol vr))]}
            #{}))))

(defmacro track-var [v]
  `(track-var' (var ~v)))

; track-def-init : Sym Sym Value -> Value
(defn track-def-init [config vsym ns val]
  {:pre [(symbol? vsym)
         (namespace vsym)]}
  ;(prn "track-def-init")
  (let [v (ns-resolve ns vsym)]
    ;(prn v)
    (wrap-prim
      v
      (track config
             results-atom val 
             #{[{:op :var
                 :ns (ns-name ns)
                 :name vsym}]}
             #{}))))

(defn track-local-fn [config track-kind line column end-line end-column ns val]
  {:pre [(#{:local-fn :loop-var} track-kind)]}
  #_
  (prn "track-local-fn" 
       (symbol
         (str (ns-name ns)
              "|"
              line
              "|"
              column
              "|"
              end-line
              "|"
              end-column)))
  (track config
         results-atom val 
         #{[{:op :var
             ::track-kind track-kind
             :line line
             :column column
             :end-line end-line
             :end-column end-column
             :ns (ns-name ns)
             :name (with-meta
                     (symbol
                       (str (ns-name ns)
                            "|"
                            line
                            "|"
                            column
                            "|"
                            end-line
                            "|"
                            end-column))
                     {::track-kind track-kind
                      :line line
                      :column column
                      :end-line end-line
                      :end-column end-column
                      :ns (ns-name ns)})}]}
         #{}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Analysis compiler pass
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ns-exclusions : (Set Sym)
(def ns-exclusions
  #{'clojure.core
    spec-ns
    (if (= spec-ns 'clojure.spec)
      'clojure.spec.gen
      'clojure.spec.gen.alpha)
    'clojure.core.typed
    'clojure.core.typed.contract
    'clojure.core.typed.current-impl
    'clojure.test
    'clojure.string})

(defn dummy-do [env statements ret]
  {:pre [((some-fn nil? vector) statements)]}
  {:op :do
   :form '(do)
   :env env
   :statements statements
   :ret ret
   :children [:statements :ret]})

(defn dummy-let [env bindings body]
  {:op :let
   :form '(let)
   :env env
   :bindings bindings
   :body (assoc body :body? true)
   :children [:bindings :body]})

; dummy-sym : Env Sym -> TAExpr
(defn dummy-sym [env vsym]
  {:pre [(symbol? vsym)]}
  {:op :const
   :type :symbol
   :form `'~vsym
   :env env
   :val vsym})

(defn dummy-kw [env kw]
  {:pre [(keyword? kw)]}
  {:op :const
   :type :keyword
   :form kw
   :env env
   :val kw})

(defn dummy-num [env n]
  {:op :const
   :type :number
   :form n
   :env env
   :val n})

(defn dummy-const-map [env m]
  ; if we need something more specific, think more carefully
  ; about whether this is a :const node, maybe with a :quote
  ; surrounding it?
  {:pre [(every? keyword? (keys m))
         (every? (some-fn nil? number? boolean? keyword? symbol? string?) (vals m))]}
  {:op :const
   :type :map
   :form m
   :env env
   :val m})

; wrap-var-deref : TAExpr Sym Namespace -> TAExpr
(defn wrap-var-deref [{:keys [env] :as expr} vsym var-ns]
  ;(prn "wrap-var-deref")
  (let [var-nsym (ns-name var-ns)
        _ (println
            (str "Instrumenting " vsym " in " var-nsym
                 #_":" 
                 #_(-> expr :env :line)
                 #_(when-let [col (-> expr :env :column)]
                     ":" col)))
        invoke-ast {:op :invoke 
                    :meta (let [^Class cls (-> (:var expr) meta :tag)
                                tag (if (class? cls) (.getName cls) cls)]
                            {:tag tag})
                    :children [:fn :args]
                    :form `(track-var' (var ~vsym))
                    :env env
                    :fn {:op :var
                         :var #'track-var'
                         :form `track-var'
                         :env env}
                    :args [(dummy-const-map env (gen-track-config))
                           {:op :var
                            :form `results-atom
                            :env env
                            :var #'results-atom}
                           {:op :the-var
                            :form `(var ~vsym)
                            :env env
                            :var (:var expr)}
                           (dummy-sym env var-nsym)]}
        ]
    invoke-ast))

; wrap-def-init : TAExpr Sym Namespace -> TAExpr
(defn wrap-def-init [{:keys [env] :as expr} vsym def-ns]
  ;(prn ((juxt identity class) (-> expr :env :ns)))
  (let [def-nsym (ns-name def-ns)]
    (println
      (str "Instrumenting def init " vsym " in " def-nsym
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
     :args [(dummy-const-map env (gen-track-config))
            (dummy-sym (:env expr) vsym)
            (dummy-sym (:env expr) (:ns (:env expr)))
            expr]}))

(defn wrap-local-fn [track-kind coord {:keys [env] :as expr} fn-ns]
  {:pre [(#{:local-fn :loop-var} track-kind)]}
  (let [nsym (ns-name fn-ns)]
    (println (str "Instrumenting local fn in " nsym
                  " "
                  (:line coord)
                  ":"
                  (:column coord)
                  #_":" 
                  #_(-> expr :env :line)
                  #_(when-let [col (-> expr :env :column)]
                      ":" col)))
    {:op :invoke
     :children [:fn :args]
     :form `(track-local-fn ~(:form expr))
     :env env
     :fn {:op :var
          :var #'track-local-fn
          :form `track-local-fn
          :env env}
     :args [(dummy-const-map env (gen-track-config))
            (dummy-kw env  track-kind)
            (dummy-num env (:line coord))
            (dummy-num env (:column coord))
            (dummy-num env (:end-line coord))
            (dummy-num env (:end-column coord))
            (dummy-sym env nsym)
            expr]}))

(def ^:dynamic *found-fn* false)
(def alternative-arglists (atom {}))

(defn infer-arglists [v expr]
  {:pre [(var? v)]}
  (letfn [(arglists-for [expr]
            (case (:op expr)
              :var (or (-> (:var expr) meta :arglists)
                       (get @alternative-arglists (coerce/var->symbol (:var expr))))
              :fn (let [frm (emit-form/emit-form expr)]
                    (@#'clojure.core/sigs (next frm)))
              :with-meta (arglists-for (:expr expr))
              :do (arglists-for (:ret expr))
              (:let :body :letfn :loop) (arglists-for (:body expr))
              nil))]
    (let [arglists (arglists-for expr)]
      (when arglists
        (swap! alternative-arglists assoc (coerce/var->symbol v) arglists)))
    nil))

;; Only wrap library imports so we can infer how they are used.
;; Also wrap :dynamic vars since they can be rebound at runtime
;; and lose instrumentation.
(defn should-wrap-var? [v]
  (let [vsym (impl/var->symbol v)
        vns (symbol (namespace vsym))
        excluded? (contains? (conj ns-exclusions (ns-name *ns*)) vns)
        ;dynamic? (-> (:var expr) meta :dynamic)
        no-infer? (-> v meta ::t/no-infer)
        should-infer? (-> v meta ::t/infer)
        should-wrap? (or should-infer? (not (or excluded? no-infer?)))]
    ;(prn "should-wrap-var?" v should-infer? excluded? no-infer? should-wrap?)
    should-wrap?))

(defn wrap-var-expr [expr]
  (if (should-wrap-var? (:var expr))
    (wrap-var-deref expr (impl/var->symbol (:var expr)) *ns*)
    expr))

; check : (IFn [TAExpr -> TAExpr] [TAExpr CTType -> TAExpr]
(defn check
  "Assumes collect-expr is already called on this AST."
  ([expr expected] (check expr))
  ([expr]
   (let [_ (when-let [load-state vs/*typed-load-atom*]
             ;(prn "load-state" (::refreshed? @load-state))
             (when-not (::refreshed? @load-state)
               ;; delete all local fn info for the current namespace, since it's likely to go out of date
               ;; as soon as the file is edited.
               (let [;tracks-lambda? (fn [r]
                     ;                 (let [fpth (first (:path r))]
                     ;                   (assert (#{:var} (:op fpth)))
                     ;                   (boolean
                     ;                     (and (:local-fn fpth)
                     ;                          (= (:ns fpth) (current-ns))))))
                     for-current-ns? (fn [r]
                                       (let [fpth (first (:path r))]
                                         (assert (#{:var} (:op fpth)))
                                         (= (:ns fpth) (current-ns))))]
                 (swap-infer-results! results-atom 
                                      (fn [rs]
                                        (into #{}
                                              (remove for-current-ns?)
                                              rs)))
                 (swap! load-state assoc ::refreshed? true))))
         skip-track? (fn [coord]
                       (or
                         (not (:line coord))
                         (not (:column coord))
                         (and (= (:line coord) (:end-line coord))
                              ;; would these be off by one?
                              (= (:column coord) (:end-column coord)))))]
     (case (:op expr)
       ;; never rewrite a target of an assignment, Compiler.java gets angry
       ;; TODO if `should-wrap-var?` of the target, track the rhs
       :set! (update expr :val check)
       ;; Wrap def's so we can instrument their usages outside this
       ;; namespace.
       :def (let [v (:var expr)
                  _ (assert ((some-fn nil? var?) v))
                  no-infer? (some-> v meta ::t/no-infer)
                     ]
              (when no-infer?
                (prn "no-infer" (ast/def-var-name expr)))
              (if (and (:init expr)
                       (not no-infer?))
                (update expr :init 
                        (fn [init]
                          (when-not (-> v meta :arglists)
                            (infer-arglists v init))
                          (-> init
                              check
                              (wrap-def-init (ast/def-var-name expr) *ns*))))
                expr))
       :invoke (cond
                 (and (= :var (-> expr :fn :op))
                      (should-wrap-var? (-> expr :fn :var)))
                 (let [wrapped (ast/walk-children check expr)
                       ^Class fclass (-> expr :fn :var meta :tag)
                       ftag (if (class? fclass)
                              (.getName fclass)
                              fclass)]
                   ;(prn ":invoke wrap")
                   ;; if we track var v, rewrite (v ...) to ^{:tag ~(-> meta v :tag)} (v ...)
                   (if ftag
                     (update-in wrapped [:meta :tag] (fnil identity ftag))
                     wrapped))

                 :else (ast/walk-children check expr))
       :var (wrap-var-expr expr)
       :fn 
       (if (not *found-fn*)
         ;; this is a top-level function that's already being wrapped
         (binding [*found-fn* true]
           (ast/walk-children check expr))
         ;; instrument this
         (let [out (ast/walk-children check expr)
               coord (meta (:form out))
               ]
           (if (or (skip-track? coord)
                   ;; don't track if there are no arguments
                   (every? (comp empty? :params) (:methods out)))
             out
             (wrap-local-fn :local-fn coord out *ns*))))

       ;; FIXME get this to play nicely with type hints + auto-boxing
       #_#_
       :loop
       ;; track each loop argument
       (let [expr (ast/walk-children check expr)
             ;; loops don't have their metadata preserved, so
             ;; we grab the coordinates of the bindings.
             coords (map (comp meta :form) (:bindings expr))
             ]
         (if (or (every? skip-track? coords)
                 ;; don't track if there are no loop variables
                 (empty? (:bindings expr)))
           expr
           (let [{:keys [body bindings]} expr]
             ;(prn "rewriting")
             (assoc expr
                    :body
                    (assoc
                      (dummy-do
                        (:env body)
                        []
                        (dummy-let
                          (:env body)
                          ;; here, we're rebinding loop variables as let variable.
                          ;; This means the :local annotations in body will be out
                          ;; of date, but since we're just immediately evaluating
                          ;; this AST tree, it shouldn't matter.
                          (vec
                            (keep
                              (fn [b]
                                ;; only track the loop bindings that have file coordinates.
                                (let [coord (meta (:form b))]
                                  (when-not (skip-track? coord)
                                    (assoc b
                                           :init 
                                           (wrap-local-fn
                                             :loop-var
                                             coord
                                             (-> b
                                                 (assoc :op :local)
                                                 (dissoc :init :atom))
                                             *ns*)))))
                              bindings))
                          ;; the :env for body is now out of date.
                          body))
                      :body? true)))))

       :let
       ;; track rhs's depending on metadata.
       (let [expr (ast/walk-children check expr)
             bindings
             (mapv (fn [b]
                     (let [m (-> b :form meta)]
                       ;(prn "let binding" m)
                       (or
                         (when-let [coord (::t/auto-ann m)]
                           ;(prn ":let coord" coord
                           ;     (skip-track? coord)
                           ;     (::t/track-kind m))
                           (when-not (skip-track? coord)
                             (case (::t/track-kind m)
                               (::t/for-return ::t/for-param)
                               (assoc b
                                      :init
                                      (wrap-local-fn
                                        ;; loop-var has identical behaviour as what we
                                        ;; want for inserting `for` annotations
                                        :loop-var
                                        coord
                                        (:init b)
                                        *ns*)))))
                         b)))
                   (:bindings expr))]
         (assoc expr
                :bindings bindings))

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

(defn group-by-path
  ([infer-results] (group-by-path 0 infer-results))
  ([path-index infer-results]
   {:pre [(integer? path-index)]}
   ;(prn "group-by-path" path-index)
   (let [[current-group ps]
         (-> (group-by (comp #(nth % path-index ::current-group) :path) infer-results)
             ((juxt ::current-group #(dissoc % ::current-group))))]
     {:current-level (set (map :type current-group))
      :inner-level (apply merge {}
                          (map (fn [[pth-elem rs]]
                                 {:pre [(keyword? (:op pth-elem))]}
                                 {pth-elem (group-by-path (inc path-index) rs)})
                               ps))})))

(declare pprint-env local-fn-symbol? macro-symbol?)

(defn polymorphic-function-reports [config call-flows]
  (doseq [[vname call-ids] call-flows
          :when (not ((some-fn local-fn-symbol?
                               macro-symbol?)
                      vname))]
    (prn "Polymorphic report for" vname)
    (prn "N calls" (count call-ids))
    (doseq [[_ {:keys [:path-hashes :hash-occurrences]}] call-ids]
      ;(doseq [[path hss] path-hashes])
      (doseq [[path hs] path-hashes]
        (let [related-paths (apply concat (vals (select-keys hash-occurrences hs)))
              _ (prn "hashes" hs)
              paths (into #{path} related-paths)]
          (when (< 1 (count paths))
            (let [x (gensym 'x)
                  infer-results (map #(infer-result % {:op :free :name x}) paths)
                  by-path (group-by-path infer-results)]
              (binding [*preserve-unknown* true]
                (pprint-env 
                  (as-> (init-env) env 
                    (update-env env assoc :type-env 
                                (grouped-paths-to-env env config by-path)))
                  (assoc config :spec? false)))))))
      ;(prn "")
      ;(prn "Path" (unparse-path path) (count hss))
      #_(prn "N paths" (count path-hashes))
      #_(prn "N hashes" (count hash-occurrences)))))


; generate-tenv : InferResultEnv -> AliasTypeEnv
(defn generate-tenv
  "Reset and populate global type environment."
  [env config {:keys [infer-results :call-flows] :as is}]
  (println "generate-tenv:"
                  (str (count infer-results) " infer-results"))
  (let [by-path (group-by-path infer-results)
        ;; filter results
        #_#_infer-results (if *root-results*
                        (let [root-count (atom {})
                              _ (println "Filtering results...")
                              infer-results (reduce (fn [infer-results {:keys [path] :as res}]
                                                      (let [;; to get a good spread of annotations, pick
                                                            ;; *root-results* number of infer-results from
                                                            ;; each (possibly nested) function position.
                                                            ;; This way, we avoid annotations where any one
                                                            ;; position hogs
                                                            path-fn-prefix (cons (path-root path)
                                                                                 (take-while
                                                                                   (comp #{:fn-domain :fn-range} :op)
                                                                                   (next path)))
                                                            _ (swap! root-count update path-fn-prefix (fnil inc 0))]
                                                        (if (>= *root-results* (get @root-count path-fn-prefix 0))
                                                          (conj infer-results res)
                                                          infer-results)))
                                                    #{}
                                                    infer-results)]
                          (println "generate-tenv:"
                                   "Filtered to"
                                   (str (count infer-results) " infer-results"))
                          infer-results)
                        infer-results)
        ]
    (as-> (init-env) env 
      (update-env env assoc :type-env (grouped-paths-to-env env config by-path)))))

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
                                  (let [singles (filter (comp #{:val} :op val) (::HMap-req t))]
                                    (when-let [[k v] (and (= (count singles) 1)
                                                          (first singles))]
                                      (str k "-" (pr-str (:val v)))))
                                  nil)))
                   :HMaps
                   (fn [n]
                     (let [t (-> n meta :type)]
                       (case (:op t)
                         :HMap (apply str (interpose "-" (sort (map name (keys (::HMap-req t))))))
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

(defn reachable-aliases 
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

; Env AliasSym -> (Map Kw (Set KwVals))
;; gathers all the possible tagged entries under this
;; alias. Returns a map from keyword entries to a
;; set of all the tags found used under this alias.
(defn possibly-tagged-entries-in-alias [env a]
  {:post [(map? %)]}
  (let [deep-reqs (HMap-deep-reqs env (-alias a))
        combine-kw-vals
        (fn [reqs]
          (apply merge-with into
                 ;; only keep kw valued entries.
                 (sequence
                   (comp
                     (map (fn [req]
                            (into {}
                                  (comp
                                    (filter (comp kw-val? val))
                                    (map (fn [[k v]]
                                           {:post [(-> % second first keyword?)]}
                                           [k #{(:val v)}])))
                                  req)))
                     (filter seq))
                   reqs)))
        ]
    ;(prn "deep-reqs" deep-reqs)
    (or
      (combine-kw-vals deep-reqs)
      {})))

(defn relevant-alias [asym relevant-entries kw-reqs]
  ; ALIAS: KwValsMap => (Map Kw (Set Kw))
  ; relevant-entries : KwValsMap
  {:pre [(symbol? asym)
         (map? relevant-entries)
         (every? (fn [[k vs]]
                   (and (keyword? k)
                        (and (set? vs)
                             (every? keyword? vs))))
                 relevant-entries)]
   :post [((some-fn nil? symbol?) %)]}
  (let [all-other-kw-vals 
        (apply merge-with into (vals (dissoc kw-reqs asym)))
        ]
    ;; if any other alias contains exactly the
    ;; same keyword entry, don't merge
    ;; the current alias.
    ;; This avoids merging:
    ;; (defalias A '{:op ':foo, ...})
    ;; (defalias B '{:op ':bar, ...})
    (when (every? (fn [[k other-kws]]
                    (let [rel (get relevant-entries k)]
                      ;(prn "rel" rel)
                      ;(prn "other-kws" other-kws)
                      ;; only merge with another map if they
                      ;; have the same dispatch entry.
                      ;; Don't want to end up with '{:op (U ':and ':or ...)}.
                      (if-not rel
                        true
                        (and (= 1 (count rel))
                             (= 1 (count other-kws))
                             (= rel other-kws)))))
                  all-other-kw-vals)
      asym)))

(defn merge-aliases 
  "Join all the aliases `as`, then reuse a name in `as`
  to call this new type and add to alias env.
  
  Finally, point every `as` to this new alias name,
  in effect rewriting each `as`. This extra indirection
  is cleaned up in `follow-aliases`.
  
  This never adds a new alias name, only reuses
  ones in `as`."
  [env as]
  (letfn [(join*-aliases [env anew as]
            {:pre [(symbol? anew)
                   (every? symbol? as)]
             :post [(type? %)]}
            ;(prn "join*-aliases" anew as)
            (let [as (set as)
                  ts (map #(resolve-alias env (-alias %)) as)
                  ;_ (prn "ts with anew" (mapv unp ts))
                  ;; remove all top-level aliase refs to as, otherwise
                  ;; we'll create infinite aliases.
                  ts (map (fn remove-top-level-alias [t]
                            {:pre [(type? t)]
                             :post [(type? %)]}
                            (case (:op t)
                              :alias (if (contains? as (:name t))
                                       -nothing
                                       t)
                              :union (make-Union
                                       (map remove-top-level-alias (:types t)))
                              t))
                          ts)
                  ]
              ;(prn "before merge" (mapv unp ts))
              (apply join* ts)))
          ;; returns a pair
          ;; [<name to rewrite to> <all other aliases>]
          (rewrite-to [as]
            [(first as) (rest as)])
          ; returns env
          (point-old-aliases-to-new
            [env anew aolds]
            (reduce (fn [env aold]
                      (update-alias-env env assoc aold (-alias anew)))
                    env
                    aolds))]
    (let []
      ;; Are there any aliases left to merge?
      (if (>= 1 (count as))
        env
        (let [;; important invariant: we don't create new aliases,
              ;; only reuse ones in `as`
              [anew aolds] (rewrite-to as)
              newtyp (join*-aliases env anew as)
              ;; update new alias
              env (update-alias-env env assoc anew newtyp)
              ;; point old aliases to the new alias
              env (point-old-aliases-to-new env anew aolds)]
          env)))))

; Env (Coll Alias) -> (Map (Set Kw) (Set Alias))
(defn group-HMap-aliases-by-req-keyset [env as]
  (let [as (set as)]
    (apply merge-with into
           (->> (alias-env env)
                (filter (fn [[k t]]
                          (and (contains? as k)
                               (HMap? t))))
                (map (fn [[a t]]
                       {:pre [(HMap? t)]}
                       {(HMap-req-keyset t) #{a}}))))))

;; group HMap aliases by req keysets, but allow some differences
;; for optional entries
(defn group-similar-HMap-aliases-by-req-keysets [env as]
  (letfn [(remove-differently-tagged-aliases
            [env as]
            {:post [(vector? %)
                    (every? symbol? %)]}
            ; kw-reqs : (Map AliasSym (Map Kw (Set Kw)))
            (let [kw-reqs
                  (into {}
                        (map (fn [a]
                               [a (possibly-tagged-entries-in-alias env a)]))
                        as)]
              (into []
                    (keep (fn [[a relevant-entries]]
                            (relevant-alias a relevant-entries kw-reqs)))
                    kw-reqs)))
          ;; return all aliases that differ by one or
          ;; two entries.
          (find-similar-aliases
            [kset ksets]
            {:pre [(set? kset)
                   (map? ksets)]
             :post [(set? %)
                    (every? symbol? %)]}
            (let [;; max number of keys different allowable
                  ;; to qualify for merging
                  different-kset-thres
                  (cond
                    (< (count kset) 5) 1
                    (< (count kset) 10) 4
                    ;; too expensive? revisit.
                    (< 15 (count kset)) 0
                    :else 5)

                  possible-ksets
                  (reduce (fn [ksets n]
                            {:pre [(pos? n)]
                             :post [(set? %)
                                    (every? set? %)]}
                            (into
                              ksets
                              (map set (comb/combinations 
                                         (vec kset) 
                                         ;; no idea if this is correct, used to
                                         ;; be (- (count kset) n) but got arity
                                         ;; exception in comb/combinations
                                         (max
                                           different-kset-thres
                                           (- (count kset) n))))))
                          #{}
                          (range 1 (inc different-kset-thres)))]
              (reduce (fn [res kset]
                        (into res (get ksets kset)))
                      #{}
                      possible-ksets)))]
  (let [ksets (group-HMap-aliases-by-req-keyset env as)
        alias-groups (into []
                           (comp
                             (map (fn [[kset as]]
                                    (into as (find-similar-aliases kset ksets))))
                             (map #(remove-differently-tagged-aliases env %)))
                           ksets)
        ;_ (prn "similar aliases" as)
        ]
    alias-groups)))

(defn group-HMap-aliases-by-overlapping-qualified-keys [env as]
  (let [as (set as)
        overlapping (apply merge-with into
                           (->> (alias-env env)
                                (filter (fn [[k t]]
                                          (contains? as k)))
                                (mapcat (fn [[a t]]
                                          {:pre [(type? t)]}
                                          (let [hmap-qkeys (fn [hmap]
                                                             {:pre [(HMap? hmap)]}
                                                             (set
                                                               (filter namespace
                                                                       (concat (keys (::HMap-req hmap))
                                                                               (keys (::HMap-opt hmap))))))
                                                ]
                                          (case (:op t)
                                            :HMap (map (fn [qkey]
                                                         {qkey #{a}})
                                                       (hmap-qkeys t))
                                            :union
                                            (let [hmap-qkeys (->> (filter HMap? (:types t))
                                                                  (mapcat (fn [t]
                                                                            (map (fn [qkey]
                                                                                   {qkey #{a}})
                                                                                 (hmap-qkeys t)))))]
                                              hmap-qkeys)
                                            nil))))))]
    (prn "group-HMap-aliases-by-overlapping-qualified-keys"
         overlapping)
    (vals overlapping)))

(defn group-HMap-aliases-by-likely-tag* [env as]
  {:post [((con/hash-c?
             keyword? ; tag key
             (con/hash-c?
               keyword? ; tag value
               (con/set-c? symbol?) ; aliases
               ))
           %)]}
  (let [as (set as)
        hmap-aliases (->> (select-keys (alias-env env) as)
                          (filter (fn [[k t]]
                                    (HMap? t))))
        ;; map of aliases to sets of possible tag key/val pairs
        possible-tag-keys ;; :- (Map Alias (Set '[Kw Kw]))
        (into {}
              (comp
                (map (fn [[a t]]
                       {:pre [(HMap? t)]
                        :post [((con/hvector-c?
                                  symbol?
                                  (con/set-c?
                                    (con/hvector-c? keyword? keyword?)))
                                %)]}
                       [a (set (keep (fn [[k t]]
                                       {:post [((some-fn nil? (con/hvector-c? keyword? keyword?))
                                                %)]}
                                       (when (kw-val? t)
                                         [k (:val t)]))
                                     (::HMap-req t)))]))
                (filter (comp seq second)))
              hmap-aliases)
        all-tags (mapcat (fn [ts]
                           {:pre [((con/set-c?
                                    (con/hvector-c? keyword? keyword?))
                                   ts)]
                            :post [(every? keyword? %)]}
                           (map first ts))
                         (vals possible-tag-keys))
        _ (assert (every? keyword? all-tags))
        ;; frequency of tag keys
        ;; :- (Map Kw Int)
        tag-key-frequencies (let [freqs (frequencies all-tags)
                                  ;_ (prn "freqs" freqs)
                                  _ (assert ((con/hash-c? keyword? integer?) freqs))]
                              (fn [k]
                                {:pre [(keyword? k)]
                                 :post [(integer? %)]}
                                (freqs k)))
        ;; map from tag keys to maps of aliases mapped to their associated tag values
        ;; (defalias TagKey Kw)
        ;; (defalias TagVal Kw)
        ;; (defalias AliasSym Sym)
        ;; :- (Map TagKey (Map TagVal (Set AliasSym)))
        tag-keys-to-aliases
        (apply merge-with #(merge-with into %1 %2)
               {}
               (map (fn [[a ks-and-tags]]
                      {:pre [(symbol? a)
                             ((con/set-c?
                                (con/hvector-c? keyword? keyword?))
                              ks-and-tags)
                             (seq ks-and-tags)]
                       :post [((con/hash-c?
                                 keyword? ; tag key
                                 (con/hash-c?
                                   keyword? ; tag value
                                   (con/set-c? symbol?) ; aliases
                                   ))
                               %)]}
                      ;(prn "ks-and-tags" ks-and-tags)
                      (let [[freqk tag] (apply max-key (comp tag-key-frequencies first) ks-and-tags)]
                        (assert (keyword? freqk))
                        (assert (keyword? tag))
                        {freqk {tag #{a}}}))
                    possible-tag-keys))]
    tag-keys-to-aliases))

(defn group-HMap-aliases-by-likely-tag [env as]
  {:post [((con/vec-c? (con/set-c? symbol?))
           %)]}
  (let [tagk->tagv->HMap-as (group-HMap-aliases-by-likely-tag* env as)
        ;_ (prn "tagk->tagv->HMap-as" tagk->tagv->HMap-as)
        coll-of-tagv->HMap-as (vals (group-HMap-aliases-by-likely-tag* env as))
        ;_ (prn "coll-of-tagv->HMap-as" coll-of-tagv->HMap-as)
        HMap-as (mapcat vals coll-of-tagv->HMap-as)]
    ;(prn "HMap-as" HMap-as)
    (vec HMap-as)))

;; group all aliases by their probably tag key.
(defn group-aliases-by-likely-tag-key [env as]
  (let [tagk->tagv->HMap-as (group-HMap-aliases-by-likely-tag* env as)
        HMap-a->tagk (into {}
                           (mapcat 
                             (fn [[tagk tagv->HMap-as]]
                               (map (fn [HMap-a]
                                      {:pre [(symbol? HMap-a)]}
                                      [HMap-a tagk])
                                    (apply set/union (vals tagv->HMap-as)))))
                           tagk->tagv->HMap-as)
        find-alias-tag-keys (fn find-alias-tag-keys 
                              ([t] (find-alias-tag-keys t #{}))
                              ([t seen]
                              {:pre [(type? t)]
                               :post [((con/set-c? keyword?) %)]}
                              (let [;; we've already separated all HMap aliases.
                                   ; _ (assert (not (HMap? t)))
                                    ]
                                (case (:op t)
                                  :union (apply set/union 
                                                (map #(find-alias-tag-keys % seen) (:types t)))
                                  :alias (if-let [tagk (HMap-a->tagk (:name t))]
                                           #{tagk}
                                           (if (contains? seen (:name t))
                                             #{}
                                             (find-alias-tag-keys (resolve-alias env t)
                                                                  (conj seen (:name t)))))
                                  #{}))))
        tagk->HMap-as (apply merge-with into {}
                             (map (fn [[HMap-a tagk]]
                                    {tagk #{HMap-a}})
                                  HMap-a->tagk))
        ;; assign a tag key to every alias
        tagk->as (apply merge-with into {}
                        tagk->HMap-as
                        (keep (fn [a]
                                {:pre [(symbol? a)]
                                 :post [((some-fn
                                           nil?
                                           (con/hash-c? 
                                             keyword? ; tag key
                                             (con/set-c? symbol?) ; aliases with given tag key
                                             ))
                                         %)]}
                                (let [tagks (find-alias-tag-keys (-alias a))]
                                  (when (= 1 (count tagks))
                                    {(first tagks) #{a}})))
                              (apply disj as (keys HMap-a->tagk))))
        ]
    (vals tagk->as)))

(declare debug-output)

(defn rename-HMap-aliases [env config]
  (reduce (fn [env a]
            ;; don't rename aliases specifically crafted for `s/keys`
            (if (and (:spec? config)
                     (namespace a))
              (do
                ;(prn "Not renaming HMap alias" a)
                env)
              (let [t (get (alias-env env) a)
                    ;_ (prn "renaming HMap alias" a (unp t) (:op t))
                    _ (assert (type? t))
                    ;_ (debug-output (str "before rename-HMap-aliases " a) env config)
                    env (case (:op t)
                          :union (let [ts (:types t)
                                       every-hmap? (every? HMap? ts)]
                                   (if every-hmap?
                                     (let [k (HMap-likely-tag-key ts)]
                                       (if (every? #(HMap-has-tag-key? % k) ts)
                                         (let [new-a (gen-unique-alias-name env config (symbol (name k)))]
                                           (rename-alias env a new-a))
                                         env))
                                     env))
                          ;; this is not a tagged map
                          :HMap (let [relevant-names (map
                                                       camel-case
                                                       (concat
                                                         (sort
                                                           (map name (keys (::HMap-req t))))
                                                         (sort
                                                           (map name (keys (::HMap-opt t))))))
                                      a-new (gen-unique-alias-name
                                              env config
                                              (symbol
                                                (if (empty? relevant-names)
                                                  "EmptyMap"
                                                  (apply str (concat (take 3 relevant-names) ["Map"])))))]
                                  (rename-alias env a a-new))
                          env)]
                ;(debug-output (str "after rename-HMap-aliases " a) env config)
                env)))
          env
          (keys (alias-env env))))

(defn remove-unreachable-aliases [env config as]
  (update-alias-env env
                    (fn [aenv]
                      (into {}
                            (filter (fn [[k v]]
                                      ;; never remove namespaced keys, because of how kw-vals?
                                      ;; are accumulated wrt aliasing in spec.
                                      (or (when (:spec? config)
                                            (namespace k))
                                          (contains? as k))))
                            aenv))))

(defn alias-env-diff-removed [oldaenv newaenv] 
  (into #{} (filter namespace)
        (set/difference (set (keys (alias-env oldaenv)))
                        (set (keys (alias-env newaenv))))))

; Env Config -> Env
(defn squash-horizonally
  "Join aliases that refer to exactly
  one HMap with overlapping req keys.

  First we merge HMaps that have similar keysets.
  In this first stage, we are careful to not
  merge 'tagged' maps with different tags.
  
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

  Then we merge map aliases based on their
  likely tag. When multiple tag keys are possible,
  we choose the most frequent tag key from the alias env
  as a tie-breaker.

  Finally, we combine aliases with the same dispatch keys.
  "
  [env config]
  (let [as (reachable-aliases env)
        env (remove-unreachable-aliases env config as)

        ;; merge HMaps with similar keysets, excluding differently-tagged maps.
        asets (group-similar-HMap-aliases-by-req-keysets env as)
        ;; merge-aliases does not introduce new aliases, so `as`
        ;; is still the set of reachable aliases after this line.
        env (reduce merge-aliases env asets)

        ;; merge HMaps on their tags key/val pairs.
        asets (group-HMap-aliases-by-likely-tag env as)
        env (reduce merge-aliases env asets)

        ;; collect and group all HMaps with the
        ;; same tag key. This is pretty aggressive ---
        ;; it effectively upcasts any specific tagged
        ;; HMap to its "parent" (the union of all maps
        ;; with the same tag key).
        asets (group-aliases-by-likely-tag-key env as)
        env (reduce merge-aliases env asets)

        ;; merge HMaps on their tags key/val pairs.
        ;asets (group-HMap-aliases-by-overlapping-qualified-keys env as)
        ;env (reduce merge-aliases env asets)

        as (reachable-aliases env)
        ;; remove unreachable aliases
        env (remove-unreachable-aliases env config as)

        ;; delete intermediate aliases
        env (follow-all env (assoc config :simplify? false))

        ;_ (debug-output "before rename-HMap-aliases" env config)
        ;; rename aliases pointing to HMaps
        env (rename-HMap-aliases env config)
        ;_ (debug-output "after rename-HMap-aliases" env config)
        ]
    env))

(declare envs-to-annotations
         envs-to-specs)

(defn pprint-env [env {:keys [spec?] :as config}]
  (pprint ((if spec?
             envs-to-specs
             envs-to-annotations)
           env config)))

(defn debug-output [msg env config]
  (when (= :iterations *debug*)
    (println (str "ITERATION: " msg))
    (pprint-env env config)))

(defn dec-fuel [env]
  (if (contains? env :fuel)
    (update env :fuel dec)
    env))

(defn enough-fuel? [env]
  (if (contains? env :fuel)
    (< 0 (:fuel env))
    true))

(defn comment-form [& body]
  (list* (qualify-core-symbol 'comment)
         body))

(defn def-spec [k s]
  (list (qualify-spec-symbol 'def)
        k
        ; handle possibly recursive specs
        ; TODO intelligently order specs to minimize this issue
        (if (or (symbol? s)
                (set? s)
                ;; already late bound
                (and (seq? s)
                     (let [fs (first s)]
                       ((into #{}
                              (map qualify-spec-symbol)
                              ; late binding ops
                              '[and keys cat alt or nilable coll-of
                                fspec map-of tuple cat multi-spec
                                *])
                        fs))))
          s
          (list (qualify-spec-symbol 'and)
                s))))

(defmacro when-fuel [env & body]
  `(let [env# ~env]
     (if (enough-fuel? env#)
       (dec-fuel (do ~@body))
       env#)))

(defn squash-vertically [env config]
  (reduce
    (fn [env [v t]]
      (let [;; create graph nodes from HMap types
            [t env] (alias-hmap-type env config t)
            ;_ (debug-output (str "after alias-hmap-type for " v) env config)
            ;; squash local recursive types
            [t env] (squash-all env config t)
            ;_ (debug-output (str "after squash-all for" v) env config)
            ;; trim redundant aliases in local types
            [t env] (follow-aliases env (assoc config :simplify? true) t)
            ;_ (debug-output (str "after follow-aliases for" v) env config)
            ;_ (prn "new type for" v (unparse-spec t))
            ]
        (update-type-env env assoc v t)))
    env
    (type-env env)))

(defn populate-envs [env {:keys [spec? no-squash-vertically] :as config}]
  (debug "populate-envs:"
  (let [;; create recursive types
        env (if-let [fuel (:fuel config)]
              (assoc env :fuel fuel)
              env)
        env (if no-squash-vertically
              env
              (let [_ (debug-output "top of populate-envs" env config)
                    env (when-fuel env
                                   (squash-vertically env config))
                    _ (debug-output "after squash vertically" env config)]
                env))
        ;; ensure all HMaps correspond to an alias
        env (when-fuel env
              (alias-single-HMaps env config))
           _ (debug-output "after alias-single-HMaps" env config)
        ;; merge aliases that point to HMaps
        ;; with the same keys (they must point to *exactly*
        ;; one top-level HMap, not a union etc.)
        env (when-fuel env
              (squash-horizonally env config))
        _ (println "finished squash-horizonally")
         _ (debug-output "after squash-horizonally" env config)
         ;; Clean up redundant aliases and inline simple
         ;; type aliases.
         _ (println "Start follow-all")
         env (when-fuel env
               (follow-all env (assoc config :simplify? false)))
         _ (println "end follow-all")
         _ (debug-output "after follow-all" env config)
         _ (println "start remove unreachable aliases")
         env (when-fuel env
               (let [as (reachable-aliases env)]
                 (remove-unreachable-aliases env config as)))
         _ (println "end remove unreachable aliases")
        ]
    (println "done populating")
    env)))

;(defn order-defaliases [env as]
;  (let [afvs (into {}
;                   (map (fn [[k v]]
;                          [k (set (fv env v))]))
;                   as)]
;    ))

(defn local-fn-symbol? [s]
  (= :local-fn (::track-kind (meta s))))

(defn loop-var-symbol? [s]
  (= :loop-var (::track-kind (meta s))))

(defn macro-symbol? [s]
  {:pre [(symbol? s)]}
  (boolean
    (when (namespace s)
      (when-let [v (find-var s)]
        (:macro (meta v))))))

(defn imported-symbol? [s]
  {:pre [(symbol? s)]}
  (not= (str (ns-name (current-ns)))
        (namespace s)))

(defn envs-to-specs [env {:keys [spec-macros] :as config}]
  ;(prn "envs-to-specs" (keys (alias-env env)))
  (binding [*envs* (atom env)]
    (let [should-spec-macros? (boolean spec-macros)
          tenv (into {}
                     (remove (fn [[k _]]
                               (or ;; don't spec local functions
                                    (local-fn-symbol? k)
                                    ;; macro specs are opt-in
                                    (if should-spec-macros?
                                      false
                                      (macro-symbol? k))
                                    ;; don't spec external functions
                                    (imported-symbol? k))))
                     (type-env env))
          get-spec-form (fn [spec-name]
                          (some-> spec-name
                                  ((impl/v (symbol (str spec-ns) "get-spec")))
                                  ((impl/v (symbol (str spec-ns) "form")))))
          ;_ (prn (keys tenv))
          aliases-generated (atom #{})
          prep-alias-map (fn [a-needed a-used]
                           {:pre [(map? a-needed)
                                  (set? a-used)]
                            :post [(map? %)]}
                           (into a-needed
                                 (map (fn [a]
                                        {:pre [(symbol? a)]}
                                        #_(prn "resolve-alias" a)
                                        [a (or (resolve-alias-or-nil @*envs* (-alias a))
                                               -any)]))
                                 a-used))
          gen-aliases (fn gen-aliases 
                        [as {:keys [just-in-time?] :as opt}]
                        {:pre [(map? as)]}
                        (into []
                          (mapcat (fn [[a v]]
                                    {:pre [((some-fn symbol? keyword?) a)]}
                                    (when-not (@aliases-generated a)
                                      (swap! aliases-generated conj a)
                                      (let [aliases-needed (atom {})
                                            multispecs-needed (atom #{})
                                            used-aliases (atom #{})
                                            unparse-spec (fn [s]
                                                           (binding [*spec-aliases* aliases-needed
                                                                     *used-aliases* used-aliases
                                                                     *multispecs-needed* multispecs-needed]
                                                             (unparse-spec s)))
                                            ;; side effects bindings
                                            s (unparse-spec v)
                                            spec-name (if (keyword? a)
                                                        a
                                                        (alias->spec-kw a))
                                            old-spec (get-spec-form spec-name)
                                            current-spec (def-spec spec-name s)
                                            out-specs (cond
                                                        just-in-time?
                                                        [(comment-form
                                                          "Generated just in time, not recursively traversed"
                                                          current-spec)]

                                                        :else
                                                        (concat
                                                          (when (and old-spec (:spec-diff? config))
                                                            [(comment-form
                                                               "Existing spec:"
                                                               (def-spec spec-name old-spec))])
                                                          [current-spec]))]
                                        (into (vec
                                                (concat
                                                  (apply concat @multispecs-needed)
                                                  (gen-aliases
                                                    (prep-alias-map @aliases-needed @used-aliases)
                                                    opt)))
                                              out-specs)))))
                          as))
          qualified-spec-aliases (atom {})
          top-level-types
          (binding [*qualified-spec-aliases* qualified-spec-aliases]
            (into []
                  (comp (filter (fn [[k v]]
                                  ;; only output fdef's. spec seems to assume all
                                  ;; top level def's are functions, which breaks spec instrumentation.
                                  ;; We work around this behaviour by simply
                                  ;; omitting non-function specs.
                                  (let [should-emit-spec? (boolean (#{:IFn} (:op v)))]
                                    should-emit-spec?)))
                        (mapcat (fn [[k v]]
                                  (let [aliases-needed (atom {})
                                        used-aliases (atom #{})
                                        multispecs-needed (atom #{})
                                        unparse-spec' (fn [s]
                                                        (binding [*spec-aliases* aliases-needed
                                                                  *used-aliases* used-aliases
                                                                  *multispecs-needed* multispecs-needed]
                                                          (unparse-spec s)))
                                        s (unparse-spec' (assoc v :top-level-def k))
                                        sym (if (= (namespace k)
                                                   (str (ns-name (current-ns))))
                                              ;; defs
                                              (symbol (name k))
                                              ;; imports
                                              k)
                                        old-spec (get-spec-form k)
                                        spec-to-maybe-fdef 
                                        (fn [sym s]
                                          (if (and (seq? s)
                                                   (#{(qualify-spec-symbol 'fspec)}
                                                    (first s)))
                                            (list*-force (qualify-spec-symbol 'fdef)
                                                         sym
                                                         (next s))
                                            (def-spec sym s)))
                                        sdef (spec-to-maybe-fdef sym s)
                                        prefix (vec
                                                 (concat
                                                   (apply concat @multispecs-needed)
                                                   (gen-aliases
                                                     (prep-alias-map
                                                       @aliases-needed
                                                       @used-aliases)
                                                     {})))
                                        suffix (concat
                                                 (when (and old-spec (:spec-diff? config))
                                                   [(comment-form
                                                      "Existing spec:"
                                                      (list (binding [*verbose-specs* true]
                                                              (qualify-spec-symbol 'def))
                                                            sym old-spec))])
                                                 [sdef])]
                                    (into prefix suffix)))))
              (sort-by first tenv)))

          top-level-types (vec (concat
                                 ;; FIXME what if this call refines qualified alias specs?
                                 ;; seems like a potential infinite loop if we try and support
                                 ;; this and we're not careful.
                                 (gen-aliases @qualified-spec-aliases {:just-in-time? true})
                                 top-level-types))
          ]
      {:top-level top-level-types
       :requires (when-let [requires (:explicit-require-needed config)]
                   [requires])})))

(def ^:dynamic *new-aliases* nil)

(defn envs-to-annotations [env {:keys [no-local-ann? polymorphic? :call-flows] :as config}]
  (let [full-type-env (type-env env)
        local-fn-env (if no-local-ann?
                       {}
                       (into {}
                             (filter (comp (some-fn loop-var-symbol?
                                                    local-fn-symbol?)
                                           key))
                             full-type-env))
        ;_ (prn "local-fn-env" local-fn-env)
        tenv (into {}
                   (remove (fn [[k v]]
                             {:pre [(symbol? k)]}
                             (let [s (if (namespace k)
                                       (symbol (str k))
                                       (symbol (str (current-ns)) (str k)))]
                               #_
                               (prn "metas" k
                                    (symbol (str (current-ns)) (str (name k)))
                                    (meta (find-var (symbol (str (current-ns)) (str (name k))))))
                               (or
                                 ;; don't annotate macros
                                 (some-> (find-var s)
                                         meta
                                         :macro)
                                 ;; don't annotate local functions
                                 (local-fn-symbol? k)
                                 ;; or loop variables
                                 (loop-var-symbol? k)))))
                   full-type-env)
        tfvs (into #{}
                   (mapcat
                     (fn [t]
                       (fv env t true)))
                   (concat (vals tenv)
                           (vals local-fn-env)))
        as (into {}
                 (filter (comp tfvs key))
                 (alias-env env))]
    (binding [*envs* (atom env)
              ;; disable for now
              *new-aliases* nil #_(atom (init-env))]
      (letfn [(unp-defalias-env [as]
                (mapv (fn [[k v]]
                        (list (qualify-typed-symbol 'defalias)
                              (symbol (name k))
                              (unparse-type v)))
                      (sort-by (comp name first) as)))
              (declares-for-aliases [as]
                (list* (if (= (ns-resolve (current-ns) 'declare)
                              #'clojure.core/declare)
                         'declare
                         'clojure.core/declare)
                       (sort (map (comp symbol name key) as))))
              (unp-anns [tenv]
                (vec
                  (mapcat (fn [[k v]]
                            (concat
                              [(list (qualify-typed-symbol 'ann)
                                     (if (= (namespace k)
                                            (str (ns-name (current-ns))))
                                       ;; defs
                                       (symbol (name k))
                                       ;; imports
                                       k)
                                     (unparse-type (assoc v :top-level-def k)))]
                              (when polymorphic?
                                (prn "polymorphic?" polymorphic? k)
                                (let [call-ids (get call-flows k)]
                                  ;(prn "count path-hashes" (count path-hashes))
                                  (apply concat 
                                    (for [[_ {:keys [:path-hashes :hash-occurrences]}] call-ids
                                          [path hs] path-hashes]
                                      (let [related-paths (apply concat (vals (select-keys hash-occurrences hs)))
                                            paths (into #{path} related-paths)]
                                        (prn "paths" (count paths))
                                        (when (< 1 (count paths))
                                          (let [x 'x
                                                infer-results (map #(infer-result % {:op :free :name x}) paths)
                                                by-path (group-by-path infer-results)
                                                v (->
                                                    (binding [*preserve-unknown* true]
                                                      (as-> (init-env) env 
                                                        (update-env env assoc :type-env 
                                                                    (grouped-paths-to-env env config by-path))))
                                                    type-env
                                                    (get k))
                                                _ (assert (type? v))]
                                            [(list 
                                               (qualify-core-symbol 'comment)
                                               (list (qualify-typed-symbol 'ann)
                                                     (if (= (namespace k)
                                                            (str (ns-name (current-ns))))
                                                       ;; defs
                                                       (symbol (name k))
                                                       ;; imports
                                                       k)
                                                     (list (qualify-typed-symbol 'All)
                                                           [x]
                                                           (binding [*preserve-unknown* true]
                                                             (unparse-type (assoc v :top-level-def k))))))])))))))))
                          (sort-by first tenv))))]
        (let [declares (declares-for-aliases as)
              defaliases (unp-defalias-env as)
              anns (unp-anns tenv)]
          {:requires (when-let [requires (:explicit-require-needed config)]
                       [requires])
           :top-level
           (into [declares] 
                 (concat defaliases anns))
           :local-fns (mapv (fn [[k v]]
                              (assoc (meta k) :type v))
                            local-fn-env)})))))


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

; Env Type -> (Vec Sym)
(defn fv
  "Returns the aliases referred in this type, in order of
  discovery. If recur? is true, also find aliases
  referred by other aliases found."
  ([env v] (fv env v false #{}))
  ([env v recur?] (fv env v recur? #{}))
  ([env v recur? seen-alias]
   {:pre [(map? env)
          (type? v)]
    :post [(vector? %)
           (every? symbol? %)]}
   ;(prn "fv" v)
   (let [fv (fn 
              ([v] (fv env v recur? seen-alias))
              ([v recur? seen-alias]
               (fv env v recur? seen-alias)))
         fvs   (case (:op v)
                 (:free :Top :unknown :val) []
                 :HMap (into []
                             (mapcat fv)
                             (concat
                               (-> v ::HMap-req vals)
                               (-> v ::HMap-opt vals)))
                 :HVec (into []
                             (mapcat fv)
                             (-> v :vec))
                 :union (into []
                              (mapcat fv)
                              (-> v :types))
                 (:unresolved-class :class)
                 (into []
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
                            (:arities v)))]
   fvs)))

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

;; adapted from tools.namespace
(defn update-file
  "Reads file as a string, calls f on the string plus any args, then
  writes out return value of f as the new contents of file, or writes
  content to `out`."
  [file ^String out f & args]
  {:pre [(instance? java.net.URL file)
         ((some-fn nil? string?) out)]}
  (let [old (slurp file)
        new (str (apply f old args))
        _ (when out
            (let [leading-slash? (boolean (#{\/} (first out)))
                  dirs (apply str (interpose "/" (pop (str/split out #"/"))))
                  dirs (if leading-slash?
                         (str "/" dirs)
                         dirs)
                  ;_ (prn "creating" dirs)
                  _ (doto (java.io.File. ^String dirs)
                      .mkdirs)]
              out))
        out (or out file)]
    (spit out new)
    (println "Output annotations to " out)))

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

(def ^:dynamic *indentation* 2)

(defn split-at-column 
  ([s column] (split-at-column s column nil))
  ([s column end-column]
   (let [before (subs s 0 (dec column))
         after  (if end-column
                  (subs s (dec column) (dec end-column))
                  (subs s (dec column)))]
     [before after])))

;; returns a pair [leading-first-line file-slice trailing-final-line]
(defn extract-file-slice [ls line column end-line end-column]
  (let [;_ (prn "ls" (count ls) (dec line) end-line)
        v (subvec ls (dec line) end-line)
        first-line (nth v 0)
        last-line (peek v)
        ;_ (prn "last-line" last-line (dec end-column))
        [before-column after-column] (split-at-column first-line column 
                                                      (when (= line end-line)
                                                        end-column))
        [before-end-column after-end-column] (split-at-column last-line end-column)
        ]
    [before-column
     (if (= 1 (count v))
       (assoc v
              0 after-column)
       (assoc v
              0 after-column
              (dec (count v)) before-end-column))
     after-end-column]))

(defn restitch-ls [ls line end-line split]
  (vec (concat
         (subvec ls 0 (dec line))
         split
         (subvec ls end-line))))

(defn insert-loop-var [{:keys [line column end-line end-column] :as f} ls]
  {:pre [(#{:loop-var} (::track-kind f))
         #_(= line end-line)
         #_(< column end-column)
         ]}
  (let [end-line line
        end-column column
        [leading file-slice trailing] (extract-file-slice ls line column end-line end-column)
        ;_ (prn "leading" leading) 
        ;_ (prn "file-slice" file-slice) 
        ;_ (prn "trailing" trailing)
        the-ann (binding [*print-length* nil
                          *print-level* nil]
                  (with-out-str 
                    ;(print "^")
                    ;(print (pprint-str-no-line ::t/rt-gen))
                    ;(print " ")
                    (print "^{")
                    (print (pprint-str-no-line ::t/ann))
                    (print " ")
                    (print (pprint-str-no-line (unparse-type (:type f))))
                    (print "} ")))
        [full-first-line
         offset-first-line]
        (if (> (count leading) 0)
          (let [extra-columns (atom 0)
                last-char (nth leading (dec (count leading)))]
            [(str leading 
                  (when-not (#{\[ \space} last-char)
                    (swap! extra-columns inc)
                    " ")
                  the-ann)
             (+ @extra-columns (count the-ann))])
          [the-ann (count the-ann)])
        ; FIXME this should always be [""], but it adds a useless new line
        ;file-slice
        _ (assert (every? #{""} file-slice)
                  file-slice)
        final-split [(str full-first-line trailing)]
        new-ls (restitch-ls ls line end-line final-split)
        update-line (fn [old-line]
                      ;; we never add a new line
                      old-line)
        update-column (fn [old-column old-line]
                        (cond
                          ;; changes in the current line. Compensate
                          ;; for the type annotation.
                          (and (= old-line line)
                               (< column old-column))
                          (+ old-column offset-first-line)
                          ;; we preserve columns since we don't add
                          ;; extra indentation.
                          :else old-column))]
    {:ls new-ls
     :update-line update-line
     :update-column update-column}))


(defn insert-local-fn* [{:keys [line column end-line end-column] :as f} ls]
  {:pre [(#{:local-fn} (::track-kind f))]}
  (let [;_ (prn "current fn" f)
        [before-first-pos file-slice trailing] (extract-file-slice ls line column end-line end-column)
        ;_ (prn "before-first-pos" before-first-pos) 
        ;_ (prn "file-slice" file-slice) 
        ;_ (prn "trailing" trailing)
        after-first-pos (nth file-slice 0)
        ;_ (prn "after-first-pos" after-first-pos)
        before-line (str
                      before-first-pos
                      (binding [*print-length* nil
                                *print-level* nil]
                        (with-out-str 
                          ;; DON'T DELETE THESE PRINTS
                          (print "(")
                          ;(print (str "^" (pprint-str-no-line ::t/auto-gen) " "))
                          (print (pprint-str-no-line (qualify-typed-symbol 'ann-form))))))
        indentation *indentation*
        indentation-spaces (apply str (repeat (+ (dec column) indentation) " "))
        ;; insert column+indentation spaces
        the-fn-line (str indentation-spaces after-first-pos)

        rest-slice (if (= 1 (count file-slice))
                     []
                     (subvec file-slice 1 (count file-slice)))

        ;; indent each line at column
        indented-fn (map (fn [a]
                           {:pre [(string? a)]}
                           ;; insert indentation at column if there's already whitespace there
                           (if (= \space (nth a (dec column)))
                             (let [;_ (prn "indenting" a)
                                   ;_ (prn "left half " (subs a 0 (dec column)))
                                   ;_ (prn "right half" (subs a (dec column)))
                                   ]
                               (str (subs a 0 column)
                                    (apply str (repeat indentation " "))
                                    (subs a column)))
                             (do
                               (prn (str
                                      "WARNING: Not indenting line " line
                                      " of " (:ns f) ", found non-whitespace "
                                      " at column " column "."))
                               a)))
                         rest-slice)
        ;_ (prn "the type pp" (pprint-str-no-line (unparse-type (:type f))))
        the-type-line (str indentation-spaces
                           (pprint-str-no-line (unparse-type (:type f)))
                           ")")
        ;; now add any trailing code after end-column
        ;; eg. (map (fn ...) c) ==> (map (ann-form (fn ...) ...)
        ;;                               c)
        trailing-line (when (not= 0 (count trailing))
                        (str (apply str (repeat (dec column) " "))
                             ;; TODO compensate for this change in update-column
                             (if nil #_(= \space (nth trailing 0))
                               (subs trailing 1)
                               trailing)))

        final-split (concat
                      [before-line
                       the-fn-line]
                      indented-fn
                      [the-type-line]
                      (when trailing-line
                        [trailing-line]))
        new-ls (restitch-ls ls line end-line final-split)
        update-line (fn [old-line]
                      (cond
                        ;; occurs before the current changes
                        (< old-line line) old-line
                        ;; occurs inside the bounds of the current function.
                        ;; Since we've added an extra line before this function (the beginning ann-form)
                        ;; we increment the line.
                        (<= line old-line end-line) (inc old-line)
                        ;; occurs after the current function.
                        ;; We've added possibly 2-3 lines: 
                        ;; - the beginning of the ann-form
                        ;; - the end of the ann-form
                        ;; - possibly, the trailing code
                        :else (if trailing-line
                                (+ 3 old-line)
                                (+ 2 old-line))))
        update-column (fn [old-column old-line]
                        (cond
                          ;; occurs before the current changes
                          (< old-line line) old-column
                          ;; occurs inside the bounds of the current function.
                          ;; We indent each of these lines by 2.
                          ;; WARNING: we might not have indented here
                          (<= line old-line end-line) (+ 2 old-column)
                          :else old-column))]
  {:ls new-ls
   :update-line update-line
   :update-column update-column}))

(defn insert-local-fns [local-fns old config]
  {:post [(string? %)]}
  ;(prn "insert-local-fns" local-fns)
  (let [update-coords
        (fn [update-line update-column]
          ;; adjust the coordinates of any functions that have moved.
          (fn [v]
            (-> v
                (update :line update-line)
                (update :end-line update-line)
                ;; pass original line
                (update :column update-column (:line v))
                ;; pass original end-line
                (update :end-column update-column (:end-line v)))))
        ;; reverse
        sorted-fns (sort-by (juxt :line :column) local-fns)
        ls (with-open [pbr (java.io.BufferedReader.
                             (java.io.StringReader. old))]
             (vec (doall (line-seq pbr))))]
    ;(prn "top ls" (count ls))
    (loop [ls ls
           fns sorted-fns]
      ;(prn "current ls")
      ;(println (str/join "\n" ls))
      (if (empty? fns)
        (str/join "\n" ls)
        (let [;; assume these coordinates are correct
              f (first fns)
              ;_ (prn "current f" f)
              {:keys [ls update-line update-column]}
              (case (::track-kind f)
                :local-fn (insert-local-fn* f ls)
                :loop-var (insert-loop-var f ls))
              _ (assert (vector? ls))
              _ (assert (fn? update-line))
              _ (assert (fn? update-column))
              next-fns (map 
                         ;; adjust the coordinates of any functions that have moved.
                         (update-coords update-line update-column)
                         (next fns))]
          (recur ls
                 next-fns))))))

(comment
  (println
    (insert-local-fns
      [{::track-kind :local-fn
        :line 1 :column 1
        :end-line 1 :end-column 11
        :type {:op :Top}}]
      "(fn [a] a)"
      {}))
  (println
    (insert-local-fns
      [{::track-kind :local-fn
        :line 1 :column 1
        :end-line 2 :end-column 5
        :type {:op :Top}}]
      "(fn [a]\n  a) foo"
      {}))
  (println
    (insert-local-fns
      [{::track-kind :local-fn
        :line 1 :column 3
        :end-line 2 :end-column 7
        :type {:op :Top}}]
      "  (fn [a]\n    a) foo"
      {}))
  (println
    (insert-local-fns
      [{::track-kind :local-fn
        :line 1 :column 1
        :end-line 1 :end-column 20
        :type {:op :Top}}
       {::track-kind :local-fn
        :line 1 :column 9
        :end-line 1 :end-column 19
        :type {:op :Top}}]
      "(fn [b] (fn [a] a))"
      {}))
  )

(declare prepare-ann infer-anns)

(defn insert-generated-annotations-in-str
  "Insert annotations after ns form."
  [old ns {:keys [replace-top-level? no-local-ann?] :as config}]
  {:pre [(string? old)]
   :post [(string? %)]}
  ;(prn "insert" ann-str)
  (binding [*ns* (the-ns ns)
            *ann-for-ns* #(the-ns ns)]
    (let [{:keys [requires top-level local-fns] :as as} (infer-anns ns config)
          ann-str (prepare-ann requires top-level config)
          _ (assert (string? ann-str))
          old (if no-local-ann?
                old
                (insert-local-fns local-fns old config))
          old (delete-generated-annotations-in-str old)
          insert-after (ns-end-line old)]
      (with-open [pbr (java.io.BufferedReader.
                        (java.io.StringReader. old))]
        (loop [ls (line-seq pbr)
               current-line 0
               out []]
          (if (= current-line insert-after)
            (str/join "\n" (concat out 
                                   [(first ls)
                                    ;""
                                    ann-str]
                                   (rest ls)))
            (if (seq ls)
              (recur (next ls)
                     (inc current-line)
                     (conj out (first ls)))
              (str/join "\n" (concat out 
                                     [""
                                      ann-str])))))))))
    


(defn delete-generated-annotations [ns config]
  (impl/with-clojure-impl
    (update-file (ns-file-name (if (symbol? ns)
                                 ns ;; avoid `the-ns` call in case ns does not exist yet.
                                 (ns-name ns)))
                 nil
                 delete-generated-annotations-in-str)))

(declare infer-anns)

(defn prepare-ann [requires top-level config]
  {:post [(string? %)]}
  (binding [*print-length* nil
            *print-level* nil]
    (with-out-str
      ;; print requires outside start/end annotations so we don't
      ;; delete them between runs
      (when (seq requires)
        (println ";; Automatically added requires by core.typed")
        (doseq [[n a] requires]
          (pprint (list (qualify-core-symbol 'require) `'[~n :as ~a]))))
      (println generate-ann-start)
      (doseq [a top-level]
        (pprint a))
      (print generate-ann-end))))

(defn default-out-dir [{:keys [spec?] :as config}]
  (let [cp-root (-> "" java.io.File. .getAbsoluteFile .getPath)
        dir-name (str "generated-" (if spec? "spec" "type") "-annotations")]
    (str cp-root "/" dir-name)))

(defn insert-or-replace-generated-annotations [ns {:keys [out-dir] :as config}]
  (impl/with-clojure-impl
    (let [nsym (ns-name ns)
          ^java.net.URL
          file-in (ns-file-name nsym)]
      (update-file file-in
                   (when (or out-dir 
                             (not= "file" (.getProtocol file-in)))
                     (str (or out-dir
                              (default-out-dir config))
                          "/" 
                          (coerce/ns->file nsym)))
                   insert-generated-annotations-in-str
                   ns
                   config))))

(defn insert-generated-annotations [ns config]
  (insert-or-replace-generated-annotations ns config))
(defn replace-generated-annotations [ns config]
  (insert-or-replace-generated-annotations ns (assoc config :replace-top-level? true)))

(defn infer-anns
  ([ns {:keys [spec?] :as config}]
   {:pre [(or (instance? clojure.lang.Namespace ns)
              (symbol? ns))]}
     (let [existing-alias? (some? (namespace-alias-in
                                    (the-ns (current-ns))
                                    (find-ns
                                      (if spec?
                                        spec-ns
                                        'clojure.core.typed))))
           require-info
           (when-not existing-alias?
             (let [current-aliases (ns-aliases (the-ns (current-ns)))
                   new-alias (first
                               (remove current-aliases
                                       (if spec?
                                         '[s spc spec]
                                         '[t typ ct typed])))
                   explicit-ns (if spec?
                                 spec-ns
                                 'clojure.core.typed)]
               (when new-alias
                 (binding [*ns* (the-ns (current-ns))]
                   (println (str "Aliasing " explicit-ns
                                 " as " new-alias " in "
                                 (current-ns)))
                   (require [explicit-ns :as new-alias])
                   [explicit-ns new-alias]))))
           out (if spec? 
                 envs-to-specs
                 envs-to-annotations)
           infer-results (if-let [load-infer-results (:load-infer-results config)]
                           (do (println (str "Loading inference results from " load-infer-results))
                             (read-string (slurp load-infer-results)))
                           @results-atom)
           _ (assert (infer-results? infer-results))
           _ (when-let [save-infer-results (:save-infer-results config)]
               (let [;; turn :class types into :unresolved-class's
                     infer-results (update infer-results :infer-results
                                           (fn [rs]
                                             (into #{}
                                                   (map (fn [ir]
                                                          (update ir :type 
                                                                  (fn [t]
                                                                    (postwalk t
                                                                              (fn [c]
                                                                                (cond
                                                                                  (when (= :class (:op c))
                                                                                    (let [^Class inst (::class-instance c)
                                                                                          _ (assert (class? inst))
                                                                                          nme (.getName inst)]
                                                                                      (and (not (.startsWith nme "clojure.lang"))
                                                                                           (not (.startsWith nme "java.lang")))))
                                                                                  {:op :unresolved-class
                                                                                   ::class-string (let [^Class inst (::class-instance c)]
                                                                                                    (.getName inst))
                                                                                   :args (:args c)}
                                                                                  :else c)))))))
                                                   rs)))]
                 (spit save-infer-results infer-results)
                 (println (str "Saved inference results to " save-infer-results))))]
       (-> (init-env)
           (generate-tenv config infer-results)
           (populate-envs config)
           (out (assoc config :explicit-require-needed require-info
                       :call-flows (:call-flows infer-results)))))))

(defn infer-with-frontend [front-end {:keys [ns fuel out-dir save-infer-results load-infer-results debug
                                             no-local-ann? polymorphic? spec-diff? no-squash-vertically
                                             spec-macros] :as args}]
  {:pre [((some-fn nil? string?) out-dir)
         ((some-fn nil? #{:all :iterations}) debug)]}
  (when (and load-infer-results (symbol? ns))
    (require ns))
  ;(prn "polymorphic?" polymorphic?)
  (binding [*spec* (= :spec front-end)
            *debug* debug
            *preserve-unknown* (if-let [[_ preserve-unknown] (find args :preserve-unknown)]
                                 preserve-unknown
                                 *preserve-unknown*)
            *track-depth* (if-let [[_ track-depth] (find args :track-depth)]
                            track-depth
                            *track-depth*)
            *track-count* (if-let [[_ track-count] (find args :track-count)]
                            track-count
                            *track-count*)
            *root-results* (if-let [[_ root-results] (find args :root-results)]
                             root-results
                             *root-results*)
            *higher-order-fspec* (if-let [[_ higher-order-fspec] (find args :higher-order-fspec)]
                                   higher-order-fspec
                                   *higher-order-fspec*)
            ]
    ;(prn "args" args)
    (when *track-depth*
      (println "*track-depth*:" *track-depth*))
    (when *track-count*
      (println "*track-count*:" *track-count*))
    (when *root-results*
      (println "*root-results*:" *root-results*))
    (replace-generated-annotations ns 
                                   (merge
                                     (init-config)
                                     {:no-local-ann? no-local-ann?
                                      :no-squash-vertically no-squash-vertically
                                      :spec? (= :spec front-end)
                                      :polymorphic? polymorphic?
                                      :spec-diff? spec-diff?
                                      :spec-macros spec-macros}
                                     (when save-infer-results
                                       (assert (string? save-infer-results)
                                               (str ":save-infer-results must be a string, given"
                                                    (pr-str (class save-infer-results))))
                                       {:save-infer-results save-infer-results})
                                     (when load-infer-results
                                       (assert (string? load-infer-results)
                                               (str ":load-infer-results must be a string, given"
                                                    (pr-str (class load-infer-results))))
                                       {:load-infer-results load-infer-results})
                                     (when out-dir
                                       {:out-dir out-dir})
                                     (when fuel
                                       {:fuel fuel})))))

(defn runtime-infer [args] (infer-with-frontend :type args))
(defn spec-infer [args] (infer-with-frontend :spec args))

(defn refresh-runtime-infer []
  (reset! results-atom (initial-results))
  nil)

;; Runtime Instrumentation API

;(defn instrument-var [v]
;  )
;
;(defn instrument-ns [v]
;  )

;; TESTS

(comment

  (binding [*ns* (the-ns 'clojure.core.typed.test.mini-occ)]
    (-> (generate-tenv (init-env) (init-config) @results-atom)
        (envs-to-annotations (init-config))
        pprint)
    )

(defmacro defntrack [n & args]
  `(def ~n (track-def-init
             '~(gen-track-config)
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
