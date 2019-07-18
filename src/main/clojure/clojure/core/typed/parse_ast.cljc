;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc ^:skip-wiki clojure.core.typed.parse-ast
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.set :as set]))

(t/ann *parse-type-in-ns* (t/U nil t/Sym))
(defonce ^:dynamic *parse-type-in-ns* nil)

;(t/ann ^:no-check clojure.core.typed.current-impl/*current-impl* (t/U nil t/Kw))
(t/ann ^:no-check clojure.core.typed.current-impl/current-impl [-> t/Kw])

(t/ann parse-in-ns [-> t/Sym])
(let [cljs-ns (delay (impl/dynaload 'clojure.core.typed.util-cljs/cljs-ns))]
  (defn- parse-in-ns []
    {:post [(symbol? %)]}
    (or *parse-type-in-ns* 
        (impl/impl-case
          :clojure (ns-name *ns*)
          :cljs (t/tc-ignore (@cljs-ns))))))

(t/ann ^:no-check clojure.core.typed.current-impl/assert-clojure
       (t/IFn [-> nil]
              [(t/U nil (t/Seqable t/Any)) -> nil]))

(t/ann ^:no-check clojure.core.typed.errors/int-error 
       (t/IFn [t/Any -> t/Nothing]
              [t/Nothing (t/HMap :optional {:use-current-env t/Any}) -> t/Nothing]))

(t/ann resolve-type-clj [t/Sym -> (t/U (t/Var2 t/Nothing t/Any) Class nil)])
(defn- resolve-type-clj 
  "Returns a var, class or nil"
  [sym]
  {:pre [(symbol? sym)]
   :post [((some-fn var? class? nil?) %)]}
  (impl/assert-clojure)
  (let [nsym (parse-in-ns)]
    (if-let [ns (find-ns nsym)]
      (ns-resolve ns sym)
      (err/int-error (str "Cannot find namespace: " sym)))))

(defn- resolve-alias-clj 
  "Returns a symbol if sym maps to a type alias, otherwise nil"
  [sym]
  {:pre [(symbol? sym)]
   :post [((some-fn symbol? nil?) %)]}
  (impl/assert-clojure)
  (let [nsym (parse-in-ns)
        nsp (some-> (namespace sym) symbol)]
    (if-let [ns (find-ns nsym)]
      (when-let [qual (if nsp
                        (some-> (or ((ns-aliases ns) nsp)
                                    (find-ns nsp))
                                ns-name)
                        (ns-name ns))]
        (let [_ (assert (and (symbol? qual)
                             (not (namespace qual))))
              qsym (symbol (name qual) (name sym))]
          (when (contains? (impl/alias-env) qsym)
            qsym)))
      (err/int-error (str "Cannot find namespace: " sym)))))

(declare parse parse-path-elem)

(t/ann ^:no-check parse [t/Any -> Type])

(t/defalias Type
  (t/Rec [Type]
    (t/U '{:op ':singleton
           :val t/Any
           :form t/Any}
         '{:op ':Fn
           :arities (t/Vec Function)
           :form t/Any
           :children (t/Vec t/Kw)})))

;Map from scoped vars to unique names
(t/ann *tvar-scope* (t/Map t/Sym t/Sym))
(def ^:dynamic *tvar-scope* {})

(t/ann *dotted-scope* (t/Map t/Sym t/Sym))
(def ^:dynamic *dotted-scope* {})

#?(:clj
(defmacro with-frees [fs & args]
  `(binding [*tvar-scope* (merge *tvar-scope* ~fs)]
     ~@args)))

#?(:clj
(defmacro with-dfrees [fs & args]
  `(binding [*dotted-scope* (merge *dotted-scope* ~fs)]
     ~@args)))

(t/defalias Filter
  (t/Rec [Filter]
  (t/U '{:op ':top-filter}
       '{:op ':bot-filter}
       '{:op ':or-filter
         :fs (t/Vec Filter)}
       '{:op ':and-filter
         :fs (t/Vec Filter)}
       '{:op ':impl-filter
         :a Filter
         :c Filter}
       (HMap :mandatory {:op ':type-filter
                         :type Type
                         :id NameRef}
             :optional {:path (t/Vec PathElem)})
       (HMap :mandatory {:op ':not-type-filter
                         :type Type
                         :id NameRef}
             :optional {:path (t/Vec PathElem)}))))

(t/ann parse-filter [t/Any -> Filter])
(defn parse-filter [syn]
  (case syn
    tt {:op :top-filter}
    ff {:op :bot-filter}
    (let [m (when ((every-pred seq? sequential?) syn)
              (let [[f & args] syn]
                (case f
                  is
                  (let [[tsyn nme psyns] args
                        _ (when (and (#{3} (count args))
                                     (not (vector? psyns)))
                            (err/int-error
                              (str "3rd argument to 'is' must be a vector")))
                        _ (when-not (#{2 3} (count args))
                            (throw (ex-info "Bad arguments to 'is'"
                                            {:form syn})))
                        t (parse tsyn)
                        p (when (#{3} (count args))
                            (mapv parse-path-elem psyns))]
                    (merge 
                      {:op :type-filter
                       :type t
                       :id nme}
                      (when p
                        {:path p})))
                  !
                  (let [[tsyn nme psyns] args
                        _ (when (and (#{3} (count args))
                                     (not (vector? psyns)))
                            (err/int-error
                              (str "3rd argument to '!' must be a vector")))
                        _ (when-not (#{2 3} (count args))
                            (throw (ex-info "Bad arguments to '!'"
                                            {:form syn})))
                        t (parse tsyn)
                        p (when (#{3} (count args))
                            (mapv parse-path-elem psyns))]
                    (merge 
                      {:op :not-type-filter
                       :type t
                       :id nme}
                      (when p
                        {:path p})))
                  |
                  {:op :or-filter
                   :fs (mapv parse-filter args)}
                  &
                  {:op :and-filter
                   :fs (mapv parse-filter args)}
                  when
                  (let [[a c] args]
                    (when-not (#{2} (count args))
                      (throw (ex-info "Bad arguments to 'when'"
                                      {:form syn})))
                    {:op :impl-filter
                     :a (parse-filter a)
                     :c (parse-filter c)})
                  nil)))]
      (if m
        m
        (err/int-error (str "Bad filter syntax: " syn))))))

(t/defalias FilterSet
  '{:op ':filter-set
    :then Filter
    :else Filter})

(t/ann parse-filter-set [t/Any -> FilterSet])
(defn parse-filter-set [syn]
  (when-not (map? syn)
    (err/int-error "Filter set must be a map"))
  (let [then (:then syn)
        else (:else syn)]
    {:op :filter-set
     :then (if then
             (parse-filter then)
             {:op :top-filter})
     :else (if else
             (parse-filter else)
             {:op :top-filter})}))

(t/defalias NameRef (t/U t/Sym t/Int))

(t/ann ^:no-check name-ref? (t/Pred NameRef))
(def name-ref? (some-fn symbol? (every-pred integer? (complement neg?))))

(t/defalias PathElem
  (t/U '{:op ':ClassPE}
       '{:op ':CountPE}
       '{:op ':KeysPE}
       '{:op ':ValsPE}
       '{:op ':NthPE
         :idx t/Int}
       '{:op ':KeysPE
         :key t/Any}
       '{:op ':ValsPE
         :key t/Any}))

; no-check for perf issues
(t/ann ^:no-check parse-path-elem [t/Any -> PathElem])
(defn parse-path-elem [syn]
  (case syn
    Class {:op :ClassPE}
    Count {:op :CountPE}
    Keys {:op :KeysPE}
    Vals {:op :ValsPE}
    Keyword {:op :KeywordPE}
    (let [m (when (seq? syn)
              (let [[f & args] syn]
                (case f
                  Nth (do
                        (when-not (#{1} (count args))
                          (err/int-error (str "Wrong arguments to Nth: " syn)))
                        {:op :NthPE
                         :idx (first args)})
                  Key (do
                        (when-not (#{1} (count args))
                          (err/int-error (str "Wrong arguments to Key: " syn)))
                        {:op :KeyPE
                         :key (first args)})
                  Val (do
                        (when-not (#{1} (count args))
                          (err/int-error (str "Wrong arguments to Val" syn)))
                        {:op :ValPE
                         :val (first args)})
                  nil)))]
      (if m
        m
        (err/int-error (str "Bad path element syntax: " syn))))))

(t/ann parse-object-path [t/Any -> FObject])
(defn parse-object-path [syn]
  (when-not (map? syn)
    (err/int-error (str "Object must be a map")))
  (let [id (:id syn)
        path (:path syn)]
    (when-not (name-ref? id)
      (err/int-error (str "Must pass natural number or symbol as id: " (pr-str id))))
    (when-not ((some-fn nil? vector?) path)
      (err/int-error "Path must be a vector"))
    (when (contains? syn :path)
      (when-not (vector? path)
        (err/int-error "Path must be a vector")))
    (merge
      {:op :object
       :id id}
      (when path
        {:path-elems (mapv parse-path-elem path)}))))

(defn parse-object [syn]
  (case syn
    empty {:op :empty-object}
    (parse-object-path syn)))

(t/defalias RestDrest
  (HMap :mandatory {:types (t/U nil (t/Coll Type))}
        :optional {:rest (t/U nil Type)
                   :drest (t/U nil DottedPretype)}))

(t/ann parse-with-rest-drest [t/Str (t/Seq t/Any) -> RestDrest])
(defn parse-with-rest-drest [msg syns]
  (let [rest? (#{'*} (last syns))
        dotted? (#{'...} (-> syns butlast last))
        _ (when (and rest? dotted?)
            (err/int-error (str msg syns)))
        {:keys [types rest drest]}
        (cond
          rest?
          (let [fixed (mapv parse (drop-last 2 syns))
                rest (parse (-> syns butlast last))]
            {:types fixed
             :rest rest})
          dotted?
          (let [fixed (mapv parse (drop-last 3 syns))
                [drest-type _dots_ drest-bnd :as dot-syntax] (take-last 3 syns)
                ; should never fail, if the logic changes above it's probably
                ; useful to keep around.
                _ (when-not (symbol? drest-bnd)
                    (err/int-error "Dotted rest bound after ... must be a symbol"))
                _ (when-not (#{3} (count dot-syntax))
                    (err/int-error (str "Bad vector syntax: " dot-syntax)))
                bnd (*dotted-scope* drest-bnd)
                _ (when-not bnd 
                    (err/int-error (str (pr-str drest-bnd) " is not in scope as a dotted variable")))
                gdrest-bnd (gensym bnd)]
            {:types fixed
             :drest (t/ann-form
                      {:op :dotted-pretype
                       :f {:op :F :name gdrest-bnd}
                       :drest (with-frees {drest-bnd gdrest-bnd} ;with dotted bound in scope as free
                                (parse drest-type))
                       :name gdrest-bnd}
                      DottedPretype)})
          :else {:types (mapv parse syns)})]
    {:types types
     :rest rest
     :drest drest}))

(t/tc-ignore

(defn parse-h* [op msg]
  (fn [[_ fixed & {:keys [filter-sets objects repeat]}]]
    (let [{:keys [types drest rest]}
          (parse-with-rest-drest msg fixed)]
      (merge
        {:op op
         :types types
         :children (vec (concat
                          [:types]
                          (when drest
                            [:drest])
                          (when rest
                            [:rest])
                          (when filter-sets
                            [:filter-sets])
                          (when objects
                            [:objects])
                          (when (true? repeat) [:repeat])))}
        (when drest
          {:drest drest})
        (when rest
          {:rest rest})
        (when filter-sets
          {:filter-sets (mapv parse-filter-set filter-sets)})
        (when objects
          {:objects (mapv parse-object objects)})
        (when (true? repeat) {:repeat true})))))

(def parse-HVec (parse-h* :HVec "Invalid HVec syntax:"))
(def parse-HSequential (parse-h* :HSequential "Invalid HSequential syntax:"))
(def parse-HSeq (parse-h* :HSeq "Invalid HSeq syntax:"))

(def parse-quoted-hvec (fn [syn]
                         (parse-HVec [nil syn])))

(defn parse-quoted-hseq [syn]
  (let [types (mapv parse syn)]
    {:op :HSeq
     :types types
     :children [:types]}))

(defn parse-quoted-hlist [syn]
  (let [types (mapv parse syn)]
    {:op :HList
     :types types
     :children [:types]}))

(defn- syn-to-hmap [mandatory optional absent-keys complete?]
  (when mandatory
    (when-not (map? mandatory)
      (err/int-error (str "Mandatory entries to HMap must be a map: " mandatory))))
  (when optional
    (when-not (map? optional)
      (err/int-error (str "Optional entries to HMap must be a map: " optional))))
  (letfn [(mapt [m]
            (into {} (for [[k v] m]
                       [{:op :singleton :val k}
                        (parse v)])))]
    (let [_ (when-not (every? empty? [(set/intersection (set (keys mandatory))
                                                        (set (keys optional)))
                                      (set/intersection (set (keys mandatory))
                                                        (set absent-keys))
                                      (set/intersection (set (keys optional))
                                                        (set absent-keys))])
              (err/int-error (str "HMap options contain duplicate key entries: ")))
          _ (when-not (every? keyword? (keys mandatory)) (err/int-error "HMap's mandatory keys must be keywords"))
          mandatory (mapt mandatory)
          _ (when-not (every? keyword? (keys optional)) (err/int-error "HMap's optional keys must be keywords"))
          optional (mapt optional)
          _ (when-not (every? keyword? absent-keys) (err/int-error "HMap's absent keys must be keywords"))
          absent-keys (set (map (fn [a] {:op :singleton :val a}) absent-keys))]
      {:op :HMap
       :mandatory (vec (apply concat mandatory))
       :optional (vec (apply concat optional))
       :complete? complete?
       :absent-keys (vec absent-keys)
       :children [:mandatory :optional :absent-keys]})))

(defn parse-quote [[_ syn]]
  (cond
    ((some-fn number? keyword? symbol?) syn) {:op :singleton :val syn}
    (vector? syn) (parse-quoted-hvec syn)
    ; quoted map is a partial map with mandatory keys
    (map? syn) (syn-to-hmap syn nil nil false)
    :else (err/int-error (str "Invalid use of quote:" (pr-str syn)))))

(def any-bounds {:op :bounds
                 :upper-bound {:op :Any}
                 :lower-bound {:op :U :types []}})

(def clj-primitives
  {'byte     {:op :clj-prim :name 'byte}
   'short    {:op :clj-prim :name 'short}
   'int      {:op :clj-prim :name 'int}
   'long     {:op :clj-prim :name 'long}
   'float    {:op :clj-prim :name 'float}
   'double   {:op :clj-prim :name 'double}
   'boolean  {:op :clj-prim :name 'boolean}
   'char     {:op :clj-prim :name 'char}
   'void     {:op :singleton :val nil}})

(def cljs-primitives
  {'int     {:op :cljs-prim :name 'int}
   'object  {:op :cljs-prim :name 'object}})

(defn parse-free [f gsym]
  (if (symbol? f)
    {:op :F
     :name gsym
     :bounds any-bounds}
    (let [[n & {:keys [< >] :as opts}] f]
      (when (contains? opts :kind)
        (err/deprecated-warn "kind annotation for TFn parameters is removed"))
      (when (:variance opts) 
        (err/int-error "Variance not supported for variables introduced with All"))
      {:op :F
       :name gsym
       :bounds {:upper (when (contains? opts :<)
                         (parse <))
                :lower (when (contains? opts :>)
                         (parse >))}})))

(defn parse-tfn-binder [[nme & opts-flat :as all] gsym]
  (let [_ (when-not (even? (count opts-flat))
            (err/int-error (str "Uneven arguments passed to TFn binder: "
                              (pr-str all))))
        {:keys [variance < >] 
         :or {variance :inferred}
         :as opts} 
        (apply hash-map opts-flat)]
    (when-not (symbol? nme)
      (err/int-error "Must provide a name symbol to TFn"))
    (when (contains? opts :kind)
      (err/deprecated-warn "kind annotation for TFn parameters is removed"))
    #_(when-not (r/variance? variance)
      (err/int-error (str "Invalid variance: " (pr-str variance))))
    {:name gsym :variance variance
     :bound {:op :bounds
             :upper-bound (when (contains? opts :<)
                            (parse <))
             :lower-bound (when (contains? opts :>)
                            (parse >))}}))

(defn parse-TFn 
  [[_ binder bodysyn :as tfn]]
  (when-not (= 3 (count tfn))
    (err/int-error (str "Wrong number of arguments to TFn: " (pr-str tfn))))
  (when-not (every? vector? binder)
    (err/int-error (str "TFn binder should be vector of vectors: " (pr-str tfn))))
  (let [; don't scope a free in its own bounds. Should review this decision
        [fs free-maps] (reduce
                         (fn [[fs prsed] b]
                           (let [sym (first b)
                                 _ (assert (symbol? sym))
                                 gsym (gensym sym)
                                 fs (conj fs [sym gsym])]
                             (with-frees fs
                               [fs (conj prsed (parse-tfn-binder b gsym))])))
                         [{} []]
                         binder)
        bodyt (with-frees fs
                (parse bodysyn))]
    {:op :TFn
     :binder free-maps
     :body bodyt}))

(defn multi-frequencies 
  "Like frequencies, but only returns frequencies greater
  than one"
  [coll]
  (->> coll
       frequencies
       (filter (fn [[k freq]]
                 (when (< 1 freq)
                   true)))
       (into {})))

(defn parse-HMap 
  [[_HMap_ & flat-opts :as all]]
  (let [supported-options #{:optional :mandatory :absent-keys :complete?}
        ; support deprecated syntax (HMap {}), which is now (HMap :mandatory {})
        deprecated-mandatory (when (map? (first flat-opts))
                               (err/deprecated-warn
                                 "HMap syntax changed. Use :mandatory keyword argument instead of initial map")
                               (first flat-opts))
        flat-opts (if deprecated-mandatory
                    (next flat-opts)
                    flat-opts)
        _ (when-not (even? (count flat-opts))
            (err/int-error (str "Uneven keyword arguments to HMap: " (pr-str all))))
        flat-keys (->> flat-opts
                       (partition 2)
                       (map first))
        _ (when-not (every? keyword? flat-keys)
            (err/int-error (str "HMap requires keyword arguments, given " (pr-str (first flat-keys))
                                #_#_" in: " (pr-str all))))
        _ (let [kf (->> flat-keys
                        multi-frequencies
                        (map first)
                        seq)]
            (when-let [[k] kf]
              (err/int-error (str "Repeated keyword argument to HMap: " (pr-str k)))))

        {:keys [optional mandatory absent-keys complete?]
         :or {complete? false}
         :as others} (apply hash-map flat-opts)
        _ (when-let [[k] (seq (set/difference (set (keys others)) supported-options))]
            (err/int-error (str "Unsupported HMap keyword argument: " (pr-str k))))
        _ (when (and deprecated-mandatory mandatory)
            (err/int-error (str "Cannot provide both deprecated initial map syntax and :mandatory option to HMap")))
        mandatory (or deprecated-mandatory mandatory)]
    (syn-to-hmap mandatory optional absent-keys complete?)))

(defn parse-All 
  [[_All_ & args :as syn]]
  (let [_ (when-not (#{2} (count args))
            (err/int-error "Wrong arguments to All"))
        [bnds type] args
        _ (when-not (vector? bnds)
            (err/int-error "Wrong arguments to All"))
        [bnds kwargs] (split-with (complement keyword?) bnds)
        _ (when-not (even? (count kwargs))
            (err/int-error "Wrong arguments to All"))
        {:keys [named] :as kwargs} kwargs
        dotted? (boolean 
                  ('#{...} (last bnds)))
        [fs frees-with-bnds] (reduce (fn [[fs prsed] fsyn]
                                       (let [sym (if (symbol? fsyn)
                                                   fsyn
                                                   (first fsyn))
                                             _ (assert (symbol? sym))
                                             gsym (gensym sym)
                                             fs (conj fs [sym gsym])]
                                         (with-frees fs
                                           [fs (conj prsed (parse-free fsyn gsym))])))
                                     [{} []]
                                     (if dotted?
                                       (drop-last 2 bnds)
                                       bnds))
        dvar-plain-name (when dotted?
                          (-> bnds butlast last))
        _ (assert ((some-fn nil? symbol?) dvar-plain-name))
        gdvar (gensym dvar-plain-name)
        dvar (when dotted?
               (parse-free dvar-plain-name gdvar))]
    (with-frees fs
      (with-dfrees (if dvar 
                     {dvar-plain-name (:name dvar)}
                     {})
        {:op (if dotted? :Poly :PolyDots)
         :binder (concat frees-with-bnds
                         (when dotted?
                           [dvar]))
         :named (or named {})
         :type (parse type)
         :children [:type]}))))

(defn parse-Extends
  [[_Extends_ & args :as syn]]
  (let [[extends & {:keys [without] :as opts}] args]
    (when-not (empty? (set/difference (set (keys opts)) #{:without}))
      (err/int-error (str "Invalid options to Extends:" (keys opts))))
    (when-not (vector? extends) 
      (err/int-error (str "Extends takes a vector of types: " (pr-str syn))))
    {:op :Extends
     :types (mapv parse extends)
     :without (mapv parse without)
     :children [:types :without]}))

(defn parse-U
  [[_U_ & args]]
  {:op :U
   :types (mapv parse args)
   :children [:types]})

(defn parse-I 
  [[_I_ & args]]
  {:op :I
   :types (mapv parse args)
   :children [:types]})

(t/ann parse-seq* [(t/Seq t/Any) -> Type])
(defmulti parse-seq*
  (fn [[n]]
    {:post [((some-fn nil? symbol?) %)]}
    (when (symbol? n)
      (or (impl/impl-case
            :clojure (cond
                       (special-symbol? n) n
                       :else (let [r (resolve-type-clj n)]
                               (when (var? r)
                                 (coerce/var->symbol r))))
            :cljs n)
          n))))

(defmethod parse-seq* 'quote [syn] (parse-quote syn))

(defn parse-Value [[f & args :as syn]]
  (when-not (#{1} (count args))
    (err/int-error (str "Wrong arguments to Value: " syn)))
  {:op :singleton
   :val (first args)})

(defmethod parse-seq* 'Value [syn] 
  (err/deprecated-plain-op 'Value 'Val)
  (parse-Value syn))
(defmethod parse-seq* 'clojure.core.typed/Value [syn] (parse-Value syn))
(defmethod parse-seq* 'clojure.core.typed/Val [syn] (parse-Value syn))
(defmethod parse-seq* 'cljs.core.typed/Value [syn] (parse-Value syn))

(defn parse-Difference [[f & args :as syn]]
  (let [_ (when-not (<= 2 (count args))
            (err/int-error "Wrong arguments to Difference"))
        [t & without] args]
    {:op :Difference
     :type (parse t)
     :without (mapv parse without)
     :children [:type :without]}))

(defmethod parse-seq* 'Difference [syn] 
  (err/deprecated-plain-op 'Difference)
  (parse-Difference syn))
(defmethod parse-seq* 'clojure.core.typed/Difference [syn] (parse-Difference syn))
(defmethod parse-seq* 'cljs.core.typed/Difference [syn] (parse-Difference syn))

(defn parse-Rec [[f & args :as syn]]
  (let [_ (when-not (#{2} (count args))
            (err/int-error "Wrong arguments to Rec"))
        [[sym :as binder] t] args
        gsym (gensym sym)]
    {:op :Rec
     :f {:op :F :name gsym}
     :type (with-frees {sym gsym}
             (parse t))
     :children [:type]}))

(defmethod parse-seq* 'Rec [syn] 
  (err/deprecated-plain-op 'Rec)
  (parse-Rec syn))
(defmethod parse-seq* 'clojure.core.typed/Rec [syn] (parse-Rec syn))
(defmethod parse-seq* 'cljs.core.typed/Rec [syn] (parse-Rec syn))

(defn parse-CountRange [[f & args :as syn]]
  (let [_ (when-not (#{1 2} (count args))
            (err/int-error "Wrong arguments to CountRange"))
        [l u] args]
    {:op :CountRange
     :upper u
     :lower l}))

(defmethod parse-seq* 'CountRange [syn] 
  (err/deprecated-plain-op 'CountRange)
  (parse-CountRange syn))
(defmethod parse-seq* 'clojure.core.typed/CountRange [syn] (parse-CountRange syn))
(defmethod parse-seq* 'cljs.core.typed/CountRange [syn] (parse-CountRange syn))

(defn parse-ExactCount [[f & args :as syn]]
  (let [_ (when-not (#{1} (count args))
            (err/int-error "Wrong arguments to ExactCount"))
        [n] args]
    {:op :CountRange
     :upper n
     :lower n}))

(defmethod parse-seq* 'ExactCount [syn] 
  (err/deprecated-plain-op 'ExactCount)
  (parse-ExactCount syn))
(defmethod parse-seq* 'clojure.core.typed/ExactCount [syn] (parse-ExactCount syn))
(defmethod parse-seq* 'cljs.core.typed/ExactCount [syn] (parse-ExactCount syn))

(defn parse-Pred [[f & args :as syn]]
  (let [_ (when-not (#{1} (count args))
            (err/int-error (str "Wrong arguments to " f)))
        [t] args]
    {:op :predicate
     :type (parse t)
     :children [:type]}))

(defmethod parse-seq* 'predicate [syn] 
  (err/deprecated-plain-op 'predicate 'Pred)
  (parse-Pred syn))
(defmethod parse-seq* 'clojure.core.typed/Pred [syn] (parse-Pred syn))
(defmethod parse-seq* 'cljs.core.typed/Pred [syn] (parse-Pred syn))

(defn parse-Assoc [[f & args :as syn]]
  (let [_ (when-not (<= 1 (count args))
            (err/int-error "Wrong arguments to Assoc"))
        [t & entries] args
        {ellipsis-pos '...}
        (zipmap entries (range))

        [entries dentries] (split-at (if ellipsis-pos
                                       (dec ellipsis-pos)
                                       (count entries))
                                     entries)
        _ (when-not (-> entries count even?)
            (err/int-error (str "Incorrect Assoc syntax: "
                                syn
                                " , must have even number of key/val pair.")))
        _ (when-not (or (not ellipsis-pos)
                        (= (count dentries) 3))
            (err/int-error (str "Incorrect Assoc syntax: "
                                syn
                                " , must have even number of key/val pair.")))
        [drest-type _ drest-bnd] (when ellipsis-pos
                                   dentries)
        _ (when-not (or (not ellipsis-pos) (symbol? drest-bnd))
            (err/int-error "Dotted bound must be symbol"))]
    {:op :Assoc
     :type (parse t)
     :entries (mapv parse entries)
     :dentries (when ellipsis-pos
                 (let [bnd (*dotted-scope* drest-bnd)
                       _ (when-not (symbol? bnd)
                           (err/int-error (str (pr-str drest-bnd)
                                               " is not in scope as a dotted variable")))
                       gbnd (gensym bnd)]
                   {:drest
                    {:op :dotted-pretype
                     :f {:op :F :name gbnd}
                     :drest (with-frees {drest-bnd gbnd} ;with dotted bound in scope as free
                              (parse drest-type))
                     :name gbnd}}))
     :children (concat [:type :entries] (when ellipsis-pos [:dentries]))}))

(defmethod parse-seq* 'Assoc [syn] 
  (err/deprecated-plain-op 'Assoc)
  (parse-Assoc syn))
(defmethod parse-seq* 'clojure.core.typed/Assoc [syn] (parse-Assoc syn))
(defmethod parse-seq* 'cljs.core.typed/Assoc [syn] (parse-Assoc syn))

(defn parse-Get [[f & args :as syn]]
  (let [_ (when-not (#{2 3} (count args))
            (err/int-error "Wrong arguments to Get"))
        [t ksyn not-foundsyn] args]
    (merge 
      {:op :Get
       :type (parse t)
       :key (parse ksyn)
       :children [:type :key]}
      (when (#{3} (count args))
        {:not-found (parse not-foundsyn)
         :children [:type :key :not-found]}))))

(defmethod parse-seq* 'Get [syn] 
  (err/deprecated-plain-op 'Get)
  (parse-Get syn))
(defmethod parse-seq* 'clojure.core.typed/Get [syn] (parse-Get syn))
(defmethod parse-seq* 'cljs.core.typed/Get [syn] (parse-Get syn))

(defmethod parse-seq* 'All [syn] 
  (err/deprecated-plain-op 'All)
  (parse-All syn))
(defmethod parse-seq* 'clojure.core.typed/All [syn] (parse-All syn))
(defmethod parse-seq* 'cljs.core.typed/All [syn] (parse-All syn))

(defmethod parse-seq* 'Extends [syn] (parse-Extends syn))

(defmethod parse-seq* 'U [syn] 
  (err/deprecated-plain-op 'U)
  (parse-U syn))
(defmethod parse-seq* 'clojure.core.typed/U [syn] (parse-U syn))
(defmethod parse-seq* 'cljs.core.typed/U [syn] (parse-U syn))

(defmethod parse-seq* 'I [syn] 
  (err/deprecated-plain-op 'I)
  (parse-I syn))
(defmethod parse-seq* 'clojure.core.typed/I [syn] (parse-I syn))
(defmethod parse-seq* 'cljs.core.typed/I [syn] (parse-I syn))

(defn parse-Array [[f & args :as syn]]
  (let [_ (when-not (#{1} (count args))
            (err/int-error "Expected 1 argument to Array"))
        [syn] args
        t (parse syn)]
    {:op :Array
     :read t
     :write t
     :children [:read :write]}))

(defmethod parse-seq* 'Array [syn] (parse-Array syn))

(defn parse-Array2 [[f & args :as syn]]
  (let [_ (when-not (#{2} (count args))
            (err/int-error "Expected 2 arguments to Array2"))
        [wsyn rsyn] args
        w (parse wsyn)
        r (parse rsyn)]
    {:op :Array
     :read r
     :write w
     :children [:read :write]}))

(defmethod parse-seq* 'Array2 [syn] (parse-Array2 syn))

(defn parse-ReadOnlyArray [[f & args :as syn]]
  (let [_ (when-not (#{1} (count args))
            (err/int-error "Expected 1 arguments to ReadOnlyArray"))
        [rsyn] args
        r (parse rsyn)]
    {:op :Array
     :read r
     :write {:op :U :types []}
     :children [:read :write]}))

(defmethod parse-seq* 'ReadOnlyArray [syn] (parse-ReadOnlyArray syn))

(defn parse-Array3 [[f & args :as syn]]
  (let [_ (when-not (#{3} (count args))
            (err/int-error "Expected 3 arguments to Array3"))
        [wsyn rsyn jsyn] args
        w (parse wsyn)
        r (parse rsyn)]
    {:op :Array
     :read r
     :write {:op :U :types []
             :children [:types]}
     :java-syntax jsyn
     :children [:read :write]}))

(defmethod parse-seq* 'Array3 [syn] (parse-Array3 syn))

(defmethod parse-seq* 'TFn [syn] 
  (err/deprecated-plain-op 'TFn)
  (parse-TFn syn))
(defmethod parse-seq* 'clojure.core.typed/TFn [syn] (parse-TFn syn))
(defmethod parse-seq* 'cljs.core.typed/TFn [syn] (parse-TFn syn))

(t/defalias F
  '{:op ':F
    :name t/Sym})

(t/defalias DottedPretype
  '{:op ':dotted-pretype
    :f F
    :drest Type
    :name t/Sym})

(t/defalias FObject
  (t/U '{:op ':empty-object}
       (t/HMap :mandatory
               {:op ':object
                :id (t/U t/Sym t/Int)}
               :optional
               {:path-elems (t/Vec PathElem)})))

(t/defalias Function
  (t/HMap :mandatory
          {:op ':Fn-method
           :dom (t/Vec Type)
           :rng Type
           :filter FilterSet
           :object FObject
           :flow Filter
           :children (t/Vec t/Kw)}
          :optional
          {:rest Type
           :drest DottedPretype}))

(t/ann parse-function [t/Any -> Function])
(defn parse-function [f]
  (when-not (vector? f) 
    (err/int-error "Function arity must be a vector"))
  (let [is-arrow '#{-> :->}
        all-dom (take-while (complement is-arrow) f)
        [the-arrow rng & opts-flat :as chk] (drop-while (complement is-arrow) f) ;opts aren't used yet
        _ (when ('#{->} the-arrow)
            ;TODO deprecate
            )
        _ (when-not (<= 2 (count chk)) 
            (err/int-error (str "Incorrect function syntax: " f)))

        _ (when-not (even? (count opts-flat)) 
            (err/int-error (str "Incorrect function syntax, must have even number of keyword parameters: " f)))

        opts (apply hash-map opts-flat)

        {ellipsis-pos '...
         asterix-pos '*
         ampersand-pos '&
         push-rest-pos '<*
         push-dot-pos '<...}
        (zipmap all-dom (range))

        _ (when-not (#{0 1} (count (filter identity [asterix-pos ellipsis-pos ampersand-pos push-rest-pos push-dot-pos])))
            (err/int-error "Can only provide one rest argument option: & ... * or <*"))

        _ (when-let [ks (seq (remove #{:filters :object :flow} (keys opts)))]
            (err/int-error (str "Invalid function keyword option/s: " ks)))

        filters (when-let [[_ fsyn] (find opts :filters)]
                  (parse-filter-set fsyn))

        object (when-let [[_ obj] (find opts :object)]
                 (parse-object obj))

        flow (when-let [[_ obj] (find opts :flow)]
               (parse-filter obj))

        fixed-dom (cond 
                    asterix-pos (take (dec asterix-pos) all-dom)
                    ellipsis-pos (take (dec ellipsis-pos) all-dom)
                    ampersand-pos (take ampersand-pos all-dom)
                    push-rest-pos (take (dec push-rest-pos) all-dom)
                    push-dot-pos (take (dec push-dot-pos) all-dom)
                    :else all-dom)

        rest-type (when asterix-pos
                    (nth all-dom (dec asterix-pos)))
        _ (when-not (or (not asterix-pos)
                        (= (count all-dom) (inc asterix-pos)))
            (err/int-error (str "Trailing syntax after rest parameter: " (pr-str (drop (inc asterix-pos) all-dom)))))
        [drest-type _ drest-bnd :as drest-seq] (when ellipsis-pos
                                                 (drop (dec ellipsis-pos) all-dom))
        _ (when-not (or (not ellipsis-pos) (= 3 (count drest-seq))) 
            (err/int-error "Dotted rest entry must be 3 entries"))
        _ (when-not (or (not ellipsis-pos) (symbol? drest-bnd))
            (err/int-error "Dotted bound must be symbol"))
        [pdot-type _ pdot-bnd :as pdot-seq] (when push-dot-pos
                                                 (drop (dec push-dot-pos) all-dom))
        _ (when-not (or (not push-dot-pos) (= 3 (count pdot-seq)))
            (err/int-error "push dotted rest entry must be 3 entries"))
        _ (when-not (or (not push-dot-pos) (symbol? pdot-bnd))
            (err/int-error "push dotted bound must be symbol"))
        [& {optional-kws :optional mandatory-kws :mandatory} :as kws-seq]
        (let [kwsyn (when ampersand-pos
                      (drop (inc ampersand-pos) all-dom))]
          ; support deprecated syntax [& {} -> ] to be equivalent to [& :optional {} -> ]
          (if (and kwsyn
                   (map? (first kwsyn)))
            (do (err/deprecated-warn "[& {} -> ] function syntax is deprecated. Use [& :optional {} -> ]")
                (cons :optional kwsyn))
            kwsyn))

        _ (when-not (or (not ampersand-pos) (seq kws-seq)) 
            (err/int-error "Must provide syntax after &"))

        prest-type (when push-rest-pos
                     (nth all-dom (dec push-rest-pos)))
        _ (when-not (or (not push-rest-pos)
                        (= (count all-dom) (inc push-rest-pos)))
            (err/int-error (str "Trailing syntax after push-rest parameter: " (pr-str (drop (inc push-rest-pos) all-dom)))))]
    (merge
      {:op :Fn-method
       :dom (mapv parse fixed-dom)
       :rng (parse rng)
       :filter filters
       :object object
       :flow flow
       :children (vec (concat [:dom :rng :filter :object :flow]
                              (when asterix-pos
                                [:rest])
                              (when ellipsis-pos
                                [:drest])
                              (when push-rest-pos
                                [:prest])
                              (when push-dot-pos
                                [:pdot])))}
      (when asterix-pos
        {:rest (parse rest-type)})
      (when ellipsis-pos
        (let [bnd (*dotted-scope* drest-bnd)
              _ (when-not (symbol? bnd)
                  (err/int-error (str (pr-str drest-bnd) " is not in scope as a dotted variable")))
              gbnd (gensym bnd)]
          {:drest
           {:op :dotted-pretype
            :f {:op :F :name gbnd}
            :drest (with-frees {drest-bnd gbnd} ;with dotted bound in scope as free
                     (parse drest-type))
            :name gbnd}}))
      (when push-rest-pos
        {:prest (parse prest-type)})
      (when push-dot-pos
        (let [bnd (*dotted-scope* pdot-bnd)
              _ (when-not (symbol? bnd)
                  (err/int-error (str (pr-str pdot-bnd) " is not in scope as a dotted variable")))
              gbnd (gensym bnd)]
          {:pdot
           {:op :dotted-pretype
            :f {:op :F :name gbnd}
            :drest (with-frees {pdot-bnd gbnd} ;with dotted bound in scope as free
                     (parse pdot-type))
            :name gbnd}})))))

(defn parse-Fn [[_ & args :as syn]]
  {:op :Fn
   :arities (mapv parse-function args)
   :form syn
   :children [:arities]})

(defmethod parse-seq* 'Fn [syn] 
  (err/deprecated-plain-op 'Fn 'IFn)
  (parse-Fn syn))
(defmethod parse-seq* 'clojure.core.typed/IFn [syn] (parse-Fn syn))
(defmethod parse-seq* 'cljs.core.typed/IFn [syn] (parse-Fn syn))

(defmethod parse-seq* 'HMap [syn] 
  (err/deprecated-plain-op 'HMap)
  (parse-HMap syn))
(defmethod parse-seq* 'clojure.core.typed/HMap [syn] (parse-HMap syn))
(defmethod parse-seq* 'cljs.core.typed/HMap [syn] (parse-HMap syn))

(defmethod parse-seq* 'Vector* [syn] 
  (err/deprecated-plain-op 'Vector* 'HVec)
  (parse-quoted-hvec (vec (rest syn))))
(defmethod parse-seq* 'Seq* [syn] 
  (err/deprecated-plain-op 'Seq* 'HSeq)
  (parse-quoted-hseq (rest syn)))
(defmethod parse-seq* 'List* [syn] 
  #_(err/deprecated-plain-op 'List* 'HList)
  (parse-quoted-hlist (rest syn)))

(defmethod parse-seq* 'HVec [syn] 
  (err/deprecated-plain-op 'HVec)
  (parse-HVec syn))
(defmethod parse-seq* 'clojure.core.typed/HVec [syn] (parse-HVec syn))
(defmethod parse-seq* 'cljs.core.typed/HVec [syn] (parse-HVec syn))

(defmethod parse-seq* 'HSequential [syn] 
  (err/deprecated-plain-op 'HSequential)
  (parse-HSequential syn))
(defmethod parse-seq* 'clojure.core.typed/HSequential [syn] (parse-HSequential syn))
(defmethod parse-seq* 'cljs.core.typed/HSequential [syn] (parse-HSequential syn))

(defmethod parse-seq* 'HSeq [syn] 
  (err/deprecated-plain-op 'HSeq)
  (parse-HSeq syn))
(defmethod parse-seq* 'clojure.core.typed/HSeq [syn] (parse-HSeq syn))
(defmethod parse-seq* 'cljs.core.typed/HSeq [syn] (parse-HSeq syn))

(defn parse-HSet [[_ ts & {:keys [complete?] :or {complete? true}} :as args]]
  {:op :HSet
   :fixed ts
   :complete? complete?})

(defmethod parse-seq* 'clojure.core.typed/HSet [syn] (parse-HSet syn))

(defmethod parse-seq* :default [[f & args :as syn]]
  {:op :TApp
   :rator (parse f)
   :rands (mapv parse args)
   :children [:rator :rands]})

(defn parse-Not [[f & args :as syn]]
  (let [_ (when-not (#{1} (count args))
            (err/int-error "Wrong arguments to Not"))]
    {:op :Not
     :type (parse (first args))}))

(defmethod parse-seq* 'Not [syn] (parse-Not syn))

(defn parse-seq [syn]
  (parse-seq* syn))

(defmulti parse-symbol*
  (fn [n] 
    {:pre [(symbol? n)]}
    (or (impl/impl-case
          :clojure (let [r (resolve-type-clj n)]
                     (when (var? r)
                       (coerce/var->symbol r)))
          ;TODO
          :cljs n)
        n)))

(defn parse-Any [s] {:op :Any :form s})
(defn parse-Nothing [s]
  (parse-U `(clojure.core.typed/U)))

(defmethod parse-symbol* 'Any [s] 
  (impl/impl-case
    :clojure (err/deprecated-warn "Any syntax is deprecated, use clojure.core.typed/Any")
    :cljs nil)
  (parse-Any s))
(defmethod parse-symbol* 'clojure.core.typed/Any [s] (parse-Any s))

(defmethod parse-symbol* 'Nothing [s] 
  (impl/impl-case
    :clojure (err/deprecated-warn "Nothing syntax is deprecated, use clojure.core.typed/Nothing")
    :cljs nil)
  (parse-Nothing s))
(defmethod parse-symbol* 'clojure.core.typed/Nothing [s] (parse-Nothing s))

(defn parse-AnyFunction [s]
  {:op :AnyFunction :form s})

(defmethod parse-symbol* 'AnyFunction [s] (parse-AnyFunction s))

(defmethod parse-symbol* :default
  [sym]
  (let [primitives (impl/impl-case
                     :clojure clj-primitives
                     :cljs cljs-primitives)
        free (when (symbol? sym)
               (*tvar-scope* sym))]
    (cond
      free {:op :F :name free :form sym}
      (primitives sym) (assoc (primitives sym)
                              :form sym)
      :else 
        (or (impl/impl-case
              :clojure (let [res (when (symbol? sym)
                                   (resolve-type-clj sym))]
                         (cond 
                           (class? res) (let [csym (coerce/Class->symbol res)
                                              dt? (contains? (impl/datatype-env) csym)]
                                          {:op (if dt? :DataType :Class) :name csym})
                           (var? res) (let [vsym (coerce/var->symbol res)]
                                        (if (contains? (impl/alias-env) vsym)
                                          {:op :Name :name vsym}
                                          {:op :Protocol :name vsym}))
                           (symbol? sym)
                           (if-let [qsym (resolve-alias-clj sym)]
                             ; a type alias without an interned var
                             {:op :Name :name qsym}
                             ;an annotated datatype that hasn't been defined yet
                             ; assume it's in the current namespace
                                      ; do we want to munge the sym also?
                             (let [qname (symbol (str (namespace-munge (parse-in-ns)) "." sym))]
                               (when (contains? (impl/datatype-env) qname)
                                 {:op :DataType :name qname})))))
              :cljs (assert nil)
               #_(when-let [res (when (symbol? sym)
                                      (resolve-type-cljs sym))]
                       (:name res)))
            (err/int-error (str "Cannot resolve type: " (pr-str sym)
                                "\nHint: Is " (pr-str sym) " in scope?"
                                "\nHint: Has " (pr-str sym) "'s annotation been"
                                " found via check-ns, cf or typed-deps?"))))))

(defn parse-symbol [s]
  (parse-symbol* s))

(defn parse [syn]
  (binding [vs/*current-env* (let [ne (when-let [m (meta syn)]
                                        (select-keys m [:file :line :column :end-line :end-column]))]
                               (or (when ((every-pred :file :line :column) ne)
                                     ne)
                                   vs/*current-env*))]
    (cond
      (nil? syn) {:op :singleton :val nil :form syn}
      (true? syn) {:op :singleton :val true :form syn}
      (false? syn) {:op :singleton :val false :form syn}
      (vector? syn) {:op :Fn 
                     :arities [(parse-function syn)]
                     :form syn
                     :children [:arities]}
      (symbol? syn) (assoc (parse-symbol syn)
                           :form syn)
      (seq? syn) (assoc (parse-seq syn)
                        :form syn)
      :else (err/int-error (str "Bad type syntax: " syn)))))

(defn parse-clj [syn]
  (impl/with-impl impl/clojure
    (parse syn)))

(comment
  (= (parse 'nil)
     {:op :singleton
      :val nil
      :form nil})
  (= (parse 'true)
     {:op :singleton
      :val true
      :form true})

  (= (parse 'true)
     {:op :singleton
      :val true
      :form true})

  (parse '[true true -> Nothing])
  (impl/with-impl impl/clojure
    (parse 'a))
  )
)
