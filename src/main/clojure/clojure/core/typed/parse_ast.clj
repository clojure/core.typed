(ns ^:skip-wiki clojure.core.typed.parse-ast
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.set :as set]))

(alter-meta! *ns* assoc :skip-wiki true)

(defonce ^:dynamic *parse-type-in-ns* nil)

(defn- parse-in-ns []
  {:post [(symbol? %)]}
  (or *parse-type-in-ns* 
      (impl/impl-case
        :clojure (ns-name *ns*)
        :cljs (impl/v 'cljs.analyzer/*cljs-ns*))))

(defn- resolve-type-clj 
  "Returns a var, class or nil"
  [sym]
  {:pre [(symbol? sym)]}
  (impl/assert-clojure)
  (let [nsym (parse-in-ns)]
    (if-let [ns (find-ns nsym)]
      (ns-resolve ns sym)
      (err/int-error (str "Cannot find namespace: " sym)))))

(declare parse parse-path-elem)

;Map from scoped vars to unique names
(def ^:dynamic *tvar-scope* {})
(def ^:dynamic *dotted-scope* {})

(defmacro with-frees [fs & args]
  `(binding [*tvar-scope* (merge *tvar-scope* ~fs)]
     ~@args))

(defmacro with-dfrees [fs & args]
  `(binding [*dotted-scope* (merge *dotted-scope* ~fs)]
     ~@args))

(defn parse-filter [syn]
  (cond
    ('#{tt} syn) {:op :top-filter}
    ('#{ff} syn) {:op :bot-filter}
    :else
      (let [m (when (seq? syn)
                (let [[f & args] syn]
                  (cond
                    ('#{is} f)
                      (let [[tsyn nme psyns] args
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
                    ('#{!} f)
                      (let [[tsyn nme psyns] args
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
                    ('#{|} f)
                      {:op :or-filter
                       :fs (mapv parse-filter args)}
                    ('#{&} f)
                      {:op :and-filter
                       :fs (mapv parse-filter args)}
                    ('#{when} f)
                      (let [[a c] args]
                        (when-not (#{2} (count args))
                          (throw (ex-info "Bad arguments to 'when'"
                                          {:form syn})))
                        {:op :impl-filter
                         :a (parse-filter a)
                         :c (parse-filter c)}))))]
        (if m
          m
          (err/int-error (str "Bad filter syntax: " syn))))))

(defn parse-filter-set [{:keys [then else] :as fsyn}]
  {:op :filter-set
   :then (if then
           (parse-filter then)
           {:op :top-filter})
   :else (if else
           (parse-filter else)
           {:op :top-filter})})

(def name-ref? (some-fn symbol? (every-pred integer? (complement neg?))))

(defn parse-path-elem [syn]
  (cond
    ('#{Class} syn) {:op :ClassPE}
    ('#{Count} syn) {:op :CountPE}
    ('#{Keys} syn) {:op :KeysPE}
    ('#{Vals} syn) {:op :ValsPE}
    :else
      (let [m (when (seq? syn)
                (let [[f & args] syn]
                  (cond
                    ('#{Key} f) (do
                                  (when-not (#{1} (count args))
                                    (err/int-error (str "Wrong arguments to Key: " syn)))
                                  {:op :KeyPE
                                   :key (first args)})
                    ('#{Val} f) (do
                                  (when-not (#{1} (count args))
                                    (err/int-error (str "Wrong arguments to Val" syn)))
                                  {:op :ValPE
                                   :val (first args)}))))]
        (if m
          m
          (err/int-error (str "Bad path element syntax: " syn))))))

(defn parse-object [{:keys [id path]}]
  (when-not (name-ref? id)
    (err/int-error (str "Must pass natural number or symbol as id: " (pr-str id))))
  (merge
    {:op :object
     :id id}
    (when path
      {:path-elems (mapv parse-path-elem path)})))

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
                _ (when-not (#{3} (count dot-syntax))
                    (err/int-error (str "Bad vector syntax: " dot-syntax)))
                bnd (*dotted-scope* drest-bnd)
                _ (when-not bnd 
                    (err/int-error (str (pr-str drest-bnd) " is not in scope as a dotted variable")))
                gdrest-bnd (gensym bnd)]
            {:types fixed
             :drest {:op :dotted-pretype
                     :f {:op :F :name gdrest-bnd}
                     :drest (with-frees {drest-bnd gdrest-bnd} ;with dotted bound in scope as free
                              (parse drest-type))
                     :name gdrest-bnd}})
          :else {:types (mapv parse syns)})]
    {:types types
     :rest rest
     :drest drest}))

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
          {:objects (mapv parse-object filter-sets)})
        (when (true? repeat) {:repeat true})))))

(def parse-HVec (parse-h* :HVec "Invalid heterogeneous vector syntax:"))
(def parse-HSequential (parse-h* :HSequential "Invalid HSeqnential syntax:"))
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
  {'number  {:op :cljs-prim :name 'number}
   'int     {:op :cljs-prim :name 'int}
   'boolean {:op :cljs-prim :name 'boolean}
   'object  {:op :cljs-prim :name 'object}
   'string  {:op :cljs-prim :name 'string}})

(defn parse-free [f gsym]
  (if (symbol? f)
    {:op :F
     :name gsym
     :bounds any-bounds}
    (let [[n & {:keys [< >] :as opts}] f]
      (when (contains? opts :kind)
        (prn "DEPRECATED: kind annotation for TFn parameters"))
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
      (err/int-error "DEPRECATED: kind annotation for TFn parameters"))
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
                               (println 
                                 (parse-in-ns)
                                 ": DEPRECATED: HMap syntax changed. Use :mandatory keyword argument instead of initial map")
                               (flush)
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
                                " in: " (pr-str all))))
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
  {:op (if (empty? args) :U :I) ; Nothing is always (U)
   :types (mapv parse args)
   :children [:types]})

(declare parse-Fn)

(defn parse-seq [[f & args :as syn]]
  (cond
    ('#{quote} f) (parse-quote syn)
    ('#{Value} f) (let []
                    (when-not (#{1} (count args))
                      (err/int-error (str "Wrong arguments to Value: " syn)))
                    {:op :singleton
                     :val (first args)})
;    ('#{Not} f) (let [_ (when-not (#{1} (count args))
;                           (err/int-error "Wrong arguments to Not"))]
;                  {:op :Not
;                   :type (parse (first args))
;                   :children [:type]})
    ('#{Difference} f) (let [_ (when-not (<= 2 (count args))
                                 (err/int-error "Wrong arguments to Difference"))
                             [t & without] args]
                         {:op :Difference
                          :type (parse t)
                          :without (mapv parse without)
                          :children [:type :without]})
    ('#{Rec} f) (let [_ (when-not (#{2} (count args))
                           (err/int-error "Wrong arguments to Rec"))
                      [[sym :as binder] t] args
                      gsym (gensym sym)]
                  {:op :Rec
                   :f {:op :F :name gsym}
                   :type (with-frees {sym gsym}
                           (parse t))
                   :children [:type]})
    ('#{CountRange} f) (let [_ (when-not (#{1 2} (count args))
                                 (err/int-error "Wrong arguments to CountRange"))
                             [l u] args]
                         {:op :CountRange
                          :upper u
                          :lower l})
    ('#{ExactCount} f) (let [_ (when-not (#{1} (count args))
                                 (err/int-error "Wrong arguments to ExactCount"))
                             [n] args]
                         {:op :CountRange
                          :upper n
                          :lower n})
    ('#{predicate} f) (let [_ (when-not (#{1} (count args))
                                 (err/int-error "Wrong arguments to predicate"))
                            [t] args]
                         {:op :predicate
                          :type (parse t)
                          :children [:type]})
    ('#{Assoc} f) (let [_ (when-not (<= 1 (count args))
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
                     :children (concat [:type :entries] (when ellipsis-pos [:dentries]))})
    ('#{Get} f) (let [_ (when-not (#{2 3} (count args))
                          (err/int-error "Wrong arguments to Get"))
                        [t ksyn not-foundsyn] args]
                    (merge 
                      {:op :Get
                       :type (parse t)
                       :key (parse ksyn)
                       :children [:type :key]}
                      (when (#{3} (count args))
                        {:not-found (parse not-foundsyn)
                         :children [:type :key :not-found]})))
    ('#{All} f) (parse-All syn)
    ('#{Extends} f) (parse-Extends syn)
    ('#{U} f) (parse-U syn)
    ('#{I} f) (parse-I syn)
    ('#{Array} f) (let [_ (when-not (#{1} (count args))
                            (err/int-error "Expected 1 argument to Array"))
                        [syn] args
                        t (parse syn)]
                    {:op :Array
                     :read t
                     :write t
                     :children [:read :write]})
    ('#{Array2} f) (let [_ (when-not (#{2} (count args))
                            (err/int-error "Expected 2 argument to Array2"))
                         [wsyn rsyn] args
                         w (parse wsyn)
                         r (parse rsyn)]
                     {:op :Array
                      :read r
                      :write w
                      :children [:read :write]})
    ('#{ReadOnlyArray} f) (let [_ (when-not (#{1} (count args))
                                    (err/int-error "Expected 1 arguments to ReadOnlyArray"))
                                [rsyn] args
                                r (parse rsyn)]
                            {:op :Array
                             :read r
                             :write {:op :U :types []}
                             :children [:read :write]})
    ('#{Array3} f) (let [_ (when-not (#{3} (count args))
                                    (err/int-error "Expected 3 arguments to Array3"))
                         [wsyn rsyn jsyn] args
                         w (parse wsyn)
                         r (parse rsyn)]
                     {:op :Array
                      :read r
                      :write {:op :U :types []
                              :children [:types]}
                      :java-syntax jsyn
                      :children [:read :write]})
    ('#{TFn} f) (parse-TFn syn)
    ('#{Fn} f) (parse-Fn syn)
    ('#{HMap} f) (parse-HMap syn)
    ('#{Vector*} f) (parse-quoted-hvec (vec (rest syn)))
    ('#{Seq*} f) (parse-quoted-hseq (rest syn))
    ('#{List*} f) (parse-quoted-hlist (rest syn))
    ('#{HVec} f) (parse-HVec syn)
    ('#{HSequential} f) (parse-HSequential syn)
    ('#{HSeq} f) (parse-HSeq syn)
    :else {:op :TApp
           :rator (parse f)
           :rands (mapv parse args)
           :children [:rator :rands]}))

(defn parse-function [f]
  (let [all-dom (take-while #(not= '-> %) f)
        [_ rng & opts-flat :as chk] (drop-while #(not= '-> %) f) ;opts aren't used yet
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

        _ (when-not (#{0 1} (count (filter identity [asterix-pos ellipsis-pos ampersand-pos push-rest-pos])))
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
            (do (prn "DEPRECATED: implicit optional parameters for Fn arity. Use :optional keyword argument beteween & and ->.")
                (cons :optional kwsyn))
            kwsyn))

        _ (when-not (or (not ampersand-pos) (seq kws-seq))
            (err/int-error "Must provide syntax after &"))

        prest-type (when push-rest-pos
                     (nth all-dom (dec push-rest-pos)))
        _ (when-not (or (not push-rest-pos)
                        (= (count all-dom) (inc push-rest-pos)))
            (err/int-error (str "Trailing syntax after pust-rest parameter: " (pr-str (drop (inc push-rest-pos) all-dom)))))]
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

(defn parse-symbol
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
                                              dt? (contains? @impl/datatype-env csym)]
                                          {:op (if dt? :DataType :Class) :name csym})
                           (var? res) (let [vsym (coerce/var->symbol res)]
                                        (if (contains? @impl/alias-env vsym)
                                          {:op :Name :name vsym}
                                          {:op :Protocol :name vsym}))
                           (symbol? sym)
                           ;an annotated datatype that hasn't been defined yet
                           ; assume it's in the current namespace
                                    ; do we want to munge the sym also?
                           (let [qname (symbol (str (namespace-munge *ns*) "." sym))]
                             (when (contains? @impl/datatype-env qname)
                               {:op :DataType :name qname}))
                           ))
              :cljs (assert nil)
               #_(when-let [res (when (symbol? sym)
                                      (resolve-type-cljs sym))]
                       (:name res)))
            (err/int-error (str "Cannot resolve type: " (pr-str sym)
                                "\nHint: Is " (pr-str sym) " in scope?"
                                "\nHint: Has " (pr-str sym) "'s annotation been"
                                " found via check-ns, cf or typed-deps?"))))))

(defn parse [syn]
  (cond
    (nil? syn) {:op :singleton :val nil :form syn}
    (true? syn) {:op :singleton :val true :form syn}
    (false? syn) {:op :singleton :val false :form syn}
    ('#{Any} syn) {:op :Any :form syn}
    ('#{Nothing} syn) {:op :U :types [] :form syn :children [:types]}
    ('#{AnyFunction} syn) {:op :AnyFunction :form syn}
    (vector? syn) {:op :Fn 
                   :arities [(parse-function syn)]
                   :form syn
                   :children [:arities]}
    (symbol? syn) (assoc (parse-symbol syn)
                         :form syn)
    (seq? syn) (assoc (parse-seq syn)
                      :form syn)
    :else (err/int-error (str "Bad type syntax: " syn))
  ))

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
