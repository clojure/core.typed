;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.annotator.track
  (:require #?@(:clj [[potemkin.collections :as pot]])
            [clojure.core.typed.annotator.rep :refer [-val key-path map-vals-path
                                                      infer-results -class -any
                                                      fn-dom-path fn-rng-path
                                                      -nothing seq-entry
                                                      transient-vector-entry index-path
                                                      vec-entry-path
                                                      set-entry make-HMap
                                                      map-keys-path
                                                      atom-contents
                                                      var-path]]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.annotator.env :refer [add-infer-results!
                                                      results-atom]]
            [clojure.core.typed.annotator.util :refer [classify]]
            [clojure.math.combinatorics :as comb]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.current-impl :as impl]
            ))

(defn local-fn-symbol? [s]
  (= :local-fn (:clojure.core.typed.annotator.track/track-kind (meta s))))

(defn loop-var-symbol? [s]
  (= :loop-var (:clojure.core.typed.annotator.track/track-kind (meta s))))

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

#?(:clj
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
                                              current-all-kws?))))

(defn unwrap-value [v]
  (if-some [[_ u] (or (-> v meta (find ::unwrapped-fn))
                      (-> v meta (find ::unwrapped-seq))
                      #?(:clj
                         (when (instance? PersistentMapProxy v)
                           [nil (.m ^PersistentMapProxy v)])))]
    ;; values are only wrapped one level, no recursion calls needed
    u
    v))

(def track-metric-cache (atom {}))

; track : (Atom InferResultEnv) Value Path -> Value
(defn track 
  ([{:keys [track-depth track-count track-strategy track-metric root-results force-depth] :as config} results-atom v paths call-ids]
   {:pre [((con/set-c? vector?) paths)
          (seq paths)
          ((con/set-c? vector?) call-ids)]}
   #?(:clj
   (when (string? track-metric)
     (let [tm (or (get @track-metric-cache track-metric)
                  (-> track-metric read-string eval))
           _ (when-not (@track-metric-cache track-metric)
               (reset! track-metric-cache {track-metric tm}))]
       (tm (merge config 
                  {:results-atom results-atom
                   :v v 
                   :paths paths 
                   :call-ids call-ids})))))
   (let [;FIXME memory intensive
         #_#_
         _ (let [hs ((juxt 
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
                              call-ids))))
         paths-that-exceed-root-results (let [rr (:root-results @results-atom)]
                                          (when root-results
                                            (filter #(< root-results (get rr (-> % first :name) 0))
                                                    paths)))]
     (cond
       ((some-fn keyword? nil? false?) v)
       (do
         (add-infer-results! results-atom (infer-results (remove (set paths-that-exceed-root-results) paths)
                                                         (-val v)))
         v)

       ;; cut off path
       (or
         (not *should-track*)
         ;; cap at 1000 results per var
         (seq paths-that-exceed-root-results)
         (let [smallest-path-count (apply min (map count paths))]
           (if (and force-depth (>= force-depth smallest-path-count))
             false
             (when track-depth
               (> smallest-path-count track-depth)))))
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
               _ (add-infer-results! results-atom (infer-results (remove (set paths-that-exceed-root-results) paths)
                                                                 {:op :unknown}))]
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
                     _ (add-infer-results! results-atom (infer-results paths (-class :ifn [])))
                     call-ids (conj call-ids (gen-call-id paths))
                     ;; space-efficient function wrapping
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
                           (-class :list [-nothing]))))
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
             ;; space-efficient wrapping
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
                             (-class :seq [-nothing]))))
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

       (instance? #?(:clj clojure.lang.ITransientVector :cljs TransientVector) v)
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

       (vector? v)
       (let [heterogeneous? (<= (count v) 4)
             len (count v)
             so-far (atom 0)]
         (when (= 0 len)
           (add-infer-results! results-atom (infer-results paths (-class :vector [-nothing]))))
         (reduce
           (fn [e [k v]]
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
           (map-indexed vector v)))

       (set? v)
       (do
         (when (empty? v)
           (add-infer-results!
             results-atom
             (infer-results paths
                           (-class :set [-nothing]))))
         ;; preserve sorted sets
         (binding [*should-track* false]
           (into (empty v)
                 (map (fn [e]
                        (binding [*should-track* true]
                          (track config results-atom e (extend-paths paths (set-entry))
                                 call-ids))))
                 v)))

       #?(:clj (instance? PersistentMapProxy v))
       #?(:clj
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
                              (.current-all-kws? v))))

       #?(:clj
           (or (instance? clojure.lang.PersistentHashMap v)
               (instance? clojure.lang.PersistentArrayMap v)
               (instance? clojure.lang.PersistentTreeMap v))
          :cljs (map? v))
       (let [ks (set (keys v))]
         (when (empty? v)
           (add-infer-results!
             results-atom
             (infer-results paths (make-HMap {} {}))))
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
                 v #?(:cljs v
                      :clj (if (= track-strategy :lazy)
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
                             v))]
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
               no-kw-val))

           :else
           (let [so-far (atom 0)
                 v #?(:cljs v
                      :clj (if (= track-strategy :lazy)
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
                             v))]
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
               (keys v)))))

        (instance? #?(:clj clojure.lang.IAtom :cljs Atom) v)
        (let [old-val (-> v meta :clojure.core.typed/old-val)
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
               (add-infer-results! results-atom (infer-results paths (-class (classify v) [])))
               v)))))

(declare gen-track-config)

#?(:cljs
(defn track-cljs-val [v root]
  (track (gen-track-config)
         results-atom
         v
         #{[(var-path
              'root
              root)]}
         #{})))

#?(:clj
(def prim-invoke-interfaces
  (into #{}
        (->>
          (map (fn [ss] (apply str ss))
               (apply concat
                      (for [n (range 1 6)]
                        (apply comb/cartesian-product (repeat n [\D \O \L])))))
          (remove (fn [ss]
                    (every? #{\O} ss)))))))

#?(:clj
(defn char->tag [c]
  {:pre [(char? c)]
   :post [(symbol? %)]}
  (case c
    \L 'long
    \D 'double
    \O 'java.lang.Object)))

#?(:clj
(defn tag->char [t]
  {:pre [((some-fn nil? symbol?) t)]
   :post [(char? %)]}
  (case t
    long \L
    double \D
    \O)))

#?(:clj
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
    prims)))

#?(:clj
(defn gen-nonvariadic-invokes [f-this]
  (for [arity (range 0 20),
        :let [args (repeatedly arity gensym)
              this (gensym 'this)]]
    `(~'invoke [~this ~@args]
       (~(f-this this) ~@args)))))

#?(:clj
(defn gen-variadic-invoke [f-this]
  (let [args (repeatedly 21 gensym)
        this (gensym 'this)]
    `(~'invoke [~this ~@args] (apply ~(f-this this) ~@args)))))

#?(:clj
(defn gen-apply-to [f-this]
  (let [this (gensym 'this)]
    `(~'applyTo [~this args#] (apply ~(f-this this) args#)))))

#?(:clj
(defn extend-IFn [f-this prims]
  `(clojure.lang.IFn
    ~@(gen-nonvariadic-invokes f-this)
    ~(gen-variadic-invoke f-this)
    ~(gen-apply-to f-this)
    ~@(gen-prim-invokes f-this prims))))

#?(:clj
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
    source)))

#?(:clj
(def this-ns *ns*))

#?(:clj
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
      s))))

#?(:clj
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
      
      :else f))))

(defn gen-track-config []
  (merge 
    {:track-strategy :lazy
     :track-depth *track-depth*
     :track-count *track-count*
     :root-results *root-results*}
    vs/*instrument-infer-config*))

; track-var : (IFn [Var -> Value] [(Atom Result) Var Sym -> Value])
#?(:clj
(defn track-var'
  ([vr] (track-var' (gen-track-config) results-atom vr *ns*))
  ([config vr] (track-var' config results-atom vr *ns*))
  ([config results-atom vr ns]
   {:pre [(var? vr)
          (instance? #?(:clj clojure.lang.IAtom :cljs Atom) results-atom)]}
   ;(prn "tracking" vr "in ns" ns)
   (wrap-prim
     vr
     (track config
            results-atom @vr #{[(var-path
                                  (ns-name (the-ns ns))
                                  (impl/var->symbol vr))]}
            #{})))))

#?(:clj
(defmacro track-var [v]
  `(track-var' (var ~v))))

; track-def-init : Sym Sym Value -> Value
#?(:clj 
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
             #{})))))

#?(:clj 
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
             :clojure.core.typed.annotator.track/track-kind track-kind
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
                     {:clojure.core.typed.annotator.track/track-kind track-kind
                      :line line
                      :column column
                      :end-line end-line
                      :end-column end-column
                      :ns (ns-name ns)})}]}
         #{})))
