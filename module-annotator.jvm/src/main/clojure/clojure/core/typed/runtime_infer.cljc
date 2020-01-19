;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.runtime-infer
  (:refer-clojure :exclude [any? #?(:cljs -val)])
  #?(:cljs
     (:require-macros [clojure.core.typed.runtime-infer
                       :refer [debug-output
                               debug-output-when
                               when-fuel]]))
  (:require [#?(:clj clojure.pprint :cljs cljs.pprint) :as pp]
            [#?(:clj clojure.core :cljs cljs.core) :as core]
            [clojure.core.typed.annotator.rep :refer [-val key-path map-vals-path
                                                      infer-result infer-results
                                                      -class type? -any fn-dom-path
                                                      fn-rng-path -nothing
                                                      seq-entry transient-vector-entry
                                                      index-path vec-entry-path
                                                      set-entry
                                                      make-HMap
                                                      var-path
                                                      alias?
                                                      -alias
                                                      HMap?
                                                      val?
                                                      HVec?
                                                      union?
                                                      nothing?]]
            [clojure.core.typed.annotator.track :refer [track-var'
                                                        track-def-init
                                                        gen-track-config
                                                        track-local-fn
                                                        *track-depth*
                                                        *track-count*
                                                        *root-results*
                                                        local-fn-symbol?
                                                        loop-var-symbol?]]
            [clojure.core.typed.annotator.join :refer [make-Union
                                                       flatten-unions
                                                       flatten-union
                                                       merge-HMaps
                                                       join-HMaps
                                                       HMap-join-strategies
                                                       join*
                                                       join
                                                       join-IFn1]]
            [clojure.core.typed.annotator.env :refer [results-atom
                                                      initial-results
                                                      infer-results?]]
            [clojure.core.typed.annotator.frontend.spec
             :refer [def-spec or-spec unparse-spec unparse-spec'
                     alias-matches-key-for-spec-keys?
                     alias->spec-kw
                     *higher-order-fspec*
                     spec-cat
                     unq-spec-nstr
                     register-unique-alias-for-spec-keys
                     envs-to-specs
                     ]]
            [clojure.core.typed.annotator.debug-macros
             :refer [debug-flat
                     debug-when
                     time-if-slow
                     debug-squash
                     debug]]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.math.combinatorics :as comb]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.annotator.util :refer [unparse-type spec-ns core-specs-ns
                                                       qualify-typed-symbol
                                                       qualify-spec-symbol
                                                       qualify-core-symbol
                                                       *ann-for-ns*
                                                       current-ns
                                                       namespace-alias-in
                                                       camel-case
                                                       *debug*
                                                       update-env
                                                       update-type-env-in-ns
                                                       update-type-env
                                                       update-alias-env
                                                       list*-force
                                                       fv
                                                       resolve-alias
                                                       get-env
                                                       type-env
                                                       alias-env
                                                       *envs*
                                                       unp
                                                       *spec*
                                                       intersection-or-empty
                                                       HMap-common-req-keys
                                                       HMap-req-keyset
                                                       map-key-set
                                                       HMap-likely-tag-key
                                                       kw-val?
                                                       kw-vals?
                                                       *preserve-unknown*
                                                       kw->sym
                                                       *used-aliases*
                                                       *multispecs-needed*
                                                       fully-resolve-alias
                                                       *forbidden-aliases*
                                                       find-top-level-var
                                                       arglists-for-top-level-var
                                                       alternative-arglists
                                                       separate-fixed-from-rest-arglists
                                                       uniquify
                                                       gen-unique-alias-name
                                                       #?@(:clj [macro-symbol?])
                                                       imported-symbol?
                                                       register-alias
                                                       top-level-self-reference?
                                                       ]]
            [clojure.core.typed.annotator.pprint :refer [pprint pprint-str-no-line
                                                         unp-str]]
            [clojure.core.typed.annotator.parse :refer [parse-type prs]]
            [clojure.walk :as walk]
            #?@(:clj [[clojure.tools.namespace.parse :as nprs]
                      [clojure.tools.reader.reader-types :as rdrt]
                      [clojure.java.io :as io]
                      [clojure.core.typed.ast-utils :as ast]
                      [clojure.tools.analyzer.passes.jvm.emit-form :as emit-form]
                      [clojure.core.typed.analyzer.jvm :as jana2]
                      [clojure.core.typed.annotator.insert :as insert
                       :refer [replace-generated-annotations]]
                      [clojure.core.typed.coerce-utils :as coerce]])))


#_
(defalias Type
  (U '{:op :val :val Any}
     (HMap
       :mandatory
       {:op :HMap 
        :clojure.core.typed.annotator.rep/HMap-req (Map Kw Type)}
       :optional
       {:clojure.core.typed.annotator.rep/HMap-opt (Map Kw Type)})

     '{:op :HVec :vec (Vec Type)}
     '{:op :union :types (Set Type)}
     '{:op :unresolved-class 
       ::class-string String
       :args (Vec Type)}
     '{:op :class 
       :clojure.core.typed.annotator.rep/class-instance (U Keyword String)
       :args (Vec Type)}
     '{:op :IFn
       :arities (Vec (HMap
                       :mandatory {:op :IFn1
                                   :dom (Vec Type)
                                   :rng Type}
                       :optional {:rest Type}))}
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

(defn init-config []
  {})

(defn init-env []
  {}
  #_{(current-ns)
   {:type-env {}
    :alias-env {}}})

(declare get-infer-results unparse-infer-result)

(defn ppresults 
  ([] (ppresults (get-infer-results results-atom)))
  ([infer-results]
   (pprint (mapv unparse-infer-result infer-results))))

(defn swap-infer-results! [results-atom f & args]
  (apply swap! results-atom update :infer-results f args))

(defn get-infer-results [results-atom]
  (get @results-atom :infer-results))

(defn equiv-result [ps cls]
  {:pre [(set? ps)
         (= 2 (count ps))
         (keyword? cls)]}
  {:op :equiv
   := ps
   :type cls})

(def -unknown {:op :unknown})

(defn make-free [name]
  {:pre [(symbol? name)]}
  {:op :free
   :name name})

(declare parse-path-elem)

(defn parse-infer-result [[p _ t]]
  {:path (mapv parse-path-elem p)
   :type (parse-type t)})

(declare unparse-path-elem)

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

(def ^:dynamic *new-aliases* nil)

(declare postwalk)

(defn HMap-req-opt-keysets [t]
  {:pre [(HMap? t)]
   :post [(set? %)]}
  #{{:req-keyset (map-key-set (:clojure.core.typed.annotator.rep/HMap-req t))
     :opt-keyset (map-key-set (:clojure.core.typed.annotator.rep/HMap-opt t))}})

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
                            #{(:clojure.core.typed.annotator.rep/HMap-req m)})))

(defn subst-alias [t old new]
  {:pre [(type? t)
         (alias? old)
         (type? new)]
   :post [(type? %)]}
  ;(prn "subst-alias t" (unparse-type t))
  ;(prn "old" (unparse-type old)) ;(prn "new" (unparse-type new))
  (if (= old new)
    t
    (do 
      ;(apply prn "renaming" (:name old) 
      ;       (when (alias? new)
      ;         [(str "to " (:name new))]))
      (postwalk t
                (fn [c]
                  (case (:op c)
                    :alias (if (= (:name c) (:name old))
                             new
                             c)
                    c))))))

(defn rename-alias [env old new]
  {:pre [(symbol? old)
         (symbol? new)]}
  ;(prn "rename-alias" old "->" new)
  (debug-when :squash-horizontally (str "squash-vertically: renaming alias " old " -> " new))
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


(def ^:dynamic *unparse-abbrev-alias* false)
(def ^:dynamic *unparse-abbrev-class* false)

(defn resolve-class [c]
  {:pre []
   :post [(symbol? %)]}
  (assert (string? c) c)
  (symbol c)
  ;;FIXME add back
  #_
  (symbol
    (if (= (ns-resolve (current-ns) (symbol (.getSimpleName c)))
           c)
      (.getSimpleName c)
      (.getName c))))

(declare resolve-alias-or-nil)

(defn should-gen-just-in-time-alias? [t]
  (or (HMap? t)
      (alias? t)))

(declare register-just-in-time-alias)

(defn HMap-has-tag-key? [m k]
  (kw-val? (get (:clojure.core.typed.annotator.rep/HMap-req m) k)))



; [Node :-> Any]
(defn unparse-type' [{:as m}]
  (assert (type? m) m)
  (case (:op m)
    :alias (do
             (when-let [used-aliases *used-aliases*]
               (swap! used-aliases conj (:name m)))
             (if *unparse-abbrev-alias*
               (-> (:name m) name symbol)
               (if (= (some-> (namespace (:name m)) symbol)
                      (current-ns))
                 (symbol (name (:name m)))
                 (:name m))))
    :val (let [t (:val m)]
           (cond
             ((some-fn nil? false?) t) t
             (keyword? t) `'~t
             :else (qualify-typed-symbol 'Any)))
    :union (cond
             (empty? (:types m))
             (qualify-typed-symbol 'Nothing)

             ;; deterministic print order based on dispatch key
             (every? HMap? (:types m))
             (let [ts (:types m)
                   k (HMap-likely-tag-key ts)
                   ts (if (and k (every? #(HMap-has-tag-key? % k) ts))
                        (sort-by (fn [m]
                                   {:post [(keyword? %)]}
                                   (:val (get (:clojure.core.typed.annotator.rep/HMap-req m) k)))
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
                        ts))))
    :HVec `'~(mapv unparse-type (:vec m))
    :HMap (let [{:keys [:clojure.core.typed.annotator.rep/HMap-req :clojure.core.typed.annotator.rep/HMap-opt]} m
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
              :else `'~req))
    :IFn1 (let [{:keys [dom rest rng fixed-name-lookup]} m
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
            (into (mapv (fn [t nme]
                          (if (and nme *new-aliases*
                                   (should-gen-just-in-time-alias? t))
                            (let [sym (register-just-in-time-alias (camel-case nme) t)]
                              (unparse-type (-alias sym)))
                            (unparse-type t)))
                        dom
                        (or these-names
                            (repeat (count dom) nil)))
                  (concat (when rest
                            ;; TODO lookup for rest names
                            [(unparse-type rest) '*])
                          [:-> (unparse-type rng)])))
    :IFn (let [{:keys [arities top-level-def]} m
               top-level-var (find-top-level-var top-level-def)
               ;_ (prn "top-level-var" top-level-def top-level-var)
               arglists (arglists-for-top-level-var top-level-var)
               ;_ (prn "arglists" arglists)
               macro? (some-> top-level-var meta :macro)
               {fixed-arglists :fixed [rest-arglist] :rest} (separate-fixed-from-rest-arglists arglists)
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
           (let [as (mapv (fn [a]
                           (unparse-type
                            (assoc a
                                   :fixed-name-lookup fixed-name-lookup)))
                         arities)]
             (if (== 1 (count as))
               (first as)
               (list* (qualify-typed-symbol 'IFn) as))))
    :class (letfn [(unparse-class [c args]
                     (let [cls (condp = c
                                 :array 'Array
                                 :map (qualify-typed-symbol 'Map)
                                 :vector (qualify-typed-symbol 'Vec)
                                 :set (qualify-typed-symbol 'Set)
                                 :symbol (qualify-typed-symbol 'Sym)
                                 :keyword (qualify-typed-symbol 'Kw)
                                 :atom (qualify-typed-symbol 'Atom1)
                                 :seq (qualify-typed-symbol 'Coll)
                                 :list (qualify-typed-symbol 'Coll)
                                 :coll (qualify-typed-symbol 'Coll)
                                 :seqable (qualify-typed-symbol 'Seqable)
                                 :double (qualify-typed-symbol 'Num)
                                 :decimal (qualify-typed-symbol 'Num)
                                 :number  (qualify-typed-symbol 'Num)
                                 :char (resolve-class "java.lang.Character")
                                 :int (qualify-typed-symbol 'Int)
                                 :integer (qualify-typed-symbol 'Int)
                                 :ifn 'AnyFunction
                                 :string (qualify-typed-symbol 'Str)
                                 :boolean (qualify-typed-symbol 'Bool)
                                 (resolve-class c))
                           _ (assert (symbol? cls))]
                       (if (seq args)
                         (list*-force cls (map unparse-type args))
                         cls)))]
             (unparse-class (:clojure.core.typed.annotator.rep/class-instance m) (:args m)))
    :Top (qualify-typed-symbol 'Any)
    :unknown (cond 
               *preserve-unknown* '?
               :else (qualify-typed-symbol 'Any))
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

(defn update-grouped-paths [env {:keys [:spec?] :as config} {:keys [:current-level :inner-level] :as paths}]
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
                   :var (throw (ex-info "Var path element must only be first path element" {}))
                   :key (let [{:keys [kw-entries keys key]} cur-pth
                              forbidden-aliases *forbidden-aliases*
                              _ (when (and spec? forbidden-aliases)
                                  (swap! forbidden-aliases into (map kw->sym (cons key keys))))]
                          {:op :HMap
                           :clojure.core.typed.annotator.rep/HMap-req (merge (zipmap keys (repeat {:op :unknown}))
                                             ;; immediately associate kw->kw entries
                                             ;; to distinguish in merging algorithm
                                             kw-entries
                                             {key type})
                           :clojure.core.typed.annotator.rep/HMap-opt {}})
                   :set-entry (-class :set [type])
                   :seq-entry (-class :seq [type])
                   :vec-entry (-class :vector [type])
                   :map-keys (-class :map [type {:op :unknown}])
                   :map-vals (-class :map [{:op :unknown} type])
                   :transient-vector-entry (-class "clojure.lang.ITransientVector" [type])
                   :atom-contents (-class :atom [type])
                   :index (if true #_(= 2 (:count cur-pth))
                            {:op :HVec
                             :vec (assoc (vec (repeat (:count cur-pth) -unknown)) (:nth cur-pth) type)}
                            #_(-class :vector [type]))
                   :fn-domain (let [{:keys [arity position]} cur-pth]
                                {:op :IFn
                                 :arities [{:op :IFn1
                                            :dom (let [dom (into [] 
                                                                 (repeat (:arity cur-pth) {:op :unknown}))]
                                                   (if (zero? arity)
                                                     dom
                                                     (assoc dom position type)))
                                            :rest nil
                                            :rng {:op :unknown}}]})
                   :fn-range (let [{:keys [arity]} cur-pth]
                               {:op :IFn
                                :arities [{:op :IFn1
                                           :dom (into [] (repeat (:arity cur-pth) {:op :unknown}))
                                           :rest nil
                                           :rng type}]})))))))))

(defn collapse-likely-rest-arguments [t]
  {:pre [(type? t)]
   :post [(type? t)]}
  ;; no :rest arguments at this point
  (or
    (when (and (#{:IFn} (:op t))
               (not-any? :rest (:arities t)))
      (let [{:keys [top-level-def]} t
            top-level-var (find-top-level-var top-level-def)]
        (when top-level-var
          (let [rest-arglist (-> top-level-var
                                 arglists-for-top-level-var
                                 separate-fixed-from-rest-arglists
                                 :rest
                                 first)]
            (when rest-arglist
              (let [fixed-arglist (subvec rest-arglist 0 (- (count rest-arglist) 2))
                    rst-arg (peek rest-arglist)
                    {:keys [separate collapse]} (group-by (fn [{:keys [dom] :as a}]
                                                            (if (<= (count fixed-arglist) (count dom))
                                                              :collapse
                                                              :separate))
                                                          (:arities t))
                    trimmed-collapsed (mapv (fn [{:keys [dom] :as a}]
                                              {:pre [(<= (count fixed-arglist) (count dom))]}
                                              (let [[fixed rst] (split-at (count fixed-arglist) dom)]
                                                (assoc a
                                                       :dom (vec fixed)
                                                       :rest (when (seq rst)
                                                               (apply join* rst)))))
                                            collapse)]
                (merge t
                       {:arities (into (vec separate)
                                       (when (seq trimmed-collapsed)
                                         [(join-IFn1 trimmed-collapsed)]))})))))))
    t))

(defn grouped-paths-to-env [env config {:keys [:current-level :inner-level] :as paths}]
  {:pre [(empty? current-level)
         (map? inner-level)]}
  (into {}
        (map (fn [[pth next-group]]
               {:pre [(#{:var} (:op pth))]}
               (let [v (:name pth)
                     t (update-grouped-paths env config next-group)
                     t (collapse-likely-rest-arguments
                         (merge t
                                (when (#{:IFn} (:op t))
                                  {:top-level-def v})))]
                 [v t])))
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
                (update :clojure.core.typed.annotator.rep/HMap-req up)
                (update :clojure.core.typed.annotator.rep/HMap-opt up)))
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
              (update :rest (fn [a] (some-> a f)))
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

(defn register-unique-alias [env config sym t]
  {:pre [(not (namespace sym))]}
  ;(debug (println "register-unique-alias:" sym (unp-str t))
  (let [sym (gen-unique-alias-name env config sym)]
    [sym (register-alias env config sym t)])
  ;)
)

(defn resolve-alias-or-nil [env {:keys [name] :as a}]
  {:pre [(map? env)
         (alias? a)
         (symbol? name)]
   :post []}
  ;(prn "resolve-alias" name (keys (alias-env env)))
  (get (alias-env env) name))

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
  (let [singles (into {} (filter (comp kw-vals? val) (:clojure.core.typed.annotator.rep/HMap-req t)))]
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
                                                (:clojure.core.typed.annotator.rep/HMap-req t)))
                                      ;; information from a union takes precedence
                                      [k v] (or (when-let [upper-k (::union-likely-tag (meta t))]
                                                  (let [e (find (:clojure.core.typed.annotator.rep/HMap-req t) upper-k)]
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
                                              (when (<= (+ (count (:clojure.core.typed.annotator.rep/HMap-req t))
                                                           (count (:clojure.core.typed.annotator.rep/HMap-opt t)))
                                                        2)
                                                (apply str (interpose "-" (map name (concat (keys (:clojure.core.typed.annotator.rep/HMap-req t))
                                                                                            (keys (:clojure.core.typed.annotator.rep/HMap-opt t)))))))
                                              ;; otherwise give abbreviated keys
                                              (apply str (interpose "-" 
                                                                    (map (fn [k]
                                                                           (apply str (take 3 (name k))))
                                                                         (concat
                                                                           (keys (:clojure.core.typed.annotator.rep/HMap-req t))
                                                                           (keys (:clojure.core.typed.annotator.rep/HMap-opt t)))))))))
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

(defn alias+merge-single-HMaps
  "Traverse the type and alias environments
  and ensure all HMaps are aliased. Reuse
  existing aliases when :mandatory keysets are identical.
  Never merge HMaps with different likely tag values."
  [env config]
  {:post [(map? %)]}
  (letfn [(do-alias [env-atom t]
            {:pre [(HMap? t)]}
            (let [a-atom (atom nil)
                  _ (swap! env-atom 
                           (fn [env]
                             (let [existing-aliases (get-in env [::alias-keyset-cache (HMap-req-keyset t)])
                                   compatible-tag? (fn [[_ existing-t]]
                                                     (if-let [likely-tag
                                                              (when existing-t
                                                                (HMap-likely-tag-key [existing-t t]))]
                                                       (= (get-in existing-t [:clojure.core.typed.annotator.rep/HMap-req likely-tag])
                                                          (get-in t [:clojure.core.typed.annotator.rep/HMap-req likely-tag]))
                                                       true))
                                   compatible-alias-entry 
                                   (first (filter compatible-tag?
                                                  (zipmap
                                                    existing-aliases
                                                    (map #(resolve-alias env (-alias %)) existing-aliases))))
                                   [a env] (if-let [[a a-t] compatible-alias-entry]
                                             [a (register-alias env config a (join t a-t))]
                                             (let [n (gensym 'HMap)]
                                               [n
                                                (register-alias env config n t)]))
                                   new-t (resolve-alias env (-alias a))
                                   _ (assert (not (top-level-self-reference? env new-t a)))
                                   ;; don't care if retried
                                   _ (reset! a-atom a)]
                               ;; update alias cache
                               (-> env
                                   (update-in [::alias-keyset-cache (HMap-req-keyset new-t)] (fnil conj #{}) a)))))]
              (-alias @a-atom)))
          (single-HMap [env t]
            {:pre [(type? t)]}
            (let [env-atom (atom env)
                  t (postwalk 
                      t
                      (fn [t]
                        (case (:op t)
                          :HMap (do-alias env-atom t)
                          t)))]
              [t @env-atom]))]
    (let [env (reduce
                (fn [env [v t]]
                  (let [[t env] (single-HMap env t)]
                    (update-type-env env assoc v t)))
                (assoc env
                       ::alias-keyset-cache {}
                       ::alias-tag-val-cache {})
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
      (dissoc env
              ::alias-keyset-cache
              ::alias-tag-val-cache))))


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
                                                [:clojure.core.typed.annotator.rep/HMap-req :clojure.core.typed.annotator.rep/HMap-opt])]
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
                                                        (map (comp set keys :clojure.core.typed.annotator.rep/HMap-req) ts))
                                          common-tag (first
                                                       (filter
                                                         (fn [k]
                                                           (every? (fn [m]
                                                                     {:pre [(HMap? m)]}
                                                                     (kw-val? (get (:clojure.core.typed.annotator.rep/HMap-req m) k)))
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
                                                                             :clojure.core.typed.annotator.rep/HMap-req)
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

(declare unparse-defalias-entry)


; try-merge-aliases : Env Config Sym Alias -> Env
(defn try-merge-aliases [env config f t]
  {:pre [(map? env)
         (symbol? f)
         (alias? t)]
   :post [((con/hvector-c? (con/vec-c? (con/hmap-c? :old symbol? :new symbol?))
                           map?)
           %)]}
  ;(prn "Try merging" f
  ;     "with" (:name t))
  (let [tks (keysets env t)
        fks (keysets env (-alias f))]
    ;(prn "merging keysets?"
    ;     tks fks)
    (cond
      (and (not= (:name t) f)
           (let [ft (resolve-alias env (-alias f))
                 tt (resolve-alias env t)]
             (and (HMap? ft)
                  (HMap? tt)
                  (some? (HMap-likely-tag-key [ft tt])))))
      (let [old-env env
            env (update-alias-env env
                           (fn [m]
                             (-> m 
                                 (assoc f t)
                                 (update (:name t)
                                         (fn [oldt]
                                           {:pre [(type? oldt)]}
                                           ;; Rename both old aliases to point to t
                                           ;;
                                           ;; eg. Say we're merging B into A.
                                           ;;
                                           ;;  (defalias A (U nil B))
                                           ;;  (defalias B (U nil B))
                                           ;;
                                           ;;  First we rewrite *both* aliases to have occurrences
                                           ;;  of B renamed to A.
                                           ;;
                                           ;;  (defalias A (U nil A))
                                           ;;                     ^
                                           ;;
                                           ;;  (defalias B (U nil A))
                                           ;;                     ^
                                           ;;
                                           ;; Then join like usual into A:
                                           ;;
                                           ;;  (defalias A (U nil A))
                                           ;;
                                           ;; If we don't first rewrite, we'll have extra references
                                           ;; to B in our types which will increase the width of our
                                           ;; unions unnecessarily, which is a major 
                                           ;;
                                           ;; eg. if we only rewrote references to B in A:
                                           ;;
                                           ;;  (defalias A (U nil A))
                                           ;;                     ^
                                           ;;  (defalias B (U nil B))
                                           ;;
                                           ;;  then joining gives us:
                                           ;;
                                           ;;  (defalias A (U nil B A))
                                           (let [new-type (join
                                                            (subst-alias (get m f) (-alias f) t)
                                                            (subst-alias oldt (-alias f) t))]
                                             ;(prn "new-type" (unparse-type new-type))
                                             new-type))))))
            _ (debug-squash (str "Merging " f " into " (:name t)
                                 "\nOld aliases:\n"
                                 (with-out-str
                                   (pprint (mapv unparse-defalias-entry (select-keys (alias-env old-env) [f (:name t)]))))
                                 "\nNew alias:\n"
                                 (with-out-str 
                                   (pprint (unparse-defalias-entry (find (alias-env env) (:name t)))))))
            _ (assert (not (top-level-self-reference? env (resolve-alias env t) (:name t)))
                      (str (unparse-type t) " refers to an infinite type: " (unparse-type (resolve-alias env t))))
            _ (assert (not (top-level-self-reference? env (resolve-alias env (-alias f)) f))
                      (str f " refers to an infinite type: " (unparse-type (resolve-alias env (-alias f)))))]
        [[{:new (:name t)
           :old f}]
         env])

      :else [[] env])))

(defn print-pretty-tree
  ([lvl env ptree]
   (doseq [[k ptree] ptree]
     (println (str (apply str (repeat lvl " "))
                   (when-not (zero? lvl)
                     "\\")
                   "+" k
                   " " #_(keysets env (-alias k))))
     (print-pretty-tree (inc lvl) env ptree))))

; (defalias PrettyTree '{:root Sym :nodes (Map Sym (Vec Sym))})
;
; [Sym -> PrettyTree]
(defn empty-pretty-tree [root]
  {:pre [(symbol? root)]}
  {root {}})

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
  #_(prn "top squash aliases" (keys (alias-env env))
       #_(fv env t true))
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
            _ (assert (alias? t) [t #?(:clj (class t))])
            ;_ (prn "squash" (unp t))
            ;; find all keysets for downstream (and upstream) aliases
            ;; and merge.
            next-immediate-aliases (fv env (resolve-alias env t))
            seen-parents (map :name done)
            [renames env] (if-not (done t)
                            (reduce 
                              (fn [[renames env] f]
                                (let [[rs env] (try-merge-aliases env config f t)]
                                  ;; this debug doesn't work too well because the root alias
                                  ;; is not referred to in the env yet
                                  #_(when-first [{:keys [new old]} rs]
                                    (debug-squash
                                      (str "New env after merging " old " into " new "\n"
                                           (with-out-str
                                             (pprint-env env config)))))
                                  [(into renames rs) env]))
                              [[] env]
                              (set 
                                (concat
                                  ;; also try and merge with parents
                                  seen-parents
                                  next-immediate-aliases)))
                            [[] env])
            ; recalculate with new env
            new-next-immediate-aliases (fv env (resolve-alias env t))
            add-to-worklist (set/difference
                              (into #{}
                                    (map -alias)
                                    new-next-immediate-aliases)
                              done
                              #{t})
            next-worklist (into (subvec worklist 1) add-to-worklist)]
        (recur env
               next-worklist
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
               ;; don't inline if it refers to other aliases (and so, might be recursive).
               ;; too expensive to fully determine.
               res (empty? (fv env a-res))
               ;extremely slow
               #_(not (contains? (set (fv env a-res true)) (:name a)))
               ]
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
                        ;(prn "follow-aliases-in-alias-env"
                        ;     (str "rewriting " f
                        ;          ": " "originally " (unparse-type (resolve-alias env (-alias f)))
                        ;          ", now replacing " inner " with " 
                        ;          (unparse-type substt)
                        ;          (when (top-level-self-reference? env (resolve-alias env (-alias f)) f))
                        ;            " (the original binding is also infinite, so this happened somewhere else)"))
                        ;; FIXME often inner == substt, avoid this
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

#?(:clj
(defn dummy-do [env statements ret]
  {:pre [((some-fn nil? vector) statements)]}
  {:op :do
   :form '(do)
   :env env
   :statements statements
   :ret ret
   :children [:statements :ret]}))

#?(:clj
(defn dummy-let [env bindings body]
  {:op :let
   :form '(let)
   :env env
   :bindings bindings
   :body (assoc body :body? true)
   :children [:bindings :body]}))

; dummy-sym : Env Sym -> TAExpr
#?(:clj
(defn dummy-sym [env vsym]
  {:pre [(symbol? vsym)]}
  {:op :const
   :type :symbol
   :form `'~vsym
   :env env
   :val vsym}))

#?(:clj
(defn dummy-kw [env kw]
  {:pre [(keyword? kw)]}
  {:op :const
   :type :keyword
   :form kw
   :env env
   :val kw}))

#?(:clj
(defn dummy-num [env n]
  {:op :const
   :type :number
   :form n
   :env env
   :val n}))

#?(:clj
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
   :val m}))

; wrap-var-deref : TAExpr Sym Namespace -> TAExpr
#?(:clj
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
    invoke-ast)))

; wrap-def-init : TAExpr Sym Namespace -> TAExpr
#?(:clj
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
            expr]})))

#?(:clj
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
            expr]})))

(def ^:dynamic *found-fn* false)

#?(:clj
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
    nil)))

;; Only wrap library imports so we can infer how they are used.
;; Also wrap :dynamic vars since they can be rebound at runtime
;; and lose instrumentation.
#?(:clj
(defn should-wrap-var? [v]
  (let [vsym (impl/var->symbol v)
        vns (symbol (namespace vsym))
        excluded? (contains? (conj ns-exclusions (ns-name *ns*)) vns)
        ;dynamic? (-> (:var expr) meta :dynamic)
        no-infer? (-> v meta :clojure.core.typed/no-infer)
        should-infer? (-> v meta :clojure.core.typed/infer)
        should-wrap? (or should-infer? (not (or excluded? no-infer?)))]
    ;(prn "should-wrap-var?" v should-infer? excluded? no-infer? should-wrap?)
    should-wrap?)))

#?(:clj
(defn wrap-var-expr [expr]
  (if (should-wrap-var? (:var expr))
    (wrap-var-deref expr (impl/var->symbol (:var expr)) *ns*)
    expr)))

; check : (IFn [TAExpr -> TAExpr] [TAExpr CTType -> TAExpr]
#?(:clj
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
                  no-infer? (or (some-> v meta :clojure.core.typed/no-infer)
                                ;; if we enable macros, make sure we don't traverse
                                ;; &env or &form -- &env at least pollutes HMaps.
                                (some-> v meta :macro))
                     ]
              (when no-infer?
                (println (str "Not instrumenting " (ast/def-var-name expr) " definition"))
                (flush))
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
                         (when-let [coord (:clojure.core.typed/auto-ann m)]
                           ;(prn ":let coord" coord
                           ;     (skip-track? coord)
                           ;     (:clojure.core.typed/track-kind m))
                           (when-not (skip-track? coord)
                             (case (:clojure.core.typed/track-kind m)
                               (:clojure.core.typed/for-return :clojure.core.typed/for-param)
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

       (ast/walk-children check expr))))))

#?(:clj
(def runtime-infer-expr check))

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

(declare pprint-env)

#?(:clj
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
      #_(prn "N hashes" (count hash-occurrences))))))


; generate-tenv : InferResultEnv -> AliasTypeEnv
(defn generate-tenv
  "Reset and populate global type environment."
  [env config {:keys [infer-results :call-flows] :as is}]
  (println "generate-tenv:"
                  (str (count infer-results) " infer-results"))
  (let [by-path (group-by-path infer-results)]
    (as-> (init-env) env 
      (update-env env assoc :type-env (grouped-paths-to-env env config by-path)))))

(defn reachable-aliases 
  "Returns all referenced aliases from the type
  environment."
  [env]
  {:pre [(map? env)]
   :post [(set? %)
          (every? symbol? %)]}
  (set (keys (alias-env env)))
  ;; way too expensive
  #_
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
                  ts (map #(fully-resolve-alias env (-alias %)) as)
                  ;_ (prn "ts with anew" (mapv unp ts))
                  ;; remove all top-level alias refs to as, otherwise
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
              _ (assert (not (top-level-self-reference? env newtyp anew))
                        (str anew " refers to an infinite type: " (unparse-type anew)))
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
                                                                       (concat (keys (:clojure.core.typed.annotator.rep/HMap-req hmap))
                                                                               (keys (:clojure.core.typed.annotator.rep/HMap-opt hmap))))))
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
                                     (:clojure.core.typed.annotator.rep/HMap-req t)))]))
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

;; group all aliases by their likely tag key.
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

#?(:clj
(defmacro debug-output [msg env config]
  `(when (or (= :iterations *debug*)
             (when (set? *debug*)
               (contains? *debug* :iterations)))
     (let [env# ~env
           config# ~config
           msg# ~msg]
       (println)
       (println (str "ITERATION: " msg#))
       (pprint-env env# config#)))))

#?(:clj
(defmacro debug-output-when [debug-state msg env config]
  `(when (and (set? *debug*)
              (contains? *debug* ~debug-state))
     (let [env# ~env
           config# ~config
           msg# ~msg]
       (println)
       (println (str "ITERATION: " msg#))
       (pprint-env env# config#)))))

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
                                         (let [new-a (gen-unique-alias-name env config (symbol (camel-case (name k))))]
                                           (rename-alias env a new-a))
                                         env))
                                     env))
                          ;; this is not a tagged map
                          :HMap (let [relevant-names (map
                                                       camel-case
                                                       (concat
                                                         (sort
                                                           (map name (keys (:clojure.core.typed.annotator.rep/HMap-req t))))
                                                         (sort
                                                           (map name (keys (:clojure.core.typed.annotator.rep/HMap-opt t))))))
                                      a-new (gen-unique-alias-name
                                              env config
                                              (symbol
                                                (if (empty? relevant-names)
                                                  "EmptyMap"
                                                  (apply str (concat (map camel-case (take 3 relevant-names)) ["Map"])))))]
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
  (let [;; FIXME reachable-aliases is very slow, refactor it away
        as (reachable-aliases env)
        env (remove-unreachable-aliases env config as)

        ;; merge HMaps with similar keysets, excluding differently-tagged maps.
        asets (group-similar-HMap-aliases-by-req-keysets env as)
        ;; merge-aliases does not introduce new aliases, so `as`
        ;; is still the set of reachable aliases after this line.
        env (reduce (fn [env as]
                      (let [env (merge-aliases env as)]
                        (when (seq (next as))
                          (debug-output-when :squash-horizontally
                                             (str "Merged aliases " (vec (rest as))
                                                  " into " (first as))
                                             env
                                             config))
                        env))
                    env asets)

        ;; merge HMaps on their tags key/val pairs.
        asets (group-HMap-aliases-by-likely-tag env as)
        env (reduce (fn [env as]
                      (let [env (merge-aliases env as)]
                        (when (seq (next as))
                          (debug-output-when :squash-horizontally
                                             (str "Merged aliases " (vec (rest as))
                                                  " into " (first as)
                                                  " since they have the same likely tag values")
                                             env
                                             config))
                        env))
                    env asets)

        ;; collect and group all HMaps with the
        ;; same tag key. This is pretty aggressive ---
        ;; it effectively upcasts any specific tagged
        ;; HMap to its "parent" (the union of all maps
        ;; with the same tag key).
        asets (group-aliases-by-likely-tag-key env as)
        env (reduce (fn [env as]
                      (let [env (merge-aliases env as)]
                        (when (seq (next as))
                          (debug-output-when :squash-horizontally
                                             (str "Merged aliases " (vec (rest as))
                                                  " into " (first as)
                                                  " since they have the same likely tag keys")
                                             env
                                             config))
                        env))
                    env asets)

        ;; merge HMaps on their tags key/val pairs.
        ;asets (group-HMap-aliases-by-overlapping-qualified-keys env as)
        ;env (reduce merge-aliases env asets)

        as (reachable-aliases env)
        ;; remove unreachable aliases
        env (remove-unreachable-aliases env config as)

        ;; delete intermediate aliases
        env (follow-all env (assoc config :simplify? false))

        _ (debug-output-when :squash-horizontally "before rename-HMap-aliases" env config)
        ;; rename aliases pointing to HMaps
        env (rename-HMap-aliases env config)
        _ (debug-output-when :squash-horizontally "after rename-HMap-aliases" env config)
        ]
    env))

(declare envs-to-annotations
         )

(defn pprint-env [env {:keys [spec?] :as config}]
  (let [m ((if spec?
             envs-to-specs
             envs-to-annotations)
           env config)
        m (update m :local-fns #(mapv (fn [f] (update f :type unparse-type)) %))]
    (pprint m)))


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

#?(:clj
(defmacro when-fuel [env & body]
  `(let [env# ~env]
     (if (enough-fuel? env#)
       (dec-fuel (do ~@body))
       env#))))

(defn squash-vertically [env config]
  {:pre [(map? env)]}
  (reduce
    (fn [env [v t]]
      (debug-when :iterations (str "squash-vertically: " v))
      (let [;; create graph nodes from HMap types
            [t env] (alias-hmap-type env config t)
            _ (debug-output-when
                :squash-vertically
                (str "squash-vertically step 1: alias-hmap-type for " v)
                (update-type-env env assoc v t)
                config)
            ;; squash local recursive types
            [t env] (squash-all env config t)
            _ (debug-output-when
                :squash-vertically
                (str "squash-vertically step 2: squash-all for " v) 
                (update-type-env env assoc v t)
                config)
            ;; trim redundant aliases in local types
            [t env] (follow-aliases env (assoc config :simplify? true) t)
            _ (debug-output-when
                :squash-vertically
                (str "squash-vertically step 3: follow-aliases for " v) 
                (update-type-env env assoc v t)
                config)
            ;_ (prn "new type for" v (unparse-spec t))
            ]
        (update-type-env env assoc v t)))
    env
    (type-env env)))

(defn populate-envs [env {:keys [spec? no-squash-vertically] :as config}]
  {:pre [(map? env)]}
  (debug "populate-envs:"
  (let [;; create recursive types
        env (if-let [fuel (:fuel config)]
              (assoc env :fuel fuel)
              env)
        ;; give all HMaps an alias, and eagerly merge aliases
        ;; with identical :mandatory keysets. Don't merge HMaps
        ;; with different likely tag values.
        env (when-fuel env
              (alias+merge-single-HMaps env config))
           _ (debug-output "after alias+merge-single-HMaps" env config)
        env (if no-squash-vertically
              env
              (let [_ (debug-output "top of populate-envs" env config)
                    #?@(:cljs [_ (assert (map? env) [(pr-str (type env)) env])])
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

(defn unparse-defalias-entry [[k v :as e]]
  {:pre [e]}
  (list (qualify-typed-symbol 'defalias)
        (symbol (name k))
        (unparse-type v)))

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
        tenv #?(:clj (into {}
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
                :cljs full-type-env)
        as (alias-env env)]
    (binding [*envs* (atom env)
              ;; disable for now
              *new-aliases* nil #_(atom (init-env))]
      (letfn [(unp-defalias-env [as]
                (mapv unparse-defalias-entry
                      (sort-by (comp name first) as)))
              (declares-for-aliases [as]
                (list* #?(:clj (if (= (ns-resolve (current-ns) 'declare)
                                      #'clojure.core/declare)
                                 'declare
                                 'clojure.core/declare)
                          :cljs 'declare)
                       (sort (map (comp symbol name) as))))
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
                                     (unparse-type
                                       (assoc v :top-level-def k)))]
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
        (let [used-aliases-in-anns (atom #{})
              anns (binding [*used-aliases* used-aliases-in-anns]
                     (unp-anns tenv))
              [declares defaliases] (loop [worklist (vec @used-aliases-in-anns)
                                           done #{}
                                           declares []
                                           defaliases []]
                                      (if (empty? worklist)
                                        [(declares-for-aliases declares) defaliases]
                                        (let [a (nth worklist 0)]
                                          (if (done a)
                                            (recur (subvec worklist 1)
                                                   done
                                                   declares
                                                   defaliases)
                                            (let [add-to-worklist (atom #{})
                                                  e (binding [*used-aliases* add-to-worklist]
                                                      (unparse-defalias-entry (find as a)))]
                                              (recur (into (subvec worklist 1) 
                                                           (remove done)
                                                           @add-to-worklist)
                                                     (conj done a)
                                                     (conj declares a)
                                                     (conj defaliases e)))))))]
          {:requires (when-let [requires (:explicit-require-needed config)]
                       [requires])
           :top-level
           (into [declares] 
                 (concat defaliases anns))
           :local-fns (mapv (fn [[k v]]
                              (assoc (meta k) :type v))
                            local-fn-env)})))))


(defn var-constraints 
  "Return the bag of constraints in the current results-atom
  for the given fully qualified var.
  
  eg. (var-constraints 'clojure.core.typed.annotator.test.mini-occ/parse-exp)
  "

  [vsym]
  (pprint (mapv unparse-infer-result 
                (-> (->> (get-infer-results results-atom) (group-by (comp :name first :path))) 
                    (get vsym)))))


#?(:clj
(defn infer-anns
  ([ns {:keys [spec? results-atom] :as config :or {results-atom results-atom}}]
   {:pre [(or (instance? clojure.lang.Namespace ns)
              (symbol? ns))]}
   (binding [*forbidden-aliases* (when spec?
                                   (atom #{}))]
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
           infer-results (if-let [load-infer-results nil #_(:load-infer-results config)]
                           (do (println (str "Loading inference results from " load-infer-results))
                             (read-string (slurp load-infer-results)))
                           @results-atom)
           _ (assert (infer-results? infer-results))
           #_#_
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
                                                                                    (let [^Class inst (:clojure.core.typed.annotator.rep/class-instance c)
                                                                                          _ (assert (class? inst))
                                                                                          nme (.getName inst)]
                                                                                      (and (not (.startsWith nme "clojure.lang"))
                                                                                           (not (.startsWith nme "java.lang")))))
                                                                                  {:op :unresolved-class
                                                                                   ::class-string (let [^Class inst (:clojure.core.typed.annotator.rep/class-instance c)]
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
                       :call-flows (:call-flows infer-results)))))))))

#?(:cljs
(defn infer-cljs-anns
  [{:keys [spec? debug] :as config}]
  (binding [*ann-for-ns* (fn [] 'root)
            *debug* debug
            ]
    (let [infer-results @results-atom
          out (if spec? 
                envs-to-specs
                envs-to-annotations)]
      (-> (init-env)
          (generate-tenv config infer-results)
          (populate-envs config)
          (out config))))))

#?(:clj
(defn infer-with-frontend [front-end {:keys [ns fuel out-dir save-infer-results load-infer-results debug
                                             no-local-ann? polymorphic? spec-diff? no-squash-vertically
                                             spec-macros] :as args}]
  {:pre [((some-fn nil? string?) out-dir)
         ((some-fn nil? #{:all :iterations} set?) debug)]}
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
            unparse-type (if (= :spec front-end)
                           unparse-spec'
                           unparse-type')
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
                                     {:infer-anns infer-anns
                                      :no-local-ann? no-local-ann?
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
                                       {:fuel fuel}))))))

#?(:clj
(defn runtime-infer [args] (infer-with-frontend :type args)))
#?(:clj
(defn spec-infer [args] (infer-with-frontend :spec args)))

(defn refresh-runtime-infer []
  (reset! results-atom (initial-results))
  nil)

;; Runtime Instrumentation API

;(defn instrument-var [v]
;  )
;
;(defn instrument-ns [v]
;  )

#?(:clj
(defn instrument-top-level-form
  [form]
  (impl/with-clojure-impl
    (jana2/analyze+eval form (jana2/empty-env) {:eval-fn (fn [ast opts]
                                                           (-> ast
                                                               runtime-infer-expr
                                                               (jana2/eval-ast opts)))})))
)

;; TESTS

(comment

  (binding [*ns* (the-ns 'clojure.core.typed.annotator.test.mini-occ)]
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

(defmacro mapmacro []
  `(do {}))

(defntrack usemap-macro 
  [a]
  (mapmacro))

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
