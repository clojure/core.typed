(ns clojure.core.typed.annotator.frontend.spec
  (:require [clojure.core.typed.annotator.util
             :refer [qualify-spec-symbol kw-val? nil-val?
                     *preserve-unknown*
                     qualify-core-symbol
                     kw->sym
                     list*-force
                     current-ns
                     *used-aliases*
                     *multispecs-needed*
                     *envs*
                     fully-resolve-alias
                     HMap-likely-tag-key
                     alias-env
                     *forbidden-aliases*
                     find-top-level-var
                     arglists-for-top-level-var
                     separate-fixed-from-rest-arglists
                     core-specs-ns
                     uniquify
                     resolve-alias
                     gen-unique-alias-name
                     macro-symbol?
                     imported-symbol?
                     update-type-env
                     update-alias-env
                     register-alias
                     kw-vals?
                     type-env
                     ]]
            [clojure.core.typed.annotator.join :refer [make-Union join]]
            [clojure.core.typed.annotator.track :refer [local-fn-symbol?]]
            [clojure.core :as core]
            [clojure.core.typed.annotator.rep :as r]
  ))

(def ^:dynamic *higher-order-fspec* nil)

(defn unq-spec-nstr [] (str (current-ns)))

(declare unparse-spec')

(defn gen-unique-multi-spec-name [env multispecs sym]
  (if (or #?(:clj (resolve sym))
          (contains? multispecs sym))
    (gen-unique-multi-spec-name env multispecs (symbol (str (name sym) "__0")))
    sym))

(defn simplify-spec-alias [a]
  {:pre [(r/type? a)]
   :post [(r/type? %)]}
  (if (r/alias? a)
    (let [a-res (resolve-alias @*envs* a)]
      (if (and a-res (#{:class} (:op a-res)))
        a-res
        a))
    a))

(defn unparse-spec [m]
  (unparse-spec' m))

(defn alias->spec-kw [s]
  {:pre [(symbol? s)]
   :post [(keyword? %)]}
  (if (namespace s)
    (keyword s)
    (keyword (name (current-ns)) (name s))))

(defn spec-star [arg]
  (list (qualify-spec-symbol '*) arg))

(defn spec-cat [args]
  (assert (even? (count args)))
  (list*-force (qualify-spec-symbol 'cat) args))

;; Answers: is this a good alias to generate `s/keys`?
(defn alias-matches-key-for-spec-keys? [a k]
  {:pre [(r/alias? a)
         (keyword? k)]}
  (if (namespace k)
    (= (:name a) (kw->sym k))
    (= (name (:name a)) (name k))))

(declare or-spec)

(defn unparse-spec' [{:as m}]
  (assert (r/type? m) m)
  (case (:op m)
    :alias (do
             (when-let [used-aliases *used-aliases*]
               (swap! used-aliases conj (:name m)))
               (alias->spec-kw (:name m)))
    :val (let [t (:val m)]
           (cond
             (:clojure.core.typed.annotator.frontend.spec/implicit-alias m) (unparse-spec (:clojure.core.typed.annotator.frontend.spec/implicit-alias m))
             (nil? t) (qualify-core-symbol 'nil?)
             (false? t) (qualify-core-symbol 'false?)
             (keyword? t) #{t} #_(qualify-core-symbol 'keyword?)
             (string? t) (qualify-core-symbol 'string?)
             :else (qualify-core-symbol 'any?)))
    :union (if (:clojure.core.typed.annotator.frontend.spec/implicit-alias m)
             (unparse-spec (:clojure.core.typed.annotator.frontend.spec/implicit-alias m))
             (let [env *envs*
                   fully-res (if env
                               #(fully-resolve-alias @env %)
                               identity)
                   ts (map fully-res (:types m))]
               (if-let [tag (and env
                                 (every? r/HMap? ts)
                                 (HMap-likely-tag-key ts))]
                 ;; if we have a bunch of maps with a similar key,
                 ;; generate a multispec
                 (let [multispecs *multispecs-needed*
                       _ (assert multispecs)
                       _ (assert (map? @multispecs))
                       nme (gen-unique-multi-spec-name 
                             env @multispecs
                             (symbol (str (when-let [nstr (namespace tag)]
                                            (str nstr "-"))
                                          (name tag) "-multi-spec")))
                       dmulti (list
                                (qualify-core-symbol 'defmulti)
                                (with-meta nme
                                           {::generated true})
                                tag)
                       tag-for-hmap (fn [t]
                                      {:pre [(r/HMap? t)]}
                                      (let [this-tag (get (:clojure.core.typed.annotator.rep/HMap-req t) tag)
                                            _ (assert (kw-val? this-tag) (unparse-spec t))]
                                        (:val this-tag)))
                       dmethods (mapv (fn [t]
                                        {:pre [(r/HMap? t)]}
                                        (let [this-tag (tag-for-hmap t)]
                                          (list (qualify-core-symbol 'defmethod) 
                                                nme 
                                                this-tag
                                                ['_]
                                                (unparse-spec t))))
                                      (sort-by tag-for-hmap ts))
                       _ (when multispecs
                           (swap! multispecs assoc nme (vec (cons dmulti dmethods))))]
                   (list (qualify-spec-symbol 'multi-spec)
                         nme
                         tag))
                 (or-spec (:types m)))))
    :HVec (list* (qualify-spec-symbol 'tuple)
                 (mapv unparse-spec (:vec m)))
    :HMap (let [specify-keys 
                (fn [entries]
                  (->> entries
                       (map (fn [[k v]]
                              {:pre [(keyword? k)]}
                              (let [a (or (when (r/alias? v) v)
                                          (:clojure.core.typed.annotator.frontend.spec/implicit-alias v))]
                                (assert (and (r/alias? a)
                                             (alias-matches-key-for-spec-keys? a k))
                                        [k (:op v)])
                                (unparse-spec a))))
                       sort
                       vec))
                group-by-qualified #(group-by (comp boolean namespace key) %)
                {req true req-un false} (group-by-qualified (:clojure.core.typed.annotator.rep/HMap-req m))
                {opt true opt-un false} (group-by-qualified (:clojure.core.typed.annotator.rep/HMap-opt m))]
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
                                                     (= :vector
                                                        (:clojure.core.typed.annotator.rep/class-instance d))))))
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
                            (fn [{:keys [dom] :as ifn}]
                              {:pre [dom]}
                              ;(prn "doms" (count dom) (keyword (get fixed-name-lookup (count dom))))
                              (let [dom-knames 
                                    (let [[matching-fixed-names rest-arg-name]
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
                                  (concat
                                    (mapcat (fn [n k d]
                                              {:pre [(keyword? k)]}
                                              (let [spec 
                                                    (cond
                                                      (and (zero? n)
                                                           macro?
                                                           (#{:class} (:op d))
                                                           (= :vector
                                                              (:clojure.core.typed.annotator.rep/class-instance d)))
                                                      (keyword (str core-specs-ns) "bindings")

                                                      :else (unparse-spec d))]
                                                [k spec]))
                                            (range)
                                            dom-knames
                                            dom)
                                    (when-let [rest (:rest ifn)]
                                      [(or (when-let [[_ n] (seq rest-arglist)]
                                             (when (symbol? n)
                                               (keyword (name n))))
                                           :rest-arg)
                                       (spec-star (unparse-spec rest))])))))
                            arities))
                   rngs (if macro?
                          (qualify-core-symbol 'any?)
                          (or-spec (let [u (make-Union (map :rng arities))]
                                     (if (r/union? u)
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
                        :ret rngs]))))
    :class (let [cls (:clojure.core.typed.annotator.rep/class-instance m)
                 args (:args m)]
             (cond
               (#{:int} cls) (qualify-core-symbol 'int?)
               (#{:integer} cls) (qualify-core-symbol 'integer?)
               (#{:decimal} cls) (qualify-core-symbol 'decimal?)
               (#{:double} cls) (qualify-core-symbol 'double?)
               (#{:number} cls) (qualify-core-symbol 'number?)
               (#{:char} cls) (qualify-core-symbol 'char?)
               (#{:symbol} cls) (qualify-core-symbol 'symbol?)
               (#{:keyword} cls) (qualify-core-symbol 'keyword?)
               (#{:string} cls) (qualify-core-symbol 'string?)
               (#{:ifn} cls) (qualify-core-symbol 'ifn?)
               (#{:boolean} cls) (qualify-core-symbol 'boolean?)
               ;; TODO check set elements
               (#{:set} cls) (qualify-core-symbol 'set?)
               (#{:map} cls)
               ;; NOTE if we change the `empty?` specs here, also update
               ;; `or-spec` tag generation.
               (if (some r/nothing? args)
                 (list (qualify-spec-symbol 'and)
                       (qualify-core-symbol 'empty?)
                       (qualify-core-symbol 'map?))
                 (let [[k v] args]
                   (list (qualify-spec-symbol 'map-of)
                         (unparse-spec k)
                         (unparse-spec v))))
               (#{:vector :coll :seq} cls) 
               (if (r/nothing? (first args))
                 (list (qualify-spec-symbol 'and)
                       (qualify-core-symbol 'empty?)
                       (qualify-core-symbol 'coll?))
                 (list*-force
                   (qualify-spec-symbol 'coll-of)
                   (unparse-spec
                     (first args))
                   (when (#{:vector} cls)
                     [:into (qualify-core-symbol 'vector?)])))

               :else (do
                       (assert (string? cls))
                       (list (qualify-core-symbol 'partial)
                             (qualify-core-symbol 'instance?)
                             (symbol cls)))))
    :Top (qualify-core-symbol 'any?)
    :unknown (cond 
               *preserve-unknown* '?
               :else (qualify-core-symbol 'any?))
    :free (alias->spec-kw (:name m))
    (assert nil (str "No unparse-type case: " m))))

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

;; generate good alias name for `s/keys`
(defn register-unique-alias-for-spec-keys [env config k t]
  {:pre [(keyword? k)
         (r/type? t)]
   :post [(namespace (first %))]}
  (let [qualified? (boolean (namespace k))
        sym (if qualified?
              (kw->sym k)
              ;; not a truly unique namespace prefix, but let's see if it
              ;; works in practice.
              (symbol (unq-spec-nstr) (name k)))]
    ;(prn "register" sym)
    [sym (if true #_qualified?
           (update-alias-env env update sym #(if %1 (join %1 %2) %2) t)
           (register-alias env config sym t))]))

(defn or-spec [alts]
  {:pre [(every? r/type? alts)]}
  (let [;; put instance comparisons at the front of the disjunction.
        ;; avoid errors in checking specs that have both records
        ;; and collections, since records do not implement `empty`.
        {inst? true other false} (group-by (fn [t]
                                             (boolean
                                               (or (#{:val} (:op t))
                                                   (and (#{:class} (:op t))
                                                        (not
                                                          (#{:set
                                                             :map
                                                             :vector
                                                             :coll
                                                             :seq}
                                                            (:clojure.core.typed.annotator.rep/class-instance t)))))))
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
                                              (r/type? orig)]
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
                                                        (#{(qualify-core-symbol 'empty?)} c2)
                                                        (#{(qualify-core-symbol 'coll?)
                                                           (qualify-core-symbol 'map?)}
                                                                                c3)))
                                             (keyword (str "empty"
                                                           (case (symbol (name (nth s 2)))
                                                             coll? (or (let [[_ _ _ & args] s]
                                                                         (when (even? (count args))
                                                                           (let [opts (apply hash-map args)]
                                                                             (when (#{(qualify-core-symbol 'vector?)} (:into opts))
                                                                               "-vector"))))
                                                                       "-coll")
                                                             map? "-map"
                                                             nil))))
                                           ;; give up, `uniquify` will handle clashes
                                           :spec))
                                     specs)
                             names (uniquify names)
                             ;; FIXME sort by key, but preserve instance checks first
                             names+specs (map vector names (map first specs))]
                         (apply concat names+specs))))))))

; Here we need to handle the pecularities of s/keys.
;
; Some interesting scenarios:
;
; 1. Simple HMap that needs to be converted to spec aliases
;   Input: 
;     (t/defalias ABMap (t/HMap :optional {:a t/Int, :b t/Int}))
;   Output: 
;     (s/def ::ABMap (s/keys :opt-un [::a ::b]))
;     (s/def ::a int?)
;     (s/def ::b int?)
;
; 2. Recursive HMap that needs to be converted to spec aliases
;   Input:
;     (t/defalias AMap (U nil (t/HMap :mandatory {:a AMap})))
;   Output: 
;     (s/def ::AMap (s/or :nil nil? :map (s/keys :req-un [::a])))
;     (s/def ::a ::AMap)
;
; 3. Nested HMap
;   Input:
;     (t/defalias AMap (t/HMap :mandatory {:a (t/HMap :mandatory {:b AMap})}))
;   Output: 
;     (s/def ::AMap (s/keys :req-un [::a]))
;     (s/def ::a (s/keys :req-un [::b]))
;     (s/def ::b ::AMap)
;
; 4. Combine :req-un from different HMaps
;   Input:
;     (t/defalias AMap (t/HMap :mandatory {:a nil}))
;     (t/defalias ABMap (t/HMap :mandatory {:a t/Int, :b nil}))
;   Output: 
;     (s/def ::AMap (s/keys :req-un [::a]))
;     (s/def ::ABMap (s/keys :req-un [::a ::b]))
;     (s/def ::a (s/or :nil nil? :int int?))
;     (s/def ::b nil?)

; Plan:
; Add extra pass that recurs down type+alias envs and, for each HMap, add
; an appropriate alias to each entry.
;
; Continue doing this recursively until the type+alias environments do not change.
;
; Problem:
; For tagged maps we preserve key information, and it would be a problem
; if we moved or erased the tags.
; For these, add a :clojure.core.typed.annotator.frontend.spec/implicit-alias entry to the :val (or :union, sometimes) map
; that is the alias to use in spec generation.
; Then, need a special case in unparse-type to ensure :clojure.core.typed.annotator.frontend.spec/implicit-alias counts as
; as an alias usage.

(defn accumulate-env [f env config ts]
  (loop [env env
         ts ts
         out []]
    (if (empty? ts)
      [env out]
      (let [[env t] (f env config (first ts))]
        (recur env
               (next ts)
               (conj out t))))))

(defn good-alias-name-for-key? [sym k]
  {:pre [(symbol? sym)
         (keyword? k)]}
  (if (namespace k)
    (= k (keyword sym))
    (and (namespace sym)
         (= (name sym) (name k)))))

(defn ensure-alias-for-spec-keys [env config [k t]]
  {:pre [(keyword? k)]}
  (let [a (or (when (r/alias? t) t)
              (:clojure.core.typed.annotator.frontend.spec/implicit-alias t))]
    (if (and (r/alias? a)
             (good-alias-name-for-key? (:name a) k))
      [env t]
      (let [[sym env] (register-unique-alias-for-spec-keys env config k t)
            new-alias {:op :alias
                       :name sym}]
        ;; maintain structure to calculate multi-spec's
        [env (if (kw-vals? t)
               (assoc t :clojure.core.typed.annotator.frontend.spec/implicit-alias new-alias)
               new-alias)]))))

(defn implicit-aliases-for-type [env config m]
  (let [maybe-recur-entry (fn [env m kw]
                            (if-let [v (get m kw)]
                              (let [[env v] (implicit-aliases-for-type env config v)]
                                [env (assoc m kw v)])
                              [env m]))
        recur-vec-entry (fn [env m kw]
                          (let [[env ts] (accumulate-env implicit-aliases-for-type env config (get m kw))]
                            [env (assoc m kw ts)]))]
    (case (:op m)
      (:free :unknown :alias :val :Top) [env m]
      :union (recur-vec-entry env m :types)
      :HVec (recur-vec-entry env m :vec)
      :HMap (let [process-HMap-entries (fn [env es]
                                         {:pre [(map? es)]}
                                         (let [[env ts] (accumulate-env implicit-aliases-for-type env config (vals es))
                                               es (zipmap (keys es) ts)
                                               [env ts] (accumulate-env ensure-alias-for-spec-keys env config es)
                                               es (zipmap (keys es) ts)]
                                           [env es]))
                  [env req] (process-HMap-entries env (:clojure.core.typed.annotator.rep/HMap-req m))
                  [env opt] (process-HMap-entries env (:clojure.core.typed.annotator.rep/HMap-opt m))]
              [env (assoc m
                          :clojure.core.typed.annotator.rep/HMap-req req
                          :clojure.core.typed.annotator.rep/HMap-opt opt)])
      (:class :unresolved-class) (recur-vec-entry env m :args)
      :IFn (recur-vec-entry env m :arities)
      :IFn1 (let [[env m] (recur-vec-entry env m :dom)
                  [env m] (maybe-recur-entry env m :rng)
                  [env m] (maybe-recur-entry env m :rest)]
              [env m])
      #_:poly  ;TODO
      (assert nil (str "No implicit-aliases-for-type case: " m)))))

(defn implicit-aliases-for-tenv [env config]
  (loop [tenv (type-env env)
         out {}
         env env]
    (if (empty? tenv)
      (update-type-env env (constantly out))
      (let [[k t] (first tenv)
            [env t] (implicit-aliases-for-type env config t)]
        (recur (next tenv)
               (assoc out k t)
               env)))))

; If aliases are recursive, we have to be careful we don't clobber any changes
; made to them. For example, when we traverse qual/a, we'll need to register
; `nil` as part of qual/a's type. Simply adding implicit aliases to '{:qual/a nil}
; is not enough.
;
; Input:
; (defalias qual/a '{:qual/a nil})
;=>
; Output:
; (defalias qual/a (U nil '{:qual/a qual/a}))
;
; We can't just `join` the updated alias, since it will not have implicit aliases
; eg. above, we'll get `(defalias qual/a (U nil '{:qual/a nil}))`
; So, iterate on generating qual/a until it is stable.
;
; Input:
; (defalias qual/a '{:qual/a nil})
;=>
; (defalias qual/a (U nil '{:qual/a nil}))
;=>
; (defalias qual/a (U nil '{:qual/a nil}))
; Output:
; (defalias qual/a (U nil '{:qual/a qual/a}))
;

;; should only need max 2 iterations I think?
(def fixed-point-limit 3)

(defn implicit-aliases-for-aenv [env config]
  (loop [as (keys (alias-env env))
         env env]
    (if (empty? as)
      env
      (let [implicit-alias-fixed-point
            (fn [env k]
              (loop [old-alias (get (alias-env env) k)
                     env env
                     cnt 0]
                (assert old-alias)
                (assert (< cnt fixed-point-limit))
                (let [[env t] (implicit-aliases-for-type env config old-alias)
                      changed-alias (get (alias-env env) k)]
                  ;; if the current alias hasn't change from traversing it,
                  ;; we can be sure that we've correctly calculated the implicit aliases
                  ;; of this alias.
                  (if (= old-alias changed-alias)
                    (update-alias-env env assoc k t)
                    (recur changed-alias
                           env
                           (inc cnt))))))]
        (recur (next as)
               (implicit-alias-fixed-point env (first as)))))))

(defn implicit-aliases-for-env [env config]
  (let [env (implicit-aliases-for-tenv env config)
        env (implicit-aliases-for-aenv env config)]
    env))

(defn unparse-spec-aliases [env used-aliases]
  (loop [worklist (vec used-aliases)
         done-sdefs {}]
    (if (empty? worklist)
      done-sdefs
      (let [a (nth worklist 0)]
        (if (done-sdefs a)
          (recur (subvec worklist 1)
                 done-sdefs)
          (let [add-to-worklist (atom #{})
                e (binding [*used-aliases* add-to-worklist]
                    (unparse-spec (get (alias-env env) a)))]
            (recur (into (subvec worklist 1)
                         (remove done-sdefs)
                         @add-to-worklist)
                   (assoc done-sdefs a e))))))))


(defn envs-to-specs [env {:keys [spec-macros allow-top-level-non-IFn] :as config}]
  ;(prn "envs-to-specs" (keys (alias-env env)))
  (let [should-spec-macros? (boolean spec-macros)
        trim-type-env #?(:clj
                         #(into {}
                             (remove (fn [[k v]]
                                       (or ;; don't spec local functions
                                           (local-fn-symbol? k)
                                           ;; macro specs are opt-in
                                           (if should-spec-macros?
                                             false
                                             (macro-symbol? k))
                                           ;; don't spec external functions
                                           (imported-symbol? k)
                                           ;; only output fdef's. spec seems to assume all
                                           ;; top level def's are functions, which breaks spec instrumentation.
                                           ;; We work around this behaviour by simply
                                           ;; omitting non-function specs.
                                           (if allow-top-level-non-IFn
                                             false
                                             (not (#{:IFn} (:op v)))))))
                             %)
                         :cljs identity)
        ;_ (prn "pre env" env)
        env (update-type-env env trim-type-env)
        ;_ (prn "allow-top-level-non-IFn" allow-top-level-non-IFn)
        ;_ (prn "env" env)
        env (implicit-aliases-for-env env config)
        aliases-generated (atom #{})]
    (binding [*envs* (atom env)]
      (let [used-aliases (atom #{})
            multispecs-needed (atom {})
            unparse-spec' (fn [s]
                            (binding [*used-aliases* used-aliases
                                      *multispecs-needed* multispecs-needed]
                              (unparse-spec s)))
            top-level-types
            (into []
                  (mapcat (fn [[k v]]
                            (let [s (unparse-spec' (assoc v :top-level-def k))
                                  sym (if (= (namespace k)
                                             (str (ns-name (current-ns))))
                                        ;; defs
                                        (symbol (name k))
                                        ;; imports
                                        k)
                                  ;old-spec (get-spec-form k)
                                  spec-to-maybe-fdef 
                                  (fn [sym s]
                                    (if (and (seq? s)
                                             (#{(qualify-spec-symbol 'fspec)}
                                                                     (first s)))
                                      (list*-force (qualify-spec-symbol 'fdef)
                                                   sym
                                                   (next s))
                                      (def-spec sym s)))
                                  sdef (spec-to-maybe-fdef sym s)]
                              [sdef])))
                  (sort-by first (type-env env)))

            ;_ (prn "used-aliases" @used-aliases)
            ; depends on side effects from above call
            aenv (binding [*multispecs-needed* multispecs-needed]
                   (unparse-spec-aliases env (sort @used-aliases)))
            _ (every? (fn [[_ specs]]
                        (assert (#{1} (count specs))
                                (str "Clash in alias generation: " specs)))
                      (group-by alias->spec-kw (keys aenv)))
            unparsed-aliases (mapv (fn [[sym spc]] (def-spec (alias->spec-kw sym) spc))
                                   (sort-by first aenv))
            ; depends on side effects from both above calls
            multispecs (apply concat (map second (sort-by first @multispecs-needed)))
            ;; multispecs first, since aliases refer to them
            top-level-types (vec (concat multispecs
                                         unparsed-aliases
                                         top-level-types))]
        {:top-level top-level-types
         :requires (when-let [requires (:explicit-require-needed config)]
                     [requires])}))))
