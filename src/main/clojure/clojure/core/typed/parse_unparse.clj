(ns clojure.core.typed.parse-unparse
  (:require [clojure.core.typed]
            [clojure.core.typed
             [type-rep :as r]
             [type-ctors :as c]
             [object-rep :as orep]
             [path-rep :as pthrep]
             [utils :as u]
             [dvar-env :as dvar]
             [filter-rep :as f]
             [filter-ops :as fl]
             [constant-type :as const]
             [datatype-env :as dtenv]
             [protocol-env :as prenv]
             [name-env :as nmenv]
             [free-ops :as free-ops]
             [frees :as frees]]
            [clojure.set :as set]
            [clojure.math.combinatorics :as comb])
  (:import (clojure.core.typed.type_rep NotType Intersection Union FnIntersection Bounds
                                        Projection DottedPretype Function RClass App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly PolyDots
                                        Mu HeterogeneousVector HeterogeneousList HeterogeneousMap
                                        CountRange Name Value Top TopFunction B F Result AnyValue
                                        HeterogeneousSeq KwArgsSeq TCError)
           (clojure.core.typed.filter_rep TopFilter BotFilter TypeFilter NotTypeFilter AndFilter OrFilter
                                          ImpFilter)
           (clojure.core.typed.object_rep NoObject EmptyObject Path)
           (clojure.core.typed.path_rep KeyPE CountPE ClassPE)
           (clojure.lang ISeq Cons IPersistentList Symbol IPersistentVector PersistentHashMap)))

(def ^:dynamic *parse-type-in-ns* nil)

(defmulti parse-type class)
(defmulti parse-type-list first)

;return a vector of [name bnds]
(defn parse-free [f]
  {:post [(u/hvector-c? symbol? r/Bounds?)]}
  (if (symbol? f)
    [f r/no-bounds]
    (let [[n & opts] f
          {upp :<
           low :>
           kind :kind} (apply hash-map opts)]
      [n (r/->Bounds
           (when-not kind
             (if upp 
               (parse-type upp)
               (r/->Top)) )
           (when-not kind
             (if low
               (parse-type low)
               (r/Bottom)))
           (when kind
             (parse-type kind)))])))

(defn check-forbidden-rec [rec tbody]
  (when (or (= rec tbody) 
            (and (r/Intersection? tbody)
                 (contains? (set (:types tbody)) rec))
            (and (r/Union? tbody)
                 (contains? (set (:types tbody)) rec)))
    (throw (Exception. "Recursive type not allowed here"))))

(defn- Mu*-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.type-ctors) 'Mu*)]
    (assert (var? v) "Mu* unbound")
    v))

(defn parse-rec-type [[rec [free-symbol :as bnder] type]]
  (let [Mu* @(Mu*-var)
        _ (assert (= 1 (count bnder)) "Only one variable in allowed: Rec")
        f (r/make-F free-symbol)
        body (free-ops/with-frees [f]
               (parse-type type))
        
        _ (check-forbidden-rec f body)]
    (Mu* (:name f) body)))

(def ^:dynamic *parse-pretype* nil)

(defmethod parse-type-list 'DottedPretype
  [[_ psyn bsyn]]
  (assert *parse-pretype* "DottedPretype only allowed in Project")
  (let [df (dvar/*dotted-scope* bsyn)]
    (assert df bsyn)
    (r/->DottedPretype (free-ops/with-frees [df]
                         (parse-type psyn))
                       (:name (dvar/*dotted-scope* bsyn)))))

(defmethod parse-type-list 'Project
  [[_ fsyn ttsyn]]
  (let [fread (read-string (str fsyn))
        afn (eval fread)
        ts (binding [*parse-pretype* true]
             (mapv parse-type ttsyn))]
    (with-meta (r/->Projection afn ts)
               {:fsyn fread})))

(defmethod parse-type-list 'CountRange
  [[_ n u]]
  (r/make-CountRange n u))

(defmethod parse-type-list 'ExactCount
  [[_ n]]
  (r/make-ExactCountRange n))

(defn- RClass-of-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.type-ctors) 'RClass-of)]
    (assert (var? v) "RClass-of unbound")
    v))

(defmethod parse-type-list 'predicate
  [[_ t-syn]]
  (let [RClass-of @(RClass-of-var)
        on-type (parse-type t-syn)]
    (r/make-FnIntersection
      (r/make-Function [r/-any] (RClass-of 'boolean) nil nil
                       :filter (fl/-FS (fl/-filter on-type 0)
                                       (fl/-not-filter on-type 0))))))

(defmethod parse-type-list 'Rec
  [syn]
  (parse-rec-type syn))

;dispatch on last element of syntax in binder
(defmulti parse-all-type (fn [bnds type] (last bnds)))

;(All [a b ...] type)
(defmethod parse-all-type '...
  [bnds type]
  (let [frees-with-bnds (reduce (fn [fs fsyn]
                                  {:pre [(vector? fs)]
                                   :post [(every? (u/hvector-c? symbol? r/Bounds?) %)]}
                                  (conj fs
                                        (free-ops/with-bounded-frees (map (fn [[n bnd]] [(r/make-F n) bnd]) fs)
                                          (parse-free fsyn))))
                                [] (-> bnds butlast butlast))
        dvar (parse-free (-> bnds butlast last))]
    (-> 
      (c/PolyDots* (map first (concat frees-with-bnds [dvar]))
                   (map second (concat frees-with-bnds [dvar]))
                   (free-ops/with-bounded-frees (map (fn [[n bnd]] [(r/make-F n) bnd]) frees-with-bnds)
                     (dvar/with-dotted [(r/make-F (first dvar))]
                       (parse-type type))))
      (with-meta {:actual-frees (concat (map first frees-with-bnds) [(first dvar)])}))))

;(All [a b] type)
(defmethod parse-all-type :default
  [bnds type]
  (let [frees-with-bnds
        (reduce (fn [fs fsyn]
                  {:pre [(vector? fs)]
                   :post [(every? (u/hvector-c? symbol? r/Bounds?) %)]}
                  (conj fs
                        (free-ops/with-bounded-frees (map (fn [[n bnd]] [(r/make-F n) bnd]) fs)
                          (parse-free fsyn))))
                [] bnds)]
    (c/Poly* (map first frees-with-bnds)
           (map second frees-with-bnds)
           (free-ops/with-bounded-frees (map (fn [[n bnd]] [(r/make-F n) bnd]) frees-with-bnds)
             (parse-type type))
           (map first frees-with-bnds))))

(defmethod parse-type-list 'All
  [[All bnds syn & more]]
  (assert (not more) "Bad All syntax")
  (parse-all-type bnds syn))

(defn parse-union-type [[u & types]]
  (apply c/Un (doall (map parse-type types))))

(defmethod parse-type-list 'U
  [syn]
  (parse-union-type syn))

(defn parse-intersection-type [[i & types]]
  (apply c/In (doall (map parse-type types))))

(defmethod parse-type-list 'I
  [syn]
  (parse-intersection-type syn))

(defmethod parse-type-list 'Array
  [[_ syn & none]]
  (assert (empty? none) "Expected 1 argument to Array")
  (let [t (parse-type syn)
        jtype (if (r/RClass? t)
                (r/RClass->Class t)
                Object)]
    (r/->PrimitiveArray jtype t t)))

(defmethod parse-type-list 'ReadOnlyArray
  [[_ osyn & none]]
  (assert (empty? none) "Expected 1 argument to ReadOnlyArray")
  (r/->PrimitiveArray Object (r/Bottom) (parse-type osyn)))

(defmethod parse-type-list 'Array2
  [[_ isyn osyn & none]]
  (assert (empty? none) "Expected 2 arguments to Array2")
  (r/->PrimitiveArray Object (parse-type isyn) (parse-type osyn)))

(defmethod parse-type-list 'Array3
  [[_ jsyn isyn osyn & none]]
  (assert (empty? none) "Expected 3 arguments to Array3")
  (let [jrclass (parse-type jsyn)
        _ (assert (r/RClass? jrclass) "First argument to Array3 must be a Class")]
    (r/->PrimitiveArray (r/RClass->Class jrclass) (parse-type isyn) (parse-type osyn))))

(declare parse-function)

(defn parse-fn-intersection-type [[Fn & types]]
  (apply r/make-FnIntersection (mapv parse-function types)))

(defmethod parse-type-list 'Fn
  [syn]
  (parse-fn-intersection-type syn))

(defn parse-type-fn 
  [[_ binder bodysyn :as tfn]]
  (assert (= 3 (count tfn)))
  (assert (every? vector? binder))
  (let [free-maps (for [[nme & {:keys [variance < > kind] :as opts}] binder]
                    (do
                      (assert nme)
                      {:nme nme :variance (or variance :invariant)
                       :bound (r/map->Bounds 
                                {:upper-bound (when-not kind
                                                (if (contains? opts :<)
                                                  (parse-type <)
                                                  r/-any))
                                 :lower-bound (when-not kind
                                                (if (contains? opts :>) 
                                                  (parse-type >)
                                                  r/-nothing))
                                 :higher-kind (when kind
                                                (parse-type kind))})}))
        bodyt (free-ops/with-bounded-frees (map (fn [{:keys [nme bound]}] [(r/make-F nme) bound])
                                       free-maps)
                (parse-type bodysyn))
        vs (free-ops/with-bounded-frees (map (fn [{:keys [nme bound]}] [(r/make-F nme) bound])
                                             free-maps)
             (frees/fv-variances bodyt))
        _ (doseq [{:keys [nme variance]} free-maps]
            (when-let [actual-v (vs nme)]
              (assert (= (vs nme) variance)
                      (u/error-msg "Type variable " nme " appears in " (name actual-v) " position "
                                   "when declared " (name variance)))))]
    (with-meta (c/TypeFn* (map :nme free-maps) (map :variance free-maps)
                          (map :bound free-maps) bodyt)
               {:actual-frees (map :nme free-maps)})))

(defmethod parse-type-list 'TFn
  [syn]
  (parse-type-fn syn))

(defmethod parse-type-list 'Seq* [syn] (r/->HeterogeneousSeq (mapv parse-type (rest syn))))
(defmethod parse-type-list 'List* [syn] (r/->HeterogeneousList (mapv parse-type (rest syn))))
(defmethod parse-type-list 'Vector* [syn] (r/-hvec (mapv parse-type (rest syn))))

(defn- syn-to-hmap [mandatory optional]
  (letfn [(mapt [m]
            (into {} (for [[k v] m]
                       [(r/-val k)
                        (parse-type v)])))]
    (let [mandatory (mapt mandatory)
          optional (mapt optional)]
      (c/make-HMap mandatory optional))))

(defmethod parse-type-list 'quote 
  [[_ syn]]
  (cond
    ((some-fn number? keyword? symbol?) syn) (r/-val syn)
    (vector? syn) (r/-hvec (mapv parse-type syn))
    (map? syn) (syn-to-hmap syn nil)
    :else (throw (Exception. (str "Invalid use of quote:" syn)))))

(declare parse-in-ns)

(defmethod parse-type-list 'HMap
  [[_HMap_ & flat-opts]]
  (let [deprecated-mandatory (when (map? (first flat-opts))
                               (println 
                                 (ns-name (parse-in-ns))
                                 ": DEPRECATED: HMap syntax changed. Use :mandatory keyword argument instead of initial map")
                               (flush)
                               (first flat-opts))
        ^ISeq flat-opts (if deprecated-mandatory
                          (next flat-opts)
                          flat-opts)
        {:keys [optional mandatory]} (PersistentHashMap/createWithCheck flat-opts)
        _ (when (and deprecated-mandatory mandatory)
            (throw (Exception. "Cannot provide both deprecated initial map syntax and :mandatory option to HMap")))
        mandatory (or deprecated-mandatory mandatory)]
    (syn-to-hmap mandatory optional)))

(defn- parse-in-ns []
  (or *parse-type-in-ns* *ns*))

(defn- resolve-type [sym]
  (ns-resolve (parse-in-ns) sym))

(defn parse-RClass [cls-sym params-syn]
  (let [RClass-of @(RClass-of-var)
        cls (resolve-type cls-sym)
        _ (assert (class? cls) (str cls-sym " cannot be resolved"))
        tparams (doall (map parse-type params-syn))]
    (RClass-of cls tparams)))

(defmethod parse-type-list 'Value
  [[_Value_ syn]]
  (const/constant-type syn))

(defmethod parse-type-list 'KeywordArgs
  [[_KeywordArgs_ & {:keys [optional mandatory]}]]
  (assert (= #{}
             (set/intersection (set (keys optional))
                               (set (keys mandatory)))))
  (let [optional (into {} (for [[k v] optional]
                            (do (assert (keyword? k))
                              [(r/-val k) (parse-type v)])))
        mandatory (into {} (for [[k v] mandatory]
                             (do (assert (keyword? k))
                               [(r/-val k) (parse-type v)])))]
    (apply c/Un (apply concat
                     (for [opts (map #(into {} %) (comb/subsets optional))]
                       (let [m (merge mandatory opts)
                             kss (comb/permutations (keys m))]
                         (for [ks kss]
                           (r/->HeterogeneousSeq (mapcat #(find m %) ks)))))))))

(declare unparse-type)

(defmethod parse-type-list :default 
  [[n & args :as syn]]
  (let [RClass-of @(RClass-of-var)
        current-nstr (-> (parse-in-ns) ns-name name)
        res (resolve-type n)
        rsym (cond 
               (class? res) (u/Class->symbol res)
               (var? res) (u/var->symbol res))]
    (if (free-ops/free-in-scope n)
      (let [^TypeFn k (.higher-kind (free-ops/free-in-scope-bnds n))
            _ (assert (r/TypeFn? k) (u/error-msg "Cannot invoke type variable " n))
            _ (assert (= (.nbound k) (count args)) (u/error-msg "Wrong number of arguments (" (count args)
                                                                ") to type function " (unparse-type k)))]
        (r/->TApp (free-ops/free-in-scope n) (mapv parse-type args)))
      (if-let [t ((some-fn dtenv/get-datatype prenv/get-protocol nmenv/get-type-name) rsym)]
        ;don't resolve if operator is declared
        (if (keyword? t)
          (cond
            ; declared names can be TFns
            (isa? t nmenv/declared-name-type) (r/->TApp (r/->Name rsym) (mapv parse-type args))
            ; for now use Apps for declared Classes and protocols
            :else (r/->App (r/->Name rsym) (mapv parse-type args)))
          (r/->TApp (r/->Name rsym) (mapv parse-type args)))
        (cond
          ;a Class that's not a DataType
          (class? res) (c/RClass-of res (mapv parse-type args))
          :else
          ;unqualified declared protocols and datatypes
          (if-let [s (let [svar (symbol current-nstr (name n))
                           scls (symbol (munge (str current-nstr \. (name n))))]
                       (some #(and (nmenv/get-type-name %)
                                   %)
                             [svar scls]))]
            (r/->App (r/->Name s) (mapv parse-type args))
            (u/tc-error (str "Cannot parse type: " (pr-str syn)
                             (when (seq syn)
                               (str "\nHint: Does " (first syn) " accept parameters and is it in scope?"))))))))))

(defmethod parse-type Cons [l] (parse-type-list l))
(defmethod parse-type IPersistentList [l] (parse-type-list l))

(defmulti parse-type-symbol identity)
(defmethod parse-type-symbol 'Any [_] (r/->Top))
(defmethod parse-type-symbol 'Nothing [_] (r/Bottom))
(defmethod parse-type-symbol 'AnyFunction [_] (r/->TopFunction))

(defn primitives-fn []
  (let [RClass-of @(RClass-of-var)]
    {'byte (RClass-of 'byte)
     'short (RClass-of 'short)
     'int (RClass-of 'int)
     'long (RClass-of 'long)
     'float (RClass-of 'float)
     'double (RClass-of 'double)
     'boolean (RClass-of 'boolean)
     'char (RClass-of 'char)
     'void r/-nil}))

(defmethod parse-type-symbol :default
  [sym]
  (let [primitives (primitives-fn)]
    (letfn [(resolve-symbol [qsym clssym]
              (cond
                (primitives sym) (primitives sym)
                (nmenv/get-type-name qsym) (r/->Name qsym)
                (nmenv/get-type-name clssym) (r/->Name clssym)
                ;Datatypes that are annotated in this namespace, but not yet defined
                (dtenv/get-datatype clssym) (dtenv/resolve-datatype clssym)
                (prenv/get-protocol qsym) (prenv/resolve-protocol qsym)))]
      (if-let [f (free-ops/free-in-scope sym)]
        f
        (let [ RClass-of @(RClass-of-var)
              current-nstr (-> (parse-in-ns) ns-name name)
              qsym (if (namespace sym)
                     sym
                     (symbol current-nstr (name sym)))
              clssym (if (some #(= \. %) (str sym))
                       sym
                       (symbol (str (munge current-nstr) \. (name sym))))]
          (or (resolve-symbol qsym clssym)
              (let [res (resolve-type sym)
                    qsym (when (var? res)
                           (u/var->symbol res))
                    clssym (when (class? res)
                             (u/Class->symbol res))]
                (or (resolve-symbol qsym clssym)
                    (when qsym
                      (prn "WARNING: Assuming unannotated var " qsym
                           " is a protocol.")
                      (r/->Name qsym))
                    (when clssym
                      (c/RClass-of clssym))))
              (u/tc-error (str "Cannot resolve type: " (pr-str sym)
                               "\nHint: Is " (pr-str sym) " in scope?"
                               "\nHint: Has " (pr-str sym) "'s annotation been"
                               " found via check-ns, cf or typed-deps?"))))))))

(defmethod parse-type Symbol [l] (parse-type-symbol l))
(defmethod parse-type Boolean [v] (if v r/-true r/-false)) 
(defmethod parse-type nil [_] r/-nil)

(declare parse-path-elem parse-filter)

(defn parse-object [{:keys [id path]}]
  (orep/->Path (when path (mapv parse-path-elem path)) id))

(defn parse-filter-set [{:keys [then else] :as fsyn}]
  (fl/-FS (if then
            (parse-filter then)
            f/-top)
          (if else
            (parse-filter else)
            f/-top)))

(defmulti parse-filter first)

(defmethod parse-filter 'is
  [[_ & [tsyn nme psyns :as all]]]
  (assert (#{2 3} (count all)))
  (let [t (parse-type tsyn)
        p (when (= 3 (count all))
            (mapv parse-path-elem psyns))]
    (fl/-filter t nme p)))

(defmethod parse-filter '!
  [[_ & [tsyn nme psyns :as all]]]
  (assert (#{2 3} (count all)))
  (let [t (parse-type tsyn)
        p (when (= 3 (count all))
            (mapv parse-path-elem psyns))]
    (fl/-not-filter t nme p)))

(defmethod parse-filter '|
  [[_ & fsyns]]
  (apply fl/-or (mapv parse-filter fsyns)))

(defmethod parse-filter '&
  [[_ & fsyns]]
  (apply fl/-and (mapv parse-filter fsyns)))

(defmulti parse-path-elem #(cond
                             (symbol? %) %
                             :else (first %)))

(defmethod parse-path-elem 'Class [_] (pthrep/->ClassPE))

(defmethod parse-path-elem 'Key
  [[_ & [ksyn :as all]]]
  (assert (= 1 (count all)))
  (pthrep/->KeyPE ksyn))

(defn- parse-kw-map [m]
  {:post [((u/hash-c? r/Value? r/Type?) %)]}
  (into {} (for [[k v] m]
             [(r/-val k) (parse-type v)])))

(defn parse-function [f]
  {:post [(r/Function? %)]}
  (let [all-dom (take-while #(not= '-> %) f)
        [_ rng & opts-flat :as chk] (drop-while #(not= '-> %) f) ;opts aren't used yet
        _ (assert (<= 2 (count chk)) (str "Missing range in " f))

        opts (apply hash-map opts-flat)

        {ellipsis-pos '...
         asterix-pos '*
         ampersand-pos '&}
        (zipmap all-dom (range))

        _ (assert (#{0 1} (count (filter identity [asterix-pos ellipsis-pos ampersand-pos])))
                  "Can only provide one rest argument option: &, ... or *")

        _ (when-let [ks (seq (remove #{:filters :object} (keys opts)))]
            (throw (Exception. (str "Invalid option/s: " ks))))

        filters (when-let [[_ fsyn] (find opts :filters)]
                  (parse-filter-set fsyn))

        object (when-let [[_ obj] (find opts :object)]
                 (parse-object obj))

        fixed-dom (cond 
                    asterix-pos (take (dec asterix-pos) all-dom)
                    ellipsis-pos (take (dec ellipsis-pos) all-dom)
                    ampersand-pos (take ampersand-pos all-dom)
                    :else all-dom)

        rest-type (when asterix-pos
                    (nth all-dom (dec asterix-pos)))
        [drest-type _ drest-bnd] (when ellipsis-pos
                                   (drop (dec ellipsis-pos) all-dom))
        [optional-kws & {mandatory-kws :mandatory}] (when ampersand-pos
                                                      (drop (inc ampersand-pos) all-dom))]
    (r/make-Function (mapv parse-type fixed-dom)
                     (parse-type rng)
                     (when asterix-pos
                       (parse-type rest-type))
                     (when ellipsis-pos
                       (r/->DottedPretype
                         (free-ops/with-frees [(dvar/*dotted-scope* drest-bnd)] ;with dotted bound in scope as free
                           (parse-type drest-type))
                         (:name (dvar/*dotted-scope* drest-bnd))))
                     :filter filters
                     :object object
                     :optional-kws (when optional-kws
                                     (parse-kw-map optional-kws))
                     :mandatory-kws (when mandatory-kws
                                      (parse-kw-map mandatory-kws)))))

(defmethod parse-type IPersistentVector
  [f]
  (apply r/make-FnIntersection [(parse-function f)]))

(defmethod parse-type :default
  [k]
  (u/tc-error (str "Bad type syntax: " (pr-str k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unparse

(def ^:dynamic *next-nme* 0) ;stupid readable variables

(def ^:dynamic *unparse-type-in-ns* nil)
(set-validator! #'*unparse-type-in-ns* (some-fn nil? #(instance? clojure.lang.Namespace %)))

(defn alias-in-ns
  "Returns an alias for namespace sym in ns, or nil if none."
  [nsym ns]
  (some (fn [[alias ans]]
          (when (= (str nsym) (str (ns-name ans)))
            alias))
        (ns-aliases ns)))

(defn core-lang-Class-sym [clsym]
  (when (.startsWith (str clsym) "clojure.lang.")
    (symbol (.getSimpleName (Class/forName (str clsym))))))

(defn Class-symbol-intern [clsym ns]
  (some (fn [[isym cls]]
          (when (= (str clsym) (str (u/Class->symbol cls)))
            isym))
        (ns-imports ns)))

(defn var-symbol-intern 
  "Returns a symbol interned in ns for var symbol, or nil if none.

  (var-symbol-intern 'symbol (find-ns 'clojure.core))
  ;=> 'symbol
  (var-symbol-intern 'bar (find-ns 'clojure.core))
  ;=> nil"
  [sym ns]
  (some (fn [[isym var]]
          (when (= (str sym) (str (u/var->symbol var)))
            isym))
        (merge (ns-interns ns)
               (ns-refers ns))))

(defn unparse-Class-symbol-in-ns [sym]
  (if-let [ns (and (not clojure.core.typed/*verbose-types*)
                   *unparse-type-in-ns*)]
        ; use an import name
    (or (Class-symbol-intern sym ns)
        ; core.lang classes are special
        (core-lang-Class-sym sym)
        ; otherwise use fully qualified name
        sym)
    sym))

(defn unparse-var-symbol-in-ns [sym]
  {:pre [(namespace sym)]}
  (if-let [ns (and (not clojure.core.typed/*verbose-types*)
                   *unparse-type-in-ns*)]
        ; use unqualified name if interned
    (or (var-symbol-intern sym ns)
        ; use aliased ns if not interned, but ns is aliased
        (when-let [alias (alias-in-ns (namespace sym) ns)]
          (symbol (str alias) (name sym)))
        ; otherwise use fully qualified name
        sym)
    sym))

(declare unparse-type* unparse-object unparse-filter-set unparse-filter)

(defn unparse-type [t]
  (if-let [nsym (-> t meta :source-Name)]
    nsym
    (unparse-type* t)))

(defmulti unparse-type* class)
(defn unp [t] (prn (unparse-type t)))

(defmethod unparse-type* Top [_] 'Any)
(defmethod unparse-type* TCError [_] 'Error)
(defmethod unparse-type* Name [{:keys [id]}] (unparse-var-symbol-in-ns id))
(defmethod unparse-type* AnyValue [_] 'AnyValue)

(defmethod unparse-type* Projection 
  [{:keys [ts] :as t}]
  (let [{:keys [fsyn]} (meta t)]
    (list 'Project fsyn (mapv unparse-type ts))))

(defmethod unparse-type* DottedPretype
  [{:keys [pre-type name]}]
  (list 'DottedPretype (unparse-type pre-type) name))

(defmethod unparse-type* CountRange [{:keys [lower upper]}]
  (cond
    (= lower upper) (list 'ExactCount lower)
    :else (list* 'CountRange lower (when upper [upper]))))

(defmethod unparse-type* App 
  [{:keys [rator rands]}]
  (list* (unparse-type rator) (mapv unparse-type rands)))

(defmethod unparse-type* TApp 
  [{:keys [rator rands] :as tapp}]
  (cond 
    ;perform substitution if obvious
    ;(TypeFn? rator) (unparse-type (resolve-tapp tapp))
    :else
    (list* (unparse-type rator) (mapv unparse-type rands))))

(defmethod unparse-type* Result
  [{:keys [t]}]
  (unparse-type t))

(defmethod unparse-type* F
  [{:keys [name]}]
  (or (some (fn [[sym {{fname :name} :F}]]
              (when (= name fname)
                sym))
            free-ops/*free-scope*)
      name))

(defmethod unparse-type* PrimitiveArray
  [{:keys [jtype input-type output-type]}]
  (cond 
    (and (= input-type output-type)
         (= Object jtype))
    (list 'Array (unparse-type input-type))

    (= Object jtype)
    (list 'Array2 (unparse-type input-type) (unparse-type output-type))

    :else
    (list 'Array3 (u/Class->symbol jtype)
          (unparse-type input-type) (unparse-type output-type))))

(defmethod unparse-type* B
  [{:keys [idx]}]
  (list 'B idx))

(defmethod unparse-type* Union
  [{types :types :as u}]
  (cond
    ; Prefer the user provided Name for this type. Needs more thinking?
    ;(-> u meta :from-name) (-> u meta :from-name)
    (seq types) (list* 'U (doall (map unparse-type types)))
    :else 'Nothing))

(defmethod unparse-type* FnIntersection
  [{types :types}]
  (list* 'Fn (doall (map unparse-type types))))

(defmethod unparse-type* Intersection
  [{types :types}]
  (list* 'I (doall (map unparse-type types))))

(defmethod unparse-type* TopFunction [_] 'AnyFunction)

(defn- unparse-kw-map [m]
  {:pre [((u/hash-c? r/Value? r/Type?) m)]}
  (into {} (for [[^Value k v] m] 
             [(.val k) (unparse-type v)])))

(defmethod unparse-type* Function
  [{:keys [dom rng kws rest drest]}]
  (vec (concat (doall (map unparse-type dom))
               (when rest
                 [(unparse-type rest) '*])
               (when drest
                 (let [{:keys [pre-type name]} drest]
                   [(unparse-type pre-type) '... name]))
               (when kws
                 (let [{:keys [optional mandatory]} kws]
                   (list* '& 
                          (unparse-kw-map optional)
                          (when (seq mandatory) 
                            [:mandatory (unparse-kw-map mandatory)]))))
               (let [{:keys [t fl o]} rng]
                 (concat ['-> (unparse-type t)]
                         (when (not (and ((some-fn f/TopFilter? f/BotFilter?) (:then fl))
                                         ((some-fn f/TopFilter? f/BotFilter?) (:else fl))))
                           [(unparse-filter-set fl)])
                         (when (not ((some-fn orep/NoObject? orep/EmptyObject?) o))
                           [(unparse-object o)]))))))

(defmethod unparse-type* Protocol
  [{:keys [the-var poly?]}]
  (if poly?
    (list* (unparse-var-symbol-in-ns the-var) (mapv unparse-type poly?))
    the-var))

(defmethod unparse-type* DataType
  [{:keys [the-class poly?]}]
  (if poly?
    (list* (unparse-Class-symbol-in-ns the-class) (mapv unparse-type poly?))
    the-class))

(defmulti unparse-RClass :the-class)

(defmethod unparse-RClass 'clojure.lang.Atom
  [{:keys [the-class poly?]}]
  (let [[w r] poly?]
    (list* (unparse-Class-symbol-in-ns the-class) (map unparse-type (concat [w]
                                               (when (not= w r)
                                                 [r]))))))

(defmethod unparse-RClass :default
  [{:keys [the-class poly?]}]
  (list* (unparse-Class-symbol-in-ns the-class) (doall (map unparse-type poly?))))

(defmethod unparse-type* RClass
  [{:keys [the-class poly?] :as r}]
  (if (empty? poly?)
    (unparse-Class-symbol-in-ns the-class)
    (unparse-RClass r)))

(defmethod unparse-type* Mu
  [m]
  (let [nme (gensym "Mu")
        body (c/Mu-body* nme m)]
    (list 'Rec [nme] (unparse-type body))))

(defmethod unparse-type* PolyDots
  [{:keys [nbound] :as p}]
  (let [{:keys [actual-frees dvar-name]} (meta p)
        free-names actual-frees
        given-names? (and free-names dvar-name)
        end-nme (if given-names?
                  *next-nme*
                  (+ nbound *next-nme*))
        fs (if given-names?
             (vec (concat free-names [dvar-name]))
             (vec 
               (for [x (range *next-nme* end-nme)]
                 (symbol (str "v" x)))))
        body (c/PolyDots-body* fs p)]
    (binding [*next-nme* end-nme]
      (list 'All (vec (concat (butlast fs) [(last fs) '...])) (unparse-type body)))))

(defmethod unparse-type* Poly
  [{:keys [nbound] :as p}]
  (let [free-names (c/Poly-free-names* p)
        given-names? free-names
        end-nme (if given-names?
                  *next-nme*
                  (+ nbound *next-nme*))
        fs-names (or (and given-names? free-names)
                     (vec
                       (for [x (range *next-nme* end-nme)]
                         (symbol (str "v" x)))))
        bbnds (c/Poly-bbnds* fs-names p)
        fs (if given-names?
             (vec
               (for [[name {:keys [upper-bound lower-bound higher-kind]}] (map vector free-names bbnds)]
                 (let [u (when upper-bound 
                           (unparse-type upper-bound))
                       l (when lower-bound 
                           (unparse-type lower-bound))
                       h (when higher-kind
                           (unparse-type higher-kind))]
                   (or (when higher-kind
                         [name :kind h])
                       (when-not (or (r/Top? upper-bound) (r/Bottom? lower-bound))
                         [name :< u :> l])
                       (when-not (r/Top? upper-bound) 
                         [name :< u])
                       (when-not (r/Bottom? lower-bound)
                         [name :> l])
                       name))))
             fs-names)
        body (c/Poly-body* fs-names p)]
    (binding [*next-nme* end-nme]
      (list 'All fs (unparse-type body)))))

(defmethod unparse-type* TypeFn
  [{:keys [nbound] :as p}]
  (let [free-names (-> p meta :actual-frees)
        given-names? free-names
        end-nme (if given-names?
                  *next-nme*
                  (+ nbound *next-nme*))
        fs-names (or (and given-names? free-names)
                     (vec
                       (for [x (range *next-nme* end-nme)]
                         (symbol (str "v" x)))))
        bbnds (c/TypeFn-bbnds* fs-names p)
        fs (if given-names?
             (vec
               (for [[name {:keys [upper-bound lower-bound higher-kind]}] (map vector 
                                                                               (-> p meta :actual-frees)
                                                                               bbnds)]
                 (let [u (when upper-bound 
                           (unparse-type upper-bound))
                       l (when lower-bound 
                           (unparse-type lower-bound))
                       h (when higher-kind
                           (unparse-type higher-kind))]
                   (or (when higher-kind
                         [name :kind h])
                       (when-not (or (r/Top? upper-bound) (r/Bottom? lower-bound))
                         [name :< u :> l])
                       (when-not (r/Top? upper-bound) 
                         [name :< u])
                       (when-not (r/Bottom? lower-bound)
                         [name :> l])
                       name))))
             fs-names)
        body (c/TypeFn-body* fs-names p)]
    (binding [*next-nme* end-nme]
      (list 'TFn fs (unparse-type body)))))

(defmethod unparse-type* Value
  [v]
  (if ((some-fn r/Nil? r/True? r/False?) v)
    (:val v)
    (list 'Value (:val v))))

(defn- unparse-map-of-types [m]
  (into {} (map (fn [[k v]]
                  (assert (r/Value? k) k)
                  (vector (:val k) (unparse-type v)))
                m)))

(defmethod unparse-type* HeterogeneousMap
  [^HeterogeneousMap v]
  (list* (if (c/complete-hmap? v)
           'CompleteHMap 
           'PartialHMap)
         (unparse-map-of-types (:types v))
         (when-let [ks (and (not (c/complete-hmap? v))
                            (seq (.absent-keys v)))]
           [:absent-keys (set (map unparse-type ks))])))

(defmethod unparse-type* HeterogeneousSeq
  [v]
  (list* 'Seq* (doall (map unparse-type (:types v)))))

(defmethod unparse-type* KwArgsSeq
  [^KwArgsSeq v]
  (list* 'KwArgsSeq 
         (concat
           (when (seq (.optional v))
             [:optional (unparse-map-of-types (.optional v))])
           (when (seq (.mandatory v))
             [:mandatory (unparse-map-of-types (.mandatory v))]))))

(defmethod unparse-type* HeterogeneousVector
  [v]
  (mapv unparse-type (:types v)))

(defmethod unparse-type* HeterogeneousList
  [v]
  (list* 'List* (doall (map unparse-type (:types v)))))

; Objects

(declare unparse-path-elem)

(defmulti unparse-object class)
(defmethod unparse-object EmptyObject [_] 'empty-object)
(defmethod unparse-object NoObject [_] 'no-object)
(defmethod unparse-object Path [{:keys [path id]}] (conj {:id id} (when (seq path) [:path (mapv unparse-path-elem path)])))

; Path elems

(defmulti unparse-path-elem class)
(defmethod unparse-path-elem KeyPE [t] (list 'Key (:val t)))
(defmethod unparse-path-elem CountPE [t] 'Count)
(defmethod unparse-path-elem ClassPE [t] 'Class)

; Filters

(defmulti unparse-filter* class)

(declare unparse-filter)

(defn unparse-filter-set [{:keys [then else] :as fs}]
  {:pre [(f/FilterSet? fs)]}
  {:then (unparse-filter then)
   :else (unparse-filter else)})

(defn unparse-filter [f]
  (unparse-filter* f))

(defmethod unparse-filter* TopFilter [f] 'tt)
(defmethod unparse-filter* BotFilter [f] 'ff)

(declare unparse-type)

(defmethod unparse-filter* TypeFilter
  [{:keys [type path id]}]
  (concat (list 'is (unparse-type type) id)
          (when (seq path)
            [(map unparse-path-elem path)])))

(defmethod unparse-filter* NotTypeFilter
  [{:keys [type path id]}]
  (concat (list '! (unparse-type type) id)
          (when (seq path)
            [(map unparse-path-elem path)])))

(defmethod unparse-filter* AndFilter [{:keys [fs]}] (apply list '& (map unparse-filter fs)))
(defmethod unparse-filter* OrFilter [{:keys [fs]}] (apply list '| (map unparse-filter fs)))

(defmethod unparse-filter* ImpFilter
  [{:keys [a c]}]
  (list 'when (unparse-filter a) (unparse-filter c)))

;[TCResult -> Any]
(defn unparse-TCResult [r]
  (let [t (unparse-type (r/ret-t r))
        fs (unparse-filter-set (r/ret-f r))
        o (unparse-object (r/ret-o r))]
    (if (and (= (fl/-FS f/-top f/-top) (r/ret-f r))
             (= (r/ret-o r) orep/-empty))
      t
      (if (= (r/ret-o r) orep/-empty)
        [t fs]
        [t fs o]))))

(defn unparse-TCResult-in-ns [r ns]
  (binding [*unparse-type-in-ns* ns]
    (unparse-TCResult r)))
