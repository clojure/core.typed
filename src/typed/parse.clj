(in-ns 'typed.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type syntax

;(Map Symbol F)
(def ^:dynamic *free-scope* {})
(set-validator! #'*free-scope* #((hash-c? symbol? (hmap-c? :F F? :bnds Bounds?)) %))

(defn free-with-name 
  "Find the free with the actual name name, as opposed to
  the alias used for scoping"
  [name]
  {:pre [(symbol? name)]
   :post [((some-fn nil? F?) %)]}
  (some (fn [[_ {{fname :name :as f} :F}]]
          (when (= name fname)
            f))
        *free-scope*))

(defn free-with-name-bnds 
  "Find the bounds for the free with the actual name name, as opposed to
  the alias used for scoping"
  [name]
  {:pre [(symbol? name)]
   :post [((some-fn nil? Bounds?) %)]}
  (some (fn [[_ {{fname :name} :F :keys [bnds]}]]
          (when (= name fname)
            bnds))
        *free-scope*))

(defn free-in-scope 
  "Find the free scoped as name"
  [name]
  {:pre [(symbol? name)]
   :post [((some-fn nil? F?) %)]}
  (:F (*free-scope* name)))

(defn free-in-scope-bnds 
  "Find the bounds for the free scoped as name"
  [name]
  {:pre [(symbol? name)]
   :post [((some-fn nil? Bounds?) %)]}
  (:bnds (*free-scope* name)))

(defmacro with-free-mappings [frees-map & body]
  `(binding [*free-scope* (merge *free-scope* ~frees-map)]
     ~@body))

(defmacro with-bounded-frees [bfrees & body]
  `(with-free-mappings (into {} (for [[f# bnds#] ~bfrees]
                                  [(:name f#) {:F f# :bnds bnds#}]))
     ~@body))

(defmacro with-frees [frees & body]
  `(with-free-mappings (into {} (for [f# ~frees]
                                  [(:name f#) {:F f# :bnds no-bounds}]))
     ~@body))

(defmulti parse-type class)
(defmulti parse-type-list first)

;return a vector of [name bnds]
(defn parse-free [f]
  {:post [(hvector-c? symbol? Bounds?)]}
  (if (symbol? f)
    [f no-bounds]
    (let [[n & opts] f
          {upp :<
           low :>
           kind :kind} (apply hash-map opts)]
      [n (->Bounds
           (when-not kind
             (if upp 
               (parse-type upp)
               (->Top)) )
           (when-not kind
             (if low
               (parse-type low)
               (Bottom)))
           (when kind
             (parse-type kind)))])))

(defn check-forbidden-rec [rec tbody]
  (when (or (= rec tbody) 
            (and (Intersection? tbody)
                 (contains? (set (:types tbody)) rec))
            (and (Union? tbody)
                 (contains? (set (:types tbody)) rec)))
    (throw (Exception. "Recursive type not allowed here"))))

(defn parse-rec-type [[rec [free-symbol :as bnder] type]]
  (let [_ (assert (= 1 (count bnder)) "Only one variable in allowed: Rec")
        f (make-F free-symbol)
        body (with-frees [f]
               (parse-type type))
        
        _ (check-forbidden-rec f body)]
    (Mu* (:name f) body)))

(def ^:dynamic *parse-pretype* nil)

(defmethod parse-type-list 'DottedPretype
  [[_ psyn bsyn]]
  (assert *parse-pretype* "DottedPretype only allowed in Project")
  (let [df (*dotted-scope* bsyn)]
    (assert df bsyn)
    (->DottedPretype (with-frees [df]
                       (parse-type psyn))
                     (:name (*dotted-scope* bsyn)))))

(defmethod parse-type-list 'Project
  [[_ fsyn ttsyn]]
  (let [fread (read-string (str fsyn))
        afn (eval fread)
        ts (binding [*parse-pretype* true]
             (mapv parse-type ttsyn))]
    (with-meta (->Projection afn ts)
               {:fsyn fread})))

(defmethod parse-type-list 'CountRange
  [[_ n u]]
  (make-CountRange n u))

(defmethod parse-type-list 'ExactCount
  [[_ n]]
  (make-ExactCountRange n))

(defmethod parse-type-list 'predicate
  [[_ t-syn]]
  (let [on-type (parse-type t-syn)]
    (make-FnIntersection
      (make-Function [-any] (RClass-of 'boolean nil) nil nil
                     :filter (-FS (-filter on-type 0)
                                  (-not-filter on-type 0))))))

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
                                   :post [(every? (hvector-c? symbol? Bounds?) %)]}
                                  (conj fs
                                        (with-bounded-frees (map (fn [[n bnd]] [(make-F n) bnd]) fs)
                                          (parse-free fsyn))))
                                [] (-> bnds butlast butlast))
        dvar (parse-free (-> bnds butlast last))]
    (-> 
      (PolyDots* (map first (concat frees-with-bnds [dvar]))
                 (map second (concat frees-with-bnds [dvar]))
                 (with-bounded-frees (map (fn [[n bnd]] [(make-F n) bnd]) frees-with-bnds)
                   (with-dotted [(make-F (first dvar))]
                     (parse-type type))))
      (with-meta {:actual-frees (concat (map first frees-with-bnds) [(first dvar)])}))))

;(All [a b] type)
(defmethod parse-all-type :default
  [bnds type]
  (let [frees-with-bnds
        (reduce (fn [fs fsyn]
                  {:pre [(vector? fs)]
                   :post [(every? (hvector-c? symbol? Bounds?) %)]}
                  (conj fs
                        (with-bounded-frees (map (fn [[n bnd]] [(make-F n) bnd]) fs)
                          (parse-free fsyn))))
                [] bnds)]
    (Poly* (map first frees-with-bnds)
           (map second frees-with-bnds)
           (with-bounded-frees (map (fn [[n bnd]] [(make-F n) bnd]) frees-with-bnds)
             (parse-type type))
           (map first frees-with-bnds))))

(defmethod parse-type-list 'All
  [[All bnds syn & more]]
  (assert (not more) "Bad All syntax")
  (parse-all-type bnds syn))

(defn parse-union-type [[u & types]]
  (apply Un (doall (map parse-type types))))

(defmethod parse-type-list 'U
  [syn]
  (parse-union-type syn))

(defn parse-intersection-type [[i & types]]
  (apply In (doall (map parse-type types))))

(defmethod parse-type-list 'I
  [syn]
  (parse-intersection-type syn))

(defmethod parse-type-list 'Array
  [[_ syn & none]]
  (assert (empty? none) "Expected 1 argument to Array")
  (let [t (parse-type syn)]
    (->PrimitiveArray Object t t)))

(defmethod parse-type-list 'Array3
  [[_ jsyn isyn osyn & none]]
  (assert (empty? none) "Expected 3 arguments to Array3")
  (->PrimitiveArray (resolve jsyn) (parse-type isyn) (parse-type osyn)))

(declare parse-function)

(defn parse-fn-intersection-type [[Fn & types]]
  (apply make-FnIntersection (mapv parse-function types)))

(defmethod parse-type-list 'Fn
  [syn]
  (parse-fn-intersection-type syn))

(declare fv-variances)

(defn parse-type-fn 
  [[_ binder bodysyn :as tfn]]
  (assert (= 3 (count tfn)))
  (assert (every? vector? binder))
  (let [free-maps (for [[nme & {:keys [variance < > kind] :as opts}] binder]
                    (do
                      (assert nme)
                      {:nme nme :variance (or variance :invariant)
                       :bound (map->Bounds 
                                {:upper-bound (when-not kind
                                                (if (contains? opts :<)
                                                  (parse-type <)
                                                  -any))
                                 :lower-bound (when-not kind
                                                (if (contains? opts :>) 
                                                  (parse-type >)
                                                  -nothing))
                                 :higher-kind (when kind
                                                (parse-type kind))})}))
        bodyt (with-bounded-frees (map (fn [{:keys [nme bound]}] [(make-F nme) bound])
                                       free-maps)
                (parse-type bodysyn))
        vs (with-bounded-frees (map (fn [{:keys [nme bound]}] [(make-F nme) bound])
                                    free-maps)
             (fv-variances bodyt))
        _ (doseq [{:keys [nme variance]} free-maps]
            (when-let [actual-v (vs nme)]
              (assert (= (vs nme) variance)
                      (error-msg "Type variable " nme " appears in " (name actual-v) " position "
                                 "when declared " (name variance)))))]
    (with-meta (TypeFn* (map :nme free-maps) (map :variance free-maps)
                        (map :bound free-maps) bodyt)
               {:actual-frees (map :nme free-maps)})))

(defmethod parse-type-list 'TFn
  [syn]
  (parse-type-fn syn))

(defmethod parse-type-list 'Seq* [syn] (->HeterogeneousSeq (mapv parse-type (rest syn))))
(defmethod parse-type-list 'List* [syn] (->HeterogeneousList (mapv parse-type (rest syn))))
(defmethod parse-type-list 'Vector* [syn] (-hvec (mapv parse-type (rest syn))))

(declare constant-type)

(defn- syn-to-hmap [mandatory optional]
  (letfn [(mapt [m]
            (into {} (for [[k v] m]
                       [(constant-type k)
                        (parse-type v)])))]
    (let [mandatory (mapt mandatory)
          optional (mapt optional)]
      (make-HMap mandatory optional))))

(defmethod parse-type-list 'quote 
  [[_ syn]]
  (cond
    ((some-fn number? keyword? symbol?) syn) (-val syn)
    (vector? syn) (-hvec (mapv parse-type syn))
    (map? syn) (syn-to-hmap syn nil)
    :else (throw (Exception. (str "Invalid use of quote:" syn)))))

(defmethod parse-type-list 'HMap
  [[_ mandatory & {:keys [optional]}]]
  (syn-to-hmap mandatory optional))

(defn parse-RClass [cls-sym params-syn]
  (let [cls (resolve cls-sym)
        _ (assert (class? cls) (str cls-sym " cannot be resolved"))
        tparams (doall (map parse-type params-syn))]
    (RClass-of (Class->symbol cls) tparams)))

(defmethod parse-type-list 'Value
  [[Value syn]]
  (constant-type syn))

(defmethod parse-type-list 'KeywordArgs
  [[_KeywordArgs_ & {:keys [optional mandatory]}]]
  (assert (= #{}
             (set/intersection (set (keys optional))
                               (set (keys mandatory)))))
  (let [optional (into {} (for [[k v] optional]
                            (do (assert (keyword? k))
                              [(->Value k) (parse-type v)])))
        mandatory (into {} (for [[k v] mandatory]
                             (do (assert (keyword? k))
                               [(->Value k) (parse-type v)])))]
    (apply Un (apply concat
                     (for [opts (map #(into {} %) (comb/subsets optional))]
                       (let [m (merge mandatory opts)
                             kss (comb/permutations (keys m))]
                         (for [ks kss]
                           (->HeterogeneousSeq (mapcat #(find m %) ks)))))))))

(defmethod parse-type-list :default 
  [[n & args :as syn]]
  (let [res (resolve n)
        rsym (cond 
               (class? res) (Class->symbol res)
               (var? res) (var->symbol res))]
    (if (free-in-scope n)
      (let [k (.higher-kind (free-in-scope-bnds n))
            _ (assert (TypeFn? k) (error-msg "Cannot invoke type variable " n))
            _ (assert (= (.nbound k) (count args)) (error-msg "Wrong number of arguments (" (count args)
                                                              ") to type function " (unparse-type k)))]
        (->TApp (free-in-scope n) (mapv parse-type args)))
      (if-let [t ((some-fn @DATATYPE-ENV @PROTOCOL-ENV @TYPE-NAME-ENV) rsym)]
        ;don't resolve if operator is declared
        (if (keyword? t)
          (cond
            ; declared names can be TFns
            (isa? t declared-name-type) (->TApp (->Name rsym) (mapv parse-type args))
            ; for now use Apps for declared Classes and protocols
            :else (->App (->Name rsym) (mapv parse-type args)))
          (->TApp (->Name rsym) (mapv parse-type args)))
        (cond
          ;a Class that's not a DataType
          (class? res) (RClass-of (Class->symbol res) (mapv parse-type args))
          :else
          ;unqualified declared protocols and datatypes
          (if-let [s (let [svar (symbol (name (ns-name *ns*)) (name n))
                           scls (symbol (munge (str (ns-name *ns*) \. (name n))))]
                       (some #(and (@TYPE-NAME-ENV %)
                                   %)
                             [svar scls]))]
            (->App (->Name s) (mapv parse-type args))
            (throw (Exception. (error-msg "Cannot parse list: " syn)))))))))

(defmethod parse-type Cons [l] (parse-type-list l))
(defmethod parse-type IPersistentList [l] (parse-type-list l))

(defmulti parse-type-symbol identity)
(defmethod parse-type-symbol 'Any [_] (->Top))
(defmethod parse-type-symbol 'Nothing [_] (Bottom))

;Symbol -> Class
(def primitives
  {'byte (RClass-of 'byte)
   'short (RClass-of 'short)
   'int (RClass-of 'int)
   'long (RClass-of 'long)
   'float (RClass-of 'float)
   'double (RClass-of 'double)
   'boolean (RClass-of 'boolean)
   'char (RClass-of 'char)
   'void -nil})

(defmethod parse-type-symbol :default
  [sym]
  (if-let [f (free-in-scope sym)]
    f
    (let [qsym (if (namespace sym)
                 sym
                 (symbol (-> *ns* ns-name name) (name sym)))
          clssym (if (some #(= \. %) (str sym))
                   sym
                   (symbol (str (munge (-> *ns* ns-name name)) \. (name sym))))]
      (cond
        (primitives sym) (primitives sym)
        (@TYPE-NAME-ENV qsym) (->Name qsym)
        (@TYPE-NAME-ENV clssym) (->Name clssym)
        ;Datatypes that are annotated in this namespace, but not yet defined
        (@DATATYPE-ENV clssym) (@DATATYPE-ENV clssym)
        (@PROTOCOL-ENV qsym) (resolve-protocol qsym)
        :else (let [res (resolve sym)]
                ;(prn *ns* "res" sym "->" res)
                (cond 
                  (class? res) (or (@DATATYPE-ENV (symbol (.getName ^Class res)))
                                   (RClass-of (Class->symbol res) nil))
                  :else (if-let [t (and (var? res) 
                                        (@TYPE-NAME-ENV (var->symbol res)))]
                          t
                          (throw (Exception. (error-msg "Cannot resolve type: " sym))))))))))

(defmethod parse-type Symbol [l] (parse-type-symbol l))
(defmethod parse-type Boolean [v] (if v -true -false)) 
(defmethod parse-type nil [_] -nil)

(declare parse-path parse-path-elem parse-filter)

(defn parse-object [{:keys [id path]}]
  (->Path (when path (mapv parse-path-elem path)) id))

(defn parse-filter-set [{:keys [then else] :as fsyn}]
  (-FS (if then
         (parse-filter then)
         -top)
       (if else
         (parse-filter else)
         -top)))

(declare parse-path)

(defmulti parse-filter first)

(defmethod parse-filter 'is
  [[_ & [tsyn nme psyn :as all]]]
  (assert ((some-fn #(= 2 %) #(= 3 %)) (count all)))
  (let [t (parse-type tsyn)
        p (when (= 3 (count all))
            (parse-path psyn))]
    (-filter t nme p)))

(defmethod parse-filter '!
  [[_ & [tsyn nme psyn :as all]]]
  (assert ((some-fn #(= 2 %) #(= 3 %)) (count all)))
  (let [t (parse-type tsyn)
        p (when (= 3 (count all))
            (parse-path psyn))]
    (-not-filter t nme p)))

(defmethod parse-filter '|
  [[_ & fsyns]]
  (apply -or (mapv parse-filter fsyns)))

(defmethod parse-filter '&
  [[_ & fsyns]]
  (apply -and (mapv parse-filter fsyns)))

(defmulti parse-path-elem #(cond
                             (symbol? %) %
                             :else (first %)))

(defmethod parse-path-elem 'Class [_] (->ClassPE))

(defmethod parse-path-elem 'Key
  [[_ & [ksyn :as all]]]
  (assert (= 1 (count all)))
  (->KeyPE ksyn))

(defn parse-path [[psyn id :as all]]
  (assert (= 2 (count all)))
  (->Path (parse-path-elem psyn) id))

(defn parse-function [f]
  (let [all-dom (take-while #(not= '-> %) f)
        [_ rng & opts-flat :as chk] (drop-while #(not= '-> %) f) ;opts aren't used yet
        _ (assert (<= 2 (count chk)) (str "Missing range in " f))

        opts (apply hash-map opts-flat)

        {ellipsis-pos '...
         asterix-pos '*}
        (into {} (map vector all-dom (range)))

        _ (assert (not (and asterix-pos ellipsis-pos))
                  "Cannot provide both rest type and dotted rest type")

        _ (when-let [ks (seq (filter #(not (#{:filters :object} %)) (keys opts)))]
            (throw (Exception. (str "Invalid option/s: " ks))))

        filters (when-let [[_ fsyn] (find opts :filters)]
                  (parse-filter-set fsyn))

        object (when-let [[_ obj] (find opts :object)]
                 (parse-object obj))

        fixed-dom (cond 
                    asterix-pos (take (dec asterix-pos) all-dom)
                    ellipsis-pos (take (dec ellipsis-pos) all-dom)
                    :else all-dom)

        rest-type (when asterix-pos
                    (nth all-dom (dec asterix-pos)))
        [drest-type _ drest-bnd] (when ellipsis-pos
                                   (drop (dec ellipsis-pos) all-dom))]
    (make-Function (doall (mapv parse-type fixed-dom))
                   (parse-type rng)
                   (when asterix-pos
                     (parse-type rest-type))
                   (when ellipsis-pos
                     (->DottedPretype
                       (with-frees [(*dotted-scope* drest-bnd)] ;with dotted bound in scope as free
                         (parse-type drest-type))
                       (:name (*dotted-scope* drest-bnd))))
                   :filter filters
                   :object object)))

(defmethod parse-type IPersistentVector
  [f]
  (apply make-FnIntersection [(parse-function f)]))
