(in-ns 'typed.core)

(def ^:dynamic *next-nme* 0) ;stupid readable variables

(declare unparse-type* unparse-object unparse-filter-set unparse-filter)

(defn unparse-type [t]
  (if-let [nsym (-> t meta :source-Name)]
    nsym
    (unparse-type* t)))

(defmulti unparse-type* class)
(defn unp [t] (prn (unparse-type t)))

(defmethod unparse-type* Top [_] 'Any)
(defmethod unparse-type* Name [{:keys [id]}] id)
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
            *free-scope*)
      name))

(defmethod unparse-type* PrimitiveArray
  [{:keys [input-type output-type]}]
  (list 'Array (unparse-type input-type) (unparse-type output-type)))

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

(defmethod unparse-type* Function
  [{:keys [dom rng rest drest]}]
  (vec (concat (doall (map unparse-type dom))
               (when rest
                 [(unparse-type rest) '*])
               (when drest
                 (let [{:keys [pre-type name]} drest]
                   [(unparse-type pre-type) '... name]))
               (let [{:keys [t fl o]} rng]
                 (concat ['-> (unparse-type t)]
                         (when (not (and ((some-fn TopFilter? BotFilter?) (:then fl))
                                         ((some-fn TopFilter? BotFilter?) (:else fl))))
                           [(unparse-filter-set fl)])
                         (when (not ((some-fn NoObject? EmptyObject?) o))
                           [(unparse-object o)]))))))

(defmethod unparse-type* Protocol
  [{:keys [the-var poly?]}]
  (if poly?
    (list* the-var (mapv unparse-type poly?))
    the-var))

(defmethod unparse-type* DataType
  [{:keys [the-class poly?]}]
  (if poly?
    (list* the-class (mapv unparse-type poly?))
    the-class))

(defmulti unparse-RClass :the-class)

(defmethod unparse-RClass 'clojure.lang.Atom
  [{:keys [the-class poly?]}]
  (let [[w r] poly?]
    (list* the-class (map unparse-type (concat [w]
                                               (when (not= w r)
                                                 [r]))))))

(defmethod unparse-RClass :default
  [{:keys [the-class poly?]}]
  (list* the-class (doall (map unparse-type poly?))))

(defmethod unparse-type* RClass
  [{:keys [the-class poly?] :as r}]
  (if (empty? poly?)
    the-class
    (unparse-RClass r)))

(defmethod unparse-type* Mu
  [m]
  (let [nme (gensym "Mu")
        body (Mu-body* nme m)]
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
        body (PolyDots-body* fs p)]
    (binding [*next-nme* end-nme]
      (list 'All (vec (concat (butlast fs) [(last fs) '...])) (unparse-type body)))))

(defmethod unparse-type* Poly
  [{:keys [nbound] :as p}]
  (let [free-names (Poly-free-names* p)
        given-names? free-names
        end-nme (if given-names?
                  *next-nme*
                  (+ nbound *next-nme*))
        fs-names (or (and given-names? free-names)
                     (vec
                       (for [x (range *next-nme* end-nme)]
                         (symbol (str "v" x)))))
        bbnds (Poly-bbnds* fs-names p)
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
                       (when-not (or (Top? upper-bound) (Bottom? lower-bound))
                         [name :< u :> l])
                       (when-not (Top? upper-bound) 
                         [name :< u])
                       (when-not (Bottom? lower-bound)
                         [name :> l])
                       name))))
             fs-names)
        body (Poly-body* fs-names p)]
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
        bbnds (TypeFn-bbnds* fs-names p)
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
                       (when-not (or (Top? upper-bound) (Bottom? lower-bound))
                         [name :< u :> l])
                       (when-not (Top? upper-bound) 
                         [name :< u])
                       (when-not (Bottom? lower-bound)
                         [name :> l])
                       name))))
             fs-names)
        body (TypeFn-body* fs-names p)]
    (binding [*next-nme* end-nme]
      (list 'TFn fs (unparse-type body)))))

(defmethod unparse-type* Value
  [v]
  (if ((some-fn Nil? True? False?) v)
    (:val v)
    (list 'Value (:val v))))

(defmethod unparse-type* HeterogeneousMap
  [v]
  (list 'HMap (into {} (map (fn [[k v]]
                              (assert (Value? k))
                              (vector (:val k)
                                      (unparse-type v)))
                            (:types v)))))

(defmethod unparse-type* HeterogeneousSeq
  [v]
  (list* 'Seq* (doall (map unparse-type (:types v)))))

(defmethod unparse-type* HeterogeneousVector
  [v]
  (mapv unparse-type (:types v)))

(defmethod unparse-type* HeterogeneousList
  [v]
  (list* 'List* (doall (map unparse-type (:types v)))))

; Objects

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

(declare FilterSet? unparse-filter)

(defn unparse-filter-set [{:keys [then else] :as fs}]
  {:pre [(FilterSet? fs)]}
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
          (when path
            [(map unparse-path-elem path)])))

(defmethod unparse-filter* AndFilter [{:keys [fs]}] (apply list '& (map unparse-filter fs)))
(defmethod unparse-filter* OrFilter [{:keys [fs]}] (apply list '| (map unparse-filter fs)))

(defmethod unparse-filter* ImpFilter
  [{:keys [a c]}]
  (list 'when (unparse-filter a) (unparse-filter c)))

