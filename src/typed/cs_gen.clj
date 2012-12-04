(in-ns 'typed.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint Generation

(defrecord t-subst [type bnds]
  ""
  [(Type? type)
   (Bounds? bnds)])

(defrecord i-subst [types]
  ""
  [(every? Type? types)])

(defrecord i-subst-starred [types starred]
  ""
  [(every? Type? types)
   (Type? starred)])

(defrecord i-subst-dotted [types dty dbound]
  ""
  [(or (nil? types)
       (every? Type? types))
   (Type? dty)
   (F? dbound)])

(def subst-rhs? (some-fn t-subst? i-subst? i-subst-starred? i-subst-dotted?))

(def substitution-c? (every-pred map? 
                                 #(every? symbol? (keys %)) 
                                 #(every? subst-rhs? (vals %))))

(defrecord c [S X T bnds]
  "A type constraint on a variable within an upper and lower bound"
  [(Type? S)
   (symbol? X)
   (Type? T)
   (Bounds? bnds)])

;; fixed : Listof[c]
;; rest : option[c]
;; a constraint on an index variable
;; the index variable must be instantiated with |fixed| arguments, each meeting the appropriate constraint
;; and further instantions of the index variable must respect the rest constraint, if it exists
(defrecord dcon [fixed rest]
  ""
  [(every? c? fixed)
   (or (nil? rest)
       (c? rest))])

(defrecord dcon-exact [fixed rest]
  ""
  [(every? c? fixed)
   (c? rest)])

(defrecord dcon-dotted [fixed dc dbound]
  ""
  [(every? c? fixed)
   (c? dc)
   (F? dbound)])

(def dcon-c? (some-fn dcon? dcon-exact? dcon-dotted?))

;; map : hash mapping index variables to dcons
(defrecord dmap [map]
  ""
  [((hash-c? symbol? dcon-c?) map)])

(defrecord cset-entry [fixed dmap projections]
  ""
  [((hash-c? symbol? c?) fixed)
   (dmap? dmap)
   ((set-c? (hvector-c? (some-fn Type? Projection?)
                        (some-fn Type? Projection?)))
     projections)])

(defn make-cset-entry
  ([fixed] (make-cset-entry fixed nil nil))
  ([fixed dmap] (make-cset-entry fixed dmap nil))
  ([fixed dmap projections] (->cset-entry fixed 
                                          (or dmap (->dmap {}))
                                          (or projections #{}))))

;; maps is a list of cset-entries, consisting of
;;    - functional maps from vars to c's
;;    - dmaps (see dmap.rkt)
;; we need a bunch of mappings for each cset to handle case-lambda
;; because case-lambda can generate multiple possible solutions, and we
;; don't want to rule them out too early
(defrecord cset [maps]
  ""
  [(every? cset-entry? maps)])


;widest constraint possible
(defn no-constraint [v bnds]
  {:pre [(symbol? v)
         (Bounds? bnds)]}
  (->c (Un) v (->Top) bnds))

;; Create an empty constraint map from a set of type variables X and
;; index variables Y.  For now, we add the widest constraints for
;; variables in X to the cmap and create an empty dmap.
(defn empty-cset [X Y]
  {:pre [(every? (hash-c? symbol? Bounds?) [X Y])]
   :post [(cset? %)]}
  (->cset [(->cset-entry (into {} (for [[x bnds] X] [x (no-constraint x bnds)]))
                         (->dmap {})
                         #{})]))

(defn empty-cset-projection [X Y proj]
  {:pre [(every? (hash-c? symbol? Bounds?) [X Y])]
   :post [(cset? %)]}
  (->cset [(->cset-entry (into {} (for [[x bnds] X] [x (no-constraint x bnds)]))
                         (->dmap {})
                         #{proj})]))

(defn meet [s t] (In s t))
(defn join [s t] (Un s t))

(declare subtype type-error)

(defn c-meet [{S  :S X  :X T  :T bnds  :bnds :as c1}
              {S* :S X* :X T* :T bnds* :bnds :as c2}
              & [var]]
  #_(prn "c-meet" c1 c2)
  (when-not (or var (= X X*))
    (throw (Exception. (str "Non-matching vars in c-meet:" X X*))))
  (when-not (= bnds bnds*)
    (throw (Exception. (str "Non-matching bounds in c-meet:" bnds bnds*))))
  (let [S (join S S*)
        T (meet T T*)]
    (when-not (subtype? S T)
      (type-error S T))
    (->c S (or var X) T bnds)))

(declare dmap-meet)

(defn cset-meet [{maps1 :maps :as x} {maps2 :maps :as y}]
  {:pre [(cset? x)
         (cset? y)]}
  (let [maps (doall (for [[{map1 :fixed dmap1 :dmap prj1 :projections} 
                           {map2 :fixed dmap2 :dmap prj2 :projections}] (map vector maps1 maps2)]
                      (->cset-entry (merge-with c-meet map1 map2)
                                    (dmap-meet dmap1 dmap2)
                                    (set/union prj1 prj2))))]
    (when (empty? maps)
      (throw (Exception. (str "No meet found for csets"))))
    (->cset maps)))

(defn cset-meet* [args]
  {:pre [(every? cset? args)]
   :post [(cset? %)]}
  (reduce (fn [a c] (cset-meet a c))
          (->cset [(->cset-entry {} (->dmap {}) #{})])
          args))

(defn cset-combine [l]
  {:pre [(every? cset? l)]}
  (let [mapss (map :maps l)]
    (->cset (apply concat mapss))))

;add new constraint to existing cset
(defn insert-constraint [cs var S T bnds]
  {:pre [(cset? cs)
         (symbol? var)
         (Type? S)
         (Type? T)
         (Bounds? bnds)]
   :post [(cset? %)]}
  (->cset (doall
            (for [{fmap :fixed dmap :dmap} (:maps cs)]
              (->cset-entry (assoc fmap var (->c S var T bnds))
                            dmap
                            #{})))))

(defn dcon-meet [dc1 dc2]
  {:pre [(dcon-c? dc1)
         (dcon-c? dc2)]
   :post [(dcon-c? %)]}
  (cond
    (and (dcon-exact? dc1)
         ((some-fn dcon? dcon-exact?) dc2))
    (let [{fixed1 :fixed rest1 :rest} dc1
          {fixed2 :fixed rest2 :rest} dc2]
      (when-not (and rest2 (= (count fixed1) (count fixed2)))
        (type-error fixed1 fixed2))
      (->dcon-exact
        (doall
          (for [[c1 c2] (map vector fixed1 fixed2)]
            (c-meet c1 c2 (:X c1))))
        (c-meet rest1 rest2 (:X rest1))))
    ;; redo in the other order to call the first case
    (and (dcon? dc1)
         (dcon-exact? dc2))
    (dcon-meet dc2 dc1)

    (and (dcon? dc1)
         (not (:rest dc1))
         (dcon? dc2)
         (not (:rest dc2)))
    (let [{fixed1 :fixed} dc1
          {fixed2 :fixed} dc2]
      (when-not (= (count fixed1) (count fixed2))
        (throw (Exception. (prn-str "Don't match: " fixed1 fixed2))))
      (->dcon
        (doall
          (for [[c1 c2] (map vector fixed1 fixed2)]
            (c-meet c1 c2 (:X c1))))
        nil))

    (and (dcon? dc1)
         (not (:rest dc1))
         (dcon? dc2))
    (let [{fixed1 :fixed} dc1
          {fixed2 :fixed rest :rest} dc2]
      (when-not (>= (count fixed1) (count fixed2))
        (throw (Exception. (prn-str "Don't match: " fixed1 fixed2))))
      (->dcon
        (doall
          (for [[c1 c2] (map vector fixed1 (concat fixed2 (repeat rest)))]
            (c-meet c1 c2 (:X c1))))
        nil))

    (and (dcon? dc1)
         (dcon? dc2)
         (not (:rest dc2)))
    (dcon-meet dc2 dc1)

    (and (dcon? dc1)
         (dcon? dc2))
    (let [{fixed1 :fixed rest1 :rest} dc1
          {fixed2 :fixed rest2 :rest} dc2
          [shorter longer srest lrest]
          (if (< (count fixed1) (count fixed2))
            [fixed1 fixed2 rest1 rest2]
            [fixed2 fixed1 rest2 rest1])]
      (->dcon
        (doall
          (for [[c1 c2] (map vector longer (concat shorter (repeat srest)))]
            (c-meet c1 c2 (:X c1))))
        (c-meet lrest srest (:X lrest))))

    (and (dcon-dotted? dc1)
         (dcon-dotted? dc2))
    (let [{fixed1 :fixed c1 :dc {bound1 :name} :dbound} dc1
          {fixed2 :fixed c2 :dc {bound2 :name} :dbound} dc2]
      (when-not (and (= (count fixed1) (count fixed2))
                     (= bound1 bound2))
        (throw (Exception. (prn-str "Don't match: " bound1 bound2))))
      (->dcon-dotted (doall (for [[c1 c2] (map vector fixed1 fixed2)]
                              (c-meet c1 c2 (:X c1))))
                     (c-meet c1 c2 bound1) bound1))

    (and (dcon? dc1)
         (dcon-dotted? dc2))
    (throw (Exception. (prn-str "Don't match: " dc1 dc2)))

    (and (dcon-dotted? dc1)
         (dcon? dc2))
    (throw (Exception. (prn-str "Don't match: " dc1 dc2)))

    :else (throw (Exception. (prn-str "Got non-dcons: " dc1 dc2)))))

(defn dmap-meet [dm1 dm2]
  {:pre [(dmap? dm1)
         (dmap? dm2)]
   :post [(dmap? %)]}
  (->dmap (merge-with dcon-meet (:map dm1) (:map dm2))))


;current seen subtype relations, for recursive types
;(Set [Type Type])
(def ^:dynamic *cs-current-seen* #{})

;; V : a set of variables not to mention in the constraints
;; X : the set of type variables to be constrained
;; Y : the set of index variables to be constrained
;; S : a type to be the subtype of T
;; T : a type
;; produces a cset which determines a substitution that makes S a subtype of T
;; implements the V |-_X S <: T => C judgment from Pierce+Turner, extended with
;; the index variables from the TOPLAS paper
(defmulti cs-gen*
  (fn [V X Y S T] 
    {:pre [((set-c? symbol?) V)
           (every? (hash-c? symbol Bounds?) [X Y])
           (AnyType? S)
           (AnyType? T)]}
    [(class S) (class T) @TYPED-IMPL]))

; (see cs-gen*)
;cs-gen calls cs-gen*, remembering the current subtype for recursive types
; Add methods to cs-gen*, but always call cs-gen

(declare cs-gen-right-F cs-gen-left-F)

(defn cs-gen [V X Y S T]
  {:pre [((set-c? symbol?) V)
         (every? (hash-c? symbol? Bounds?) [X Y])
         (AnyType? S)
         (AnyType? T)]
   :post [(cset? %)]}
  #_(prn "cs-gen" (unparse-type S) (unparse-type T))
  (if (or (*cs-current-seen* [S T]) 
          (subtype? S T))
    ;already been around this loop, is a subtype
    (empty-cset X Y)
    (binding [*cs-current-seen* (conj *cs-current-seen* [S T])]
      (cond
        (Top? T)
        (empty-cset X Y)

        ; handle frees first
        (and (F? S)
             (contains? X (.name S)))
        (cs-gen-left-F V X Y S T)

        (and (F? T)
             (contains? X (.name T)))
        (cs-gen-right-F V X Y S T)

        ;; constrain body to be below T, but don't mention the new vars
        (Poly? S)
        (let [nms (repeatedly (.nbound S) gensym)
              body (Poly-body* nms S)]
          (cs-gen (set/union (set nms) V) X Y body T))

        (and (TApp? S)
             (not (F? (.rator S))))
        (cs-gen V X Y (resolve-TApp S) T)

        (and (TApp? T)
             (not (F? (.rator T))))
        (cs-gen V X Y S (resolve-TApp T))

        ;constrain *each* element of S to be below T, and then combine the constraints
        (Union? S)
        (cset-meet*
          (cons (empty-cset X Y)
                (mapv #(cs-gen V X Y % T) (.types S))))

        ;; find *an* element of T which can be made a supertype of S
        (Union? T)
        (if-let [cs (seq (filter identity (mapv #(try (cs-gen V X Y S %)
                                                   (catch IllegalArgumentException e
                                                     (throw e))
                                                   (catch Exception e)) 
                                                (.types T))))]
          (cset-combine cs)
          (type-error S T))

        (and (Intersection? S)
             (Intersection? T))
        (cset-meet*
          (doall
            ; for each element of T, we need at least one element of S that works
            (for [t* (:types T)]
              (if-let [results (seq (filter identity
                                            (map #(try 
                                                    (cs-gen V X Y % t*)
                                                    (catch IllegalArgumentException e
                                                      (throw e))
                                                    (catch Exception e))
                                                 (:types S))))]
                (cset-combine results)
                (type-error S T)))))

        ;; find *an* element of S which can be made a subtype of T
        (Intersection? S)
        (if-let [cs (some #(try (cs-gen V X Y % T)
                             (catch IllegalArgumentException e
                               (throw e))
                             (catch Exception e)) ;TODO specialised data Exceptions
                          (:types S))]
          cs
          (throw (Exception. (str "Could not constrain "
                                  (unparse-type S) " to be under "
                                  (unparse-type T)))))

        ;constrain *every* element of T to be above S, and then meet the constraints
        ;FIXME Should this combine csets instead?
        (Intersection? T)
        (cset-meet*
          (cons (empty-cset X Y)
                (mapv #(cs-gen V X Y S %) (:types T))))

        (App? S)
        (cs-gen V X Y (resolve-App S) T)

        (App? T)
        (cs-gen V X Y S (resolve-App T))

        (or (Projection? S)
            (Projection? T))
        (empty-cset-projection X Y [S T])

        :else
        (cs-gen* V X Y S T)))))

;; FIXME - anything else to say about And and OrFilters?
(defn cs-gen-filter [V X Y s t]
  {:pre [((set-c? symbol?) V)
         (every? (hash-c? symbol? Bounds?) [X Y])
         (Filter? s)
         (Filter? t)]
   :post [(cset? %)]}
  (cond
    (= s t) (empty-cset X Y)
    (TopFilter? t) (empty-cset X Y)

    (and (TypeFilter? s)
         (TypeFilter? t))
    (cset-meet (cs-gen V X Y (:type s) (:type t))
               (cs-gen V X Y (:type t) (:type s)))
    (and (NotTypeFilter? s)
         (NotTypeFilter? t))
    (cset-meet (cs-gen V X Y (:type s) (:type t))
               (cs-gen V X Y (:type t) (:type s)))

    ; simple case for unifying x and y in (& (is x sym) ...) (is y sym)
    (and (AndFilter? s)
         (TypeFilter? t)
         (every? atomic-filter? (:fs s))
         (= 1 (count TypeFilter?) (:fs s)))
    (let [tf (first (filter TypeFilter? (:fs s)))]
      (cs-gen-filter V X Y tf t))
    :else (throw (IllegalArgumentException. (error-msg "Need two filters of same type "
                                                       (pr-str (unparse-filter s)) " " (pr-str (unparse-filter t)))))))

;must be *latent* filter sets
(defn cs-gen-filter-set [V X Y s t]
  {:pre [((set-c? symbol?) V)
         (every? (hash-c? symbol? Bounds?) [X Y])
         (FilterSet? s)
         (FilterSet? t)]
   :post [(cset? %)]}
  (cond
    (= s t) (empty-cset X Y)
    (and (FilterSet? s)
         (FilterSet? t))
    (let [{s+ :then s- :else} s
          {t+ :then t- :else} t]
      (cset-meet (cs-gen-filter V X Y s+ t+)
                 (cs-gen-filter V X Y s- t-)))
    :else (throw (IllegalArgumentException. "Need two filtersets"))))

(defn cs-gen-object [V X Y s t]
  {:pre [((set-c? symbol?) V)
         (every? (hash-c? symbol? Bounds?) [X Y])
         (RObject? s)
         (RObject? t)]
   :post [(cset? %)]}
  (cond
    (= s t) (empty-cset X Y)
    (EmptyObject? t) (empty-cset X Y)
    ;;FIXME do something here
    :else (throw (IllegalArgumentException. (when *current-env*
                                              (str (:line *current-env*) ":"))
                                            "Objects don't match"))))

(defmethod cs-gen* :default
  [V X Y S T]
#_(prn "cs-gen* default" (class S) (class T))
  (when (some Result? [S T])
    (throw (IllegalArgumentException. (error-msg "Result on left or right "
                                                 (pr-str S) " " (pr-str T)))))
  (assert (subtype? S T) (type-error S T))
  (empty-cset X Y))

(declare cs-gen-Function)

(defmethod cs-gen* [TApp TApp ::default]
  [V X Y S T]
  (assert (= (.rator S) (.rator T)) (type-error S T))
  (cset-meet*
    (mapv #(cs-gen V X Y %1 %2) (.rands S) (.rands T))))

(defmethod cs-gen* [FnIntersection FnIntersection ::default]
  [V X Y S T] 
  (cset-meet*
    (doall
      (for [t-arr (.types T)]
        ;; for each t-arr, we need to get at least s-arr that works
        (let [results (filter identity
                              (doall
                                (for [s-arr (.types S)]
                                  (try
                                    (cs-gen-Function V X Y s-arr t-arr)
                                    (catch IllegalArgumentException e
                                      (throw e))
                                    (catch IllegalStateException e
                                      (throw e))
                                    (catch Exception e
                                      #_(pst e))))))]
          ;; ensure that something produces a constraint set
          (when (empty? results) 
            (type-error S T))
          (cset-combine results))))))

(defmethod cs-gen* [Result Result ::default]
  [V X Y S T] 
  (cset-meet* [(cs-gen V X Y (Result-type* S) (Result-type* T))
               (cs-gen-filter-set V X Y (Result-filter* S) (Result-filter* T))
               (cs-gen-object V X Y (Result-object* S) (Result-object* T))]))

(defmethod cs-gen* [Value AnyValue ::default] 
  [V X Y S T] 
  (empty-cset X Y))

(defmethod cs-gen* [Type Top ::default]
  [V X Y S T] 
  (empty-cset X Y))

(defmethod cs-gen* [HeterogeneousVector RClass ::clojure]
  [V X Y S T]
  (cs-gen V X Y 
          (In (RClass-of APersistentVector [(apply Un (:types S))]) 
              (make-ExactCountRange (count (:types S))))
          T))

(declare cs-gen-list)

(defmethod cs-gen* [DataType DataType ::default]
  [V X Y S T]
  (assert (= (:the-class S) (:the-class T)) (type-error S T))
  (if (seq (:poly? S))
    (cs-gen-list V X Y (:poly? S) (:poly? T))
    (empty-cset X Y)))

(defmethod cs-gen* [HeterogeneousVector HeterogeneousVector ::default] 
  [V X Y S T]
  (cs-gen-list V X Y (:types S) (:types T)))

(defmethod cs-gen* [HeterogeneousMap HeterogeneousMap ::default]
  [V X Y S T]
  (let [Skeys (set (keys (:types S)))
        Tkeys (set (keys (:types T)))]
    ; All keys must be values
    (when-not (every? Value? (set/union Skeys Tkeys))
      (type-error S T))
    ; All keys on the left must appear on the right
    (when-not (empty? (set/difference Skeys Tkeys))
      (type-error S T))
    (let [nocheck-keys (set/difference Tkeys Skeys)
          STvals (vals (merge-with vector (:types S) (apply dissoc (:types T) nocheck-keys)))
          Svals (map first STvals)
          Tvals (map second STvals)]
      (cs-gen-list V X Y Svals Tvals))))

(defmethod cs-gen* [HeterogeneousMap RClass ::clojure]
  [V X Y S T]
  ; HMaps do not record absence of fields, only subtype to (APersistentMap Any Any)
  (cs-gen V X Y (RClass-of APersistentMap [-any -any]) T))

(defmethod cs-gen* [RClass RClass ::clojure]
  [V X Y S T]
  (let [relevant-S (some #(and (= (:the-class %) (:the-class T))
                               %)
                         (conj (RClass-supers* S) S))]
    (cond
      relevant-S
      (cset-meet*
        (cons (empty-cset X Y)
              (doall
                (for [[vari si ti] (map vector
                                        (:variances T)
                                        (:poly? relevant-S)
                                        (:poly? T))]
                  (case vari
                    (:covariant :constant) (cs-gen V X Y si ti)
                    :contravariant (cs-gen V X Y ti si)
                    :invariant (cset-meet (cs-gen V X Y si ti)
                                          (cs-gen V X Y ti si)))))))
      :else (type-error S T))))

(defn demote-F [V X Y {:keys [name bnds] :as S} T]
  {:pre [(F? S)]}
  ;constrain T to be below S (but don't mention V)
  (assert (contains? X name) (str X name))
  (when (and (F? T)
             (bound-index? (:name T))
             (not (free-in-scope (:name T))))
    (type-error S T))
  (let [dt (demote-var T V)]
    (-> (empty-cset X Y)
      (insert-constraint name (Bottom) dt (X name)))))

(defn promote-F [V X Y S {:keys [name] :as T}]
  {:pre [(F? T)]}
  ;T is an F
  ;S is any Type
  ;constrain T to be above S (but don't mention V)
  (assert (contains? X name) (str X T))
  (when (and (F? S)
             (bound-index? (:name S))
             (not (free-in-scope (:name S))))
    (type-error S T))
  (let [ps (promote-var S V)]
    (-> (empty-cset X Y)
      (insert-constraint name ps -any (X name)))))

(defn cs-gen-left-F [V X Y S T]
  #_(prn "cs-gen* [F Type]" S T)
  (cond
    (contains? X (.name S))
    (demote-F V X Y S T)

    (and (F? T)
         (contains? X (.name T)))
    (promote-F V X Y S T)

    :else (type-error S T)))

(defn cs-gen-right-F [V X Y S T]
  ;(prn "cs-gen* [Type F]" S T X)
  (cond
    (contains? X (:name T))
    (promote-F V X Y S T)

    (and (F? S)
         (contains? X (:name S)))
    (demote-F V X Y S T)

    :else (type-error S T)))

(defn singleton-dmap [dbound dcon]
  (->dmap {dbound dcon}))

(defn mover [cset dbound vars f]
  {:pre [(cset? cset)
         (symbol? dbound)
         (every? symbol? vars)]
   :post [(cset? %)]}
  (->cset (map
            (fn [{cmap :fixed dmap :dmap}]
              (->cset-entry (apply dissoc cmap dbound vars)
                            (dmap-meet 
                              (singleton-dmap 
                                dbound
                                (f cmap dmap))
                              (->dmap (dissoc (:map dmap) dbound)))
                            #{}))
            (:maps cset))))

;; dbound : index variable
;; cset : the constraints being manipulated
(defn move-rest-to-dmap [cset dbound & {:keys [exact]}]
  {:pre [(cset? cset)
         (symbol? dbound)
         ((some-fn nil? true?) exact)]
   :post [(cset? %)]}
  (mover cset dbound nil
         (fn [cmap dmap]
           ((if exact ->dcon-exact ->dcon)
              nil
              (if-let [c (cmap dbound)]
                c
                (throw (Exception. (str "No constraint for bound " dbound))))))))


;; dbound : index variable
;; vars : listof[type variable] - temporary variables
;; cset : the constraints being manipulated
;; takes the constraints on vars and creates a dmap entry contstraining dbound to be |vars|
;; with the constraints that cset places on vars
(defn move-vars-to-dmap [cset dbound vars]
  {:pre [(cset? cset)
         (symbol? dbound)
         (every? symbol? vars)]
   :post [(cset? %)]}
  (mover cset dbound vars
         (fn [cmap dmap]
           (->dcon (doall (for [v vars]
                            (if-let [c (cmap v)]
                              c
                              (throw (Exception. (str "No constraint for new var " v))))))
                   nil))))

;; This one's weird, because the way we set it up, the rest is already in the dmap.
;; This is because we create all the vars, then recall cgen/arr with the new vars
;; in place, and the "simple" case will then call move-rest-to-dmap.  This means
;; we need to extract that result from the dmap and merge it with the fixed vars
;; we now handled.  So I've extended the mover to give access to the dmap, which we use here.
(defn move-vars+rest-to-dmap [cset dbound vars & {:keys [exact]}]
  {:pre [(cset? cset)
         (symbol? dbound)
         ((set-c? symbol?) vars)
         ((some-fn nil? true?) exact)]
   :post [(cset? %)]}
  (mover cset dbound vars
         (fn [cmap dmap]
           ((if exact ->dcon-exact ->dcon)
              (doall
                (for [v vars]
                  (if-let [c (cmap v)]
                    c
                    (throw (Exception. (str "No constraint for new var " v))))))
              (if-let [c ((:map dmap) dbound)]
                (cond
                  (and (dcon? c)
                       (not (:fixed c))) (:rest c)
                  (and (dcon-exact? c)
                       (not (:fixed c))) (:rest c)
                  :else (throw (Exception. (str "did not a get a rest-only dcon when moving to the dmap"))))
                (throw (Exception. (str "No constraint for bound " dbound))))))))

;; Maps dotted vars (combined with dotted types, to ensure global uniqueness)
;; to "fresh" symbols.
;; That way, we can share the same "fresh" variables between the elements of a
;; cset if they're talking about the same dotted variable.
;; This makes it possible to reduce the size of the csets, since we can detect
;; identical elements that would otherwise differ only by these fresh vars.
;; The domain of this map is pairs (var . dotted-type).
;; The range is this map is a list of symbols generated on demand, as we need
;; more dots.
(def ^:private DOTTED-VAR-STORE (atom {}))

;; Take (generate as needed) n symbols that correspond to variable var used in
;; the context of type t.
(defn- var-store-take [var t n]
  (let [key [t n]
        res (@DOTTED-VAR-STORE key)]
    (if (>= (count res) n)
      ;; there are enough symbols already, take n
      (take n res)
      ;; we need to generate more
      (let [new (take (- n (count res))
                      (repeatedly #(gensym var)))
            all (concat res new)]
        (swap! DOTTED-VAR-STORE assoc key all)
        all))))

(declare cs-gen-list)

(defn cs-gen-Function
  [V X Y S T]
  {:pre [((set-c? symbol?) V)
         (every? (hash-c? symbol? Bounds?) [X Y])
         (Function? S)
         (Function? T)]
   :post [(cset? %)]}
  ;(prn "cs-gen-Function")
  (letfn [(cg [S T] (cs-gen V X Y S T))]
    (cond
      ;easy case - no rests, drests, kws
      (and (not (:rest S))
           (not (:rest T))
           (not (:drest S))
           (not (:drest T))
           (not (:kws S))
           (not (:kws T)))
      ; contravariant
      (cset-meet* [(cs-gen-list V X Y (:dom T) (:dom S))
                   ; covariant
                   (cg (:rng S) (:rng T))])

      ;just a rest arg, no drest, no keywords
      (and (or (:rest S)
               (:rest T))
           (not (:drest S))
           (not (:drest T))
           (not (:kws S))
           (not (:kws T)))
      (let [arg-mapping (cond
                          ;both rest args are present, so make them the same length
                          (and (:rest S) (:rest T))
                          (cs-gen-list V X Y 
                                       (cons (:rest T) (concat (:dom T) (repeat (- (count (:dom S))
                                                                                   (count (:dom T)))
                                                                                (:rest T))))
                                       (cons (:rest S) (concat (:dom S) (repeat (- (count (:dom T))
                                                                                   (count (:dom S)))
                                                                                (:rest S)))))
                          ;no rest arg on the right, so just pad left and forget the rest arg
                          (and (:rest S) (not (:rest T)))
                          (let [new-S (concat (:dom S) (repeat (- (count (:dom T))
                                                                  (count (:dom S)))
                                                               (:rest S)))]
;                            (prn "infer rest arg on left")
;                            (prn "left dom" (map unparse-type (:dom S)))
;                            (prn "right dom" (map unparse-type (:dom T)))
;                            (prn "new left dom" (map unparse-type new-S))
                            (cs-gen-list V X Y (:dom T) new-S))
                          ;no rest arg on left, or wrong number = fail
                          :else (type-error S T))
            ret-mapping (cs-gen V X Y (:rng S) (:rng T))]
        (cset-meet* [arg-mapping ret-mapping]))

      ;; dotted on the left, nothing on the right
      (and (not (:rest S))
           (not (:rest T))
           (:drest S)
           (not (:drest T))
           (not (:kws S))
           (not (:kws T)))
      (let [{dty :pre-type dbound :name} (:drest S)]
        (when-not (Y dbound)
          (type-error S T))
        (when-not (<= (count (:dom S)) (count (:dom T)))
          (type-error S T))
        (let [vars (var-store-take dbound dty (- (count (:dom S))
                                                 (count (:dom T))))
              new-tys (doall (for [var vars]
                               (substitute (make-F var) dbound dty)))
              new-t-fun (make-Function (concat (:dom T) new-tys) (:rng T))
              new-cset (cs-gen-Function V 
                                        ;move dotted lower/upper bounds to vars
                                        (merge X (zipmap vars (repeat (Y dbound)))) Y S new-t-fun)]
          (move-vars-to-dmap new-cset dbound vars)))

      ;; dotted on the right, nothing on the left
      (and (not ((some-fn :rest :drest) S))
           (:drest T))
      (let [{dty :pre-type dbound :name} (:drest T)]
        (when-not (Y dbound)
          (type-error S T))
        (when-not (<= (count (:dom T)) (count (:dom S)))
          (type-error S T))
        (let [vars (var-store-take dbound dty (- (count (:dom S)) (count (:dom T))))
              new-tys (doall
                        (for [var vars]
                          (substitute (make-F var) dbound dty)))
              new-t-arr (->Function (concat (:dom T) new-tys) (:rng T) nil nil nil)
              new-cset (cs-gen-Function V 
                                        ;move dotted lower/upper bounds to vars
                                        (merge X (zipmap vars (repeat (Y dbound)))) Y S new-t-arr)]
          (move-vars-to-dmap new-cset dbound vars)))

      ;; * <: ...
      (and (:rest S)
           (:drest T))
      (let [{t-dty :pre-type dbound :name} (-> T :drest)]
        (when-not (Y dbound)
          (type-error S T))
        (if (<= (count (:dom S)) (count (:dom T)))
          ;; the simple case
          (let [arg-mapping (cs-gen-list V X Y (:dom T) (concat (:dom S) (repeat (- (count (:dom T)) (count (:dom S))) (:rest S))))
                darg-mapping (move-rest-to-dmap (cs-gen V (merge X {dbound (Y dbound)}) Y t-dty (:rest S)) dbound)
                ret-mapping (cg (:rng S) (:rng T))]
            (cset-meet* [arg-mapping darg-mapping ret-mapping]))
          ;; the hard case
          (let [vars (var-store-take dbound t-dty (- (count (:dom S)) (count (:dom T))))
                new-tys (doall (for [var vars]
                                 (substitute (make-F var) dbound t-dty)))
                new-t-arr (->Function (concat (:dom T) new-tys) (:rng T) nil (->DottedPretype t-dty dbound) nil)
                new-cset (cs-gen-Function V (merge X (zipmap vars (repeat (Y dbound))) X) Y S new-t-arr)]
            (move-vars+rest-to-dmap new-cset dbound vars))))

:else 
(throw (IllegalArgumentException. (pr-str "NYI Function inference " (unparse-type S) (unparse-type T)))))))

(defmethod cs-gen* [Function Function ::default]
  [V X Y S T]
  #_(prn "cs-gen* [Function Function]")
  (cs-gen-Function V X Y S T))

(declare error-msg)

;; C : cset? - set of constraints found by the inference engine
;; Y : (setof symbol?) - index variables that must have entries
;; R : Type? - result type into which we will be substituting
(defn subst-gen [C Y R]
  {:pre [(cset? C)
         ((set-c? symbol?) Y)
         (AnyType? R)]
   :post [((some-fn nil? substitution-c?) %)]}
  (let [var-hash (fv-variances R)
        idx-hash (idx-variances R)]
    (letfn [
            ;; v : Symbol - variable for which to check variance
            ;; h : (Hash F Variance) - hash to check variance in (either var or idx hash)
            ;; variable: Symbol - variable to use instead, if v was a temp var for idx extension
            (constraint->type [{{:keys [upper-bound lower-bound higher-kind]} :bnds :keys [S X T] :as v} h & {:keys [variable]}]
              {:pre [(c? v)
                     (variance-map? h)
                     ((some-fn nil? symbol?) variable)]}
              (assert (subtype? S T) (type-error S T))
              (assert (not higher-kind) "NYI")
              (let [var (h (or variable X) :constant)
                    inferred (case var
                               (:constant :covariant) S
                               :contravariant T
                               :invariant S)]
                inferred))
            ;TODO implement generalize
            ;                  (let [gS (generalize S)]
            ;                    (if (subtype? gS T)
            ;                      gS
            ;                      S))

            ;; Since we don't add entries to the empty cset for index variables (since there is no
            ;; widest constraint, due to dcon-exacts), we must add substitutions here if no constraint
            ;; was found.  If we're at this point and had no other constraints, then adding the
            ;; equivalent of the constraint (dcon null (c Bot X Top)) is okay.
            (extend-idxs [S]
              {:pre [(substitution-c? S)]}
              (let [fi-R (fi R)] ;free indices in R
                ;; If the index variable v is not used in the type, then
                ;; we allow it to be replaced with the empty list of types;
                ;; otherwise we error, as we do not yet know what an appropriate
                ;; lower bound is.
                (letfn [(demote-check-free [v]
                          {:pre [(symbol? v)]}
                          (if (fi-R v)
                            (throw (Exception. "attempted to demote dotted variable"))
                            (->i-subst nil)))]
                  ;; absent-entries is false if there's an error in the substitution, otherwise
                  ;; it's a list of variables that don't appear in the substitution
                  (let [absent-entries
                        (reduce (fn [no-entry v]
                                  {:pre [(symbol? v)]}
                                  (let [entry (S v)]
                                    ;; Make sure we got a subst entry for an index var
                                    ;; (i.e. a list of types for the fixed portion
                                    ;;  and a type for the starred portion)
                                    (cond
                                      (false? no-entry) no-entry
                                      (not entry) (cons v no-entry)
                                      (or (i-subst? entry) 
                                          (i-subst-starred? entry)
                                          (i-subst-dotted? entry)) no-entry
                                      :else false)))
                                [] Y)]
                    (and absent-entries
                         (merge (into {}
                                      (for [missing absent-entries]
                                        (let [var (idx-hash missing :constant)]
                                          [missing
                                           (case var
                                             (:constant :covariant :invariant) (demote-check-free missing)
                                             :contravariant (->i-subst-starred nil (->Top)))])))
                                S))))))]

      (let [{cmap :fixed dmap* :dmap} (-> C :maps first)
            _ (assert (= 1 (count (:maps C))) "More than one constraint set found")
            dm (:map dmap*)
            subst (merge 
                    (into {}
                      (for [[k dc] dm]
                        (cond
                          (and (dcon? dc) (not (:rest dc)))
                          [k (->i-subst (doall
                                          (for [f (:fixed dc)]
                                            (constraint->type f idx-hash :variable k))))]
                          (and (dcon? dc) (:rest dc))
                          [k (->i-subst-starred (doall
                                                  (for [f (:fixed dc)]
                                                    (constraint->type f idx-hash :variable k)))
                                                (constraint->type (:rest dc) idx-hash))]
                          (dcon-exact? dc)
                          [k (->i-subst-starred (doall
                                                  (for [f (:fixed dc)]
                                                    (constraint->type f idx-hash :variable k)))
                                                (constraint->type (:rest dc) idx-hash))]
                          (dcon-dotted? dc)
                          [k (->i-subst-dotted (doall
                                                 (for [f (:fixed dc)]
                                                   (constraint->type f idx-hash :variable k)))
                                               (constraint->type (:dc dc) idx-hash :variable k)
                                               (:dbound dc))]
                          :else (throw (Exception. (prn-str "What is this? " dc))))))

                    (into {}
                      (for [[k v] cmap]
                        [k (->t-subst (constraint->type v var-hash)
                                      (:bnds v))])))
            ;check bounds
            _ (let [t-substs (into {} (filter (fn [[_ v]] (t-subst? v)) subst))
                    [names images] (let [s (seq t-substs)]
                                     [(map first s)
                                      (map (comp :type second) s)])]
                (doseq [[nme {inferred :type :keys [bnds]}] t-substs]
                  (assert (not (:higher-kind bnds)) "NYI")
                  (let [lower-bound (substitute-many (:lower-bound bnds) images names)
                        upper-bound (substitute-many (:upper-bound bnds) images names)]
                    (assert (subtype? lower-bound upper-bound)
                            (error-msg "Lower-bound " (unparse-type lower-bound)
                                       " is not below upper-bound " (unparse-type upper-bound)))
                    (assert (and (subtype? inferred upper-bound)
                                 (subtype? lower-bound inferred))
                            (error-msg "Inferred type " (unparse-type inferred)
                                       " is not between bounds " (unparse-type lower-bound)
                                       " and " (unparse-type upper-bound))))))]
        ;; verify that we got all the important variables
        (when-let [r (and (every? identity
                                  (for [v (fv R)]
                                    (let [entry (subst v)]
                                      (and entry (t-subst? entry)))))
                          (extend-idxs subst))]
          r)))))

;; V : a set of variables not to mention in the constraints
;; X : the set of type variables to be constrained
;; Y : the set of index variables to be constrained
;; S : a list of types to be the subtypes of T
;; T : a list of types
;; expected-cset : a cset representing the expected type, to meet early and
;;  keep the number of constraints in check. (empty by default)
;; produces a cset which determines a substitution that makes the Ss subtypes of the Ts
(defn cs-gen-list [V X Y S T & {:keys [expected-cset] :or {expected-cset (empty-cset {} {})}}]
  {:pre [((set-c? symbol?) V)
         (every? (hash-c? symbol? Bounds?) [X Y])
         (every? Type? (concat S T))
         (cset? expected-cset)]
   :post [(cset? %)]}
;  (prn "cs-gen-list" 
;       V X Y
;       (map unparse-type S)
;       (map unparse-type T))
  (assert (= (count S) (count T))
          (pr-str "S:" (map unparse-type S)
                  "T:" (map unparse-type T)))
  (cset-meet*
    ;; We meet early to prune the csets to a reasonable size.
    ;; This weakens the inference a bit, but sometimes avoids
    ;; constraint explosion.
    (cons
      (empty-cset X Y)
      (doall 
        (for [[s t] (map vector S T)]
          (let [c (cs-gen V X Y s t)]
;            (prn "s" s)
;            (prn "t" t)
;            (prn "c" c)
;            (prn "expected cset" expected-cset)
            (cset-meet c expected-cset)))))))
