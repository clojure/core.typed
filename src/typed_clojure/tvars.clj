
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable Elimination

(defmulti replace-variables 
  "In type t, replace all occurrences of x with v"
  (fn [t x->v]
    (assert (every? type-variable? (keys x->v)) x->v)
    (assert (every? Type? (vals x->v)) x->v)
    (class t)))

(defn- arity-introduces-shadow? [t x]
  (some #(= % x) (:type-params t)))

(defn- unique-variable
  "Generate a globally unique type variable based on t"
  [t]
  (assert (type-variable? t))
  (-> t
    (update-in [:nme] #(-> % name gensym))))

(defn- rename-shadowing-variable 
  "Renames a type parameter provided by arity t, from variable x to v.
  Takes function f that takes 1 argument providing the replacement function"
  [t x v f]
  (f #(replace-variables % {x (unique-variable x)})))

(defn- handle-replace-arity 
  [t x->v rename-arity replace-free]
  ;; handle inner scopes, generate unique names for
  ;; variables with same name but different scope
  (loop [t t
         x->v x->v]
    (let [[x v] (first x->v)]
      (if (seq x->v)
        (recur
          (cond
            (arity-introduces-shadow? t x)
            (rename-arity t x v)

            :else
            (replace-free t x v))
          (next x->v))
        t))))

(defmethod replace-variables arity
  [t x->v]
  (letfn [(rename-fixed-arity-shadow [t x v]
            (rename-shadowing-variable t x v
                                       (fn [rplc]
                                         (-> t
                                           (update-in [:dom] #(doall (map rplc %)))
                                           (update-in [:rest-type] #(when % (rplc %)))
                                           (update-in [:rng] rplc) 
                                           (update-in [:type-params] #(doall (map rplc %)))))))
          
          (replace-fixed-arity-free-variable [t x v]
            (let [rplc #(replace-variables % {x v})]
              (-> t
                (update-in [:dom] #(doall (map rplc %)))
                (update-in [:rest-type] #(when % (rplc %)))
                (update-in [:rng] rplc))))]
    (handle-replace-arity
      t
      x->v
      rename-fixed-arity-shadow
      replace-fixed-arity-free-variable)))

(defmethod replace-variables Fun
  [t x->v]
  (let [rplc #(replace-variables % x->v)]
    (-> t
      (update-in [:arities] #(doall (map rplc %))))))

(defmethod replace-variables UnboundedTypeVariable
  [t x->v]
  (if-let [v (x->v t)]
    v
    t))

(defmethod replace-variables :default
  [t x->v]
  t)

;; Local Type Inference (2000) Pierce & Turner, Section 3.2

(defmulti promote 
  "Return the least supertype of s that does not reference any type variables
  in the set v"
  (fn [s v] (class s)))

(defmulti demote 
  "Return the greatest subtype of s that does not reference any type variables
  in the set v"
  (fn [s v] (class s)))

(defmethod promote Type
  [s v]
  (cond
    (Any? s) Any
    (Nothing? s) Nothing
    :else s))

(defmethod promote UnboundedTypeVariable
  [s v]
  (if (v s)
    Any
    s))

(defn- rename-type-args 
  "Rename any type parameters conflicting with type variables in set v"
  [s v]
  (assert (and (arity? s)
               (not (:rest-type s))))
  (let [renames (into {}
                      (map #(vector % (unique-variable %))
                           (filter v (:type-params s))))]
    (if (seq renames)
      ; rename shadowing variables
      (let [rplc #(replace-variables % renames)]
        (-> s
          (update-in [:dom] #(doall (map rplc %)))
          (update-in [:rng] rplc)
          (update-in [:rest-type] #(when % (rplc %)))
          (update-in [:type-params] #(doall (map rplc %)))))
      s)))

(defmethod promote arity
  [s v]
  (let [s (rename-type-args s v)
        dmt #(demote % v)
        pmt #(promote % v)]
    (-> s
      (update-in [:dom] #(doall (map dmt %)))
      (update-in [:rest-type] #(when % (dmt %)))
      (update-in [:rng] pmt))))

(defmethod promote Fun
  [s v]
  (let [pmt #(promote % v)]
    (-> s
      (update-in [:arities] #(doall (map pmt %))))))

(defmethod demote Type
  [s v]
  (cond
    (Any? s) Any
    (Nothing? s) Nothing
    :else s))

(defmethod demote UnboundedTypeVariable
  [s v]
  (if (v s)
    Nothing
    s))

(defmethod demote arity
  [s v]
  (let [s (rename-type-args s v)
        pmt #(promote % v)
        dmt #(demote % v)]
    (-> s
      (update-in [:dom] #(doall (map pmt %)))
      (update-in [:rest-type] #(when % (pmt %)))
      (update-in [:rng] dmt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint generation

;; Local Type Inference (2000) Pierce & Turner, Section 3.3

(defconstrainedrecord TypeVariableConstraint [type-var upper-bound lower-bound]
  "A constraint on a type variable type-var. Records an upper and lower bound"
  {:pre [(type-variable? type-var)
         (Type? upper-bound)
         (Type? lower-bound)]})

(defconstrainedrecord ConstraintSet [constraints]
  "A constraint set, each constraint for a different variable"
  {:pre [(every? TypeVariableConstraint? constraints)
         (let [no-duplicates? (fn [{:keys [type-var]}]
                                (= 1
                                   (count 
                                     (filter #(= (:type-var %) type-var)
                                             constraints))))]
           (every? no-duplicates? constraints))]})

(defn intersect-constraint-sets
  "Returns the intersection of constraint sets cs"
  [& cs]
  (let [merged-constraints (for [[t constraints] (group-by :type-var (mapcat :constraints cs))]
                             (map->TypeVariableConstraint
                               {:type-var t
                                :lower-bound (union (map :lower-bound constraints))
                                :upper-bound (map->Intersection
                                               {:types (map :upper-bound constraints)})}))]
    (map->ConstraintSet
      {:constraints merged-constraints})))

(defn trivial-constraint 
  "Return the trivial constraint for variable x"
  [x]
  (map->TypeVariableConstraint
    {:type-var x
     :lower-bound Nothing
     :upper-bound Any}))

(defn empty-constraint-set
  "Returns the empty constraint set for variables in xs"
  [xs]
  (map->ConstraintSet
    {:constraints (map trivial-constraint xs)}))

(defn singleton-constraint-set
  "Returns the singleton constraint set, containing the provided
  constraint, with the trivial constraint for each variable in set xs"
  [constraint xs]
  (map->ConstraintSet
    {:constraints (cons constraint (map trivial-constraint xs))}))

(declare constraint-gen*)

(defn constraint-gen [s t xs v]
  (let [conflicts (set/intersection xs v)
        renames (into {}
                      (for [n conflicts]
                        [n (unique-variable n)]))

        ;; enforce (set/intersection xs v) => #{}
        [s t] (map #(replace-variables % renames) [s t])
        xs (set (replace renames xs))]
    (constraint-gen* s t xs v)))

(defmulti constraint-gen*
  "Given a set of type variables v, a set of unknowns xs, and
  two types s and t, calculate the minimal (ie. least contraining)
  xs/v constraint set C that guarantees s <: t"
  (fn [s t xs v]
    (assert (set? xs))
    [(class s) (class t)]))

(defn- eliminate-variables 
  "Eliminate all variables in set v that occur in type t
  by renaming them. Respects inner scopes, renaming accordingly"
  [t v]
  (let [subst (into {}
                    (for [tv v]
                      [tv (unique-variable tv)]))]
    (replace-variables t subst)))

(defn cg-upper [y s xs v]
  (let [s (eliminate-variables s xs)
        t (demote s v)]
    (singleton-constraint-set
      (map->TypeVariableConstraint
        {:type-var y
         :lower-bound Nothing
         :upper-bound t})
      (disj xs y))))

(defn cg-lower [s y xs v]
  (let [s (eliminate-variables s xs)
        t (promote s v)]
    (singleton-constraint-set
      (map->TypeVariableConstraint
        {:type-var y
         :lower-bound t
         :upper-bound Any})
      (disj xs y))))

(defmethod constraint-gen* [Type Type]
  [s t xs v]
  (cond
    (xs s) (cg-upper s t xs v)
    (xs t) (cg-lower s t xs v)
    ;; cg-refl
    :else (empty-constraint-set xs)))

(defmethod constraint-gen* [arity arity]
  [s t xs v]
  (let [;; enforce (set/intersection (:type-params s)
        ;;                           (set/union xs v))
        ;;         => #{}
        conflicts (set/intersection (set (:type-params s))
                                    (set/union xs v))

        renames (into {}
                      (for [v conflicts]
                        [v (unique-variable v)]))
        
        [s t] (map #(replace-variables % renames)
                   [s t])

        v-union-ys (set/union v (:type-params s))

        cs (map #(constraint-gen %1 %2 xs v-union-ys)
                 (concat (:dom t) (when (:rest-type t) [(:rest-type t)]))
                 (concat (:dom s) (when (:rest-type s) [(:rest-type s)])))
        
        d (constraint-gen (:rng s) (:rng t) xs v-union-ys)]
    (apply intersect-constraint-sets d cs)))

(defmethod constraint-gen* [Fun Fun]
  [s t xs v]
  (->>
    (for [s-arity (:arities s)]
      (constraint-gen
        s-arity
        (match-to-fun-arity s-arity t)
        xs 
        v))
    (apply intersect-constraint-sets)))

(defn minimal-substitution [r constraint-set]
  (into {}
        (loop
          [min-sub {}
           cs (:constraints constraint-set)]
          (if (empty? cs)
            min-sub
            (let [{x :type-var
                   s :lower-bound
                   t :upper-bound} (first cs)
                  sub-entry (cond
                              ;; r is constant or covariant in x
                              (or (subtype? (replace-variables r {x Nothing})
                                            (replace-variables r {x Any}))
                                  (subtype? (replace-variables r {x s})
                                            (replace-variables r {x t})))
                              [x s]

                              ;; r is contravariant in x
                              (subtype? (replace-variables r {x t})
                                        (replace-variables r {x s}))
                              [x t]

                              ;; r is invariant in x and (= s t)
                              (and (= s t)
                                   (subtype? (replace-variables r {x s})
                                             (replace-variables r {x t})))
                              [x s]

                              :else (throw (Exception. "No substitution exists to satisfy type")))]
              (recur 
                (into min-sub
                      [sub-entry])
                (next cs)))))))

  
