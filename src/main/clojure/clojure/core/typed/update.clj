(ns clojure.core.typed.update
  (:require [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.path-rep :as pe]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.cs-gen :as cgen]
            [clojure.core.typed.cs-rep :as crep]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.remove :as remove])
  (:import (clojure.lang IPersistentMap)))

;[(Seqable Filter) Filter -> Filter]
(defn resolve* [atoms prop]
  {:pre [(every? fl/Filter? atoms)
         (fl/Filter? prop)]
   :post [(fl/Filter? %)]}
  (reduce (fn [prop a]
            (cond
              (fl/AndFilter? a)
              (loop [ps (:fs a)
                     result []]
                (if (empty? ps)
                  (apply fo/-and result)
                  (let [p (first ps)]
                    (cond
                      (fo/opposite? a p) fl/-bot
                      (fo/implied-atomic? p a) (recur (next ps) result)
                      :else (recur (next ps) (cons p result))))))
              :else prop))
          prop
          atoms))

;[(Seqable Filter) -> (Seqable Filter)]
(defn flatten-props [ps]
  {:post [(every? fl/Filter? %)]}
  (loop [acc #{}
         ps ps]
    (cond
      (empty? ps) acc
      (fl/AndFilter? (first ps)) (recur acc (concat (-> ps first :fs) (next ps)))
      :else (recur (conj acc (first ps)) (next ps)))))

;[(Seqable Filter) (Seqable Filter) (Atom Boolean) 
;  -> '[(Seqable (U ImpFilter fl/OrFilter AndFilter))
;       (Seqable (U TypeFilter NotTypeFilter))]]
(defn combine-props [new-props old-props flag]
  {:pre [(every? fl/Filter? (concat new-props old-props))
         (instance? clojure.lang.Atom flag)
         (con/boolean? @flag)]
   :post [(let [[derived-props derived-atoms] %]
            (and (every? (some-fn fl/ImpFilter? fl/OrFilter? fl/AndFilter?) derived-props)
                 (every? (some-fn fl/TypeFilter? fl/NotTypeFilter?) derived-atoms)))]}
  (let [atomic-prop? (some-fn fl/TypeFilter? fl/NotTypeFilter?)
        {new-atoms true new-formulas false} (group-by (comp boolean atomic-prop?) (flatten-props new-props))]
    (loop [derived-props []
           derived-atoms new-atoms
           worklist (concat old-props new-formulas)]
      (if (empty? worklist)
        [derived-props derived-atoms]
        (let [p (first worklist)
              p (resolve* derived-atoms p)]
          (cond
            (fl/AndFilter? p) (recur derived-props derived-atoms (concat (:fs p) (next worklist)))
            (fl/ImpFilter? p) 
            (let [{:keys [a c]} p
                  implied? (some (fn [p] (fo/implied-atomic? a p)) (concat derived-props derived-atoms))]
              #_(prn "combining " (unparse-filter p) " with " (map unparse-filter (concat derived-props
                                                                                          derived-atoms))
                     " and implied:" implied?)
              (if implied?
                (recur derived-props derived-atoms (cons c (rest worklist)))
                (recur (cons p derived-props) derived-atoms (next worklist))))
            (fl/OrFilter? p)
            (let [ps (:fs p)
                  new-or (loop [ps ps
                                result []]
                           (cond
                             (empty? ps) (apply fo/-or result)
                             (some (fn [other-p] (fo/opposite? (first ps) other-p))
                                   (concat derived-props derived-atoms))
                             (recur (next ps) result)
                             (some (fn [other-p] (fo/implied-atomic? (first ps) other-p))
                                   derived-atoms)
                             fl/-top
                             :else (recur (next ps) (cons (first ps) result))))]
              (if (fl/OrFilter? new-or)
                (recur (cons new-or derived-props) derived-atoms (next worklist))
                (recur derived-props derived-atoms (cons new-or (next worklist)))))
            (and (fl/TypeFilter? p)
                 (= (c/Un) (:type p)))
            (do 
              ;(prn "Variable set to bottom:" (unparse-filter p))
              (reset! flag false)
              [derived-props derived-atoms])
            (fl/TypeFilter? p) (recur derived-props (cons p derived-atoms) (next worklist))
            (and (fl/NotTypeFilter? p)
                 (= r/-any (:type p)))
            (do 
              ;(prn "Variable set to bottom:" (unparse-filter p))
              (reset! flag false)
              [derived-props derived-atoms])
            (fl/NotTypeFilter? p) (recur derived-props (cons p derived-atoms) (next worklist))
            (fl/TopFilter? p) (recur derived-props derived-atoms (next worklist))
            (fl/BotFilter? p) (do 
                                ;(prn "Bot filter found")
                                (reset! flag false)
                                [derived-props derived-atoms])
            :else (recur (cons p derived-props) derived-atoms (next worklist))))))))

; This is where filters are applied to existing types to generate more specific ones
;[Type Filter -> Type]
(defn update [t lo]
  {:pre [((some-fn fl/TypeFilter? fl/NotTypeFilter?) lo)]
   :post [(r/Type? %)]}
  (u/p :check/update
  (let [t (c/fully-resolve-type t)]
    (cond
      ; The easy cases: we have a filter without a further path to travel down.
      ; Just update t with the correct polarity.

      (and (fl/TypeFilter? lo)
           (empty? (:path lo))) 
      (let [u (:type lo)
            _ (assert (r/Type? u))
            r (c/restrict u t)]
        r)

      (and (fl/NotTypeFilter? lo)
           (empty? (:path lo))) 
      (let [u (:type lo)]
        (assert (r/Type? u))
        (remove/remove* t u))

      ; unwrap unions and intersections to update their members

      (r/Union? t) (let [ts (:types t)
                       new-ts (mapv (fn [t] 
                                      (let [n (update t lo)]
                                        n))
                                    ts)]
                   (apply c/Un new-ts))
      (r/Intersection? t) (let [ts (:types t)]
                          (apply c/In (doall (map (fn [t] (update t lo)) ts))))

      ;from here, t is fully resolved and is not a Union or Intersection

      ;heterogeneous map ops
      ; Positive and negative information down a keyword path
      ; eg. (number? (-> hmap :a :b))
      (and ((some-fn fl/TypeFilter? fl/NotTypeFilter?) lo)
           (pe/KeyPE? (first (:path lo)))
           (r/HeterogeneousMap? t))
      (let [polarity (fl/TypeFilter? lo)
            {update-to-type :type :keys [path id]} lo
            [fkeype & rstpth] path
            fpth (cu/KeyPE->Type fkeype)
            ; use this filter to update the right hand side value
            next-filter ((if polarity fo/-filter fo/-not-filter) 
                         update-to-type id rstpth)
            present? (contains? (:types t) fpth)
            optional? (contains? (:optional t) fpth)
            absent? (contains? (:absent-keys t) fpth)]
        ;updating a KeyPE should consider 3 cases:
        ; 1. the key is declared present
        ; 2. the key is declared absent
        ; 3. the key is not declared present, and is not declared absent
        (cond
          present?
            ; -hmap simplifies to bottom if an entry is bottom
            (c/make-HMap
              :mandatory (update-in (:types t) [fpth] update next-filter)
              :optional (:optional t)
              :absent-keys (:absent-keys t)
              :complete? (c/complete-hmap? t))
          absent?
            t

          ; key not declared present or absent
          :else
          (let [; KeyPE are only used for `get` operations where `nil` is the
                ; not-found value. If the filter does not hold when updating
                ; it to nil, then we can assume this key path is present.
                update-to-mandatory? (r/Bottom? (update r/-nil next-filter))]
            (if update-to-mandatory?
              (c/make-HMap 
                :mandatory (assoc-in (:types t) [fpth] (update r/-any next-filter))
                :optional (:optional t)
                :absent-keys (:absent-keys t)
                :complete? (c/complete-hmap? t))
              (c/make-HMap 
                :mandatory (:types t)
                :optional (if optional?
                            (update-in (:optional t) [fpth] update next-filter)
                            (assoc-in (:optional t) [fpth] (update r/-any next-filter)))
                :absent-keys (:absent-keys t)
                :complete? (c/complete-hmap? t))))))

      ; nil returns nil on keyword lookups
      (and (fl/NotTypeFilter? lo)
           (pe/KeyPE? (first (:path lo)))
           (r/Nil? t))
      (update r/-nil (update-in lo [:path] rest))

      ; update count information based on a call to `count`
      ; eg. (= 1 (count a))
      (and (fl/TypeFilter? lo)
           (pe/CountPE? (first (:path lo))))
      (let [u (:type lo)]
        (if-let [cnt (when (and (r/Value? u) (integer? (:val u)))
                       (r/make-ExactCountRange (:val u)))]
          (c/restrict cnt t)
          (do (u/tc-warning "Cannot infer Count from type " (prs/unparse-type u))
              t)))

      ;can't do much without a NotCountRange type or difference type
      (and (fl/NotTypeFilter? lo)
           (pe/CountPE? (first (:path lo))))
      t

      (and (fl/TypeFilter? lo)
           (pe/FirstPE? (-> lo :path first))
           (c/AnyHSequential? t))
      (let [type (:type lo)
            restriction-type (r/-hsequential [type] :rest r/-any)]
        (c/restrict restriction-type t))

      (and (fl/NotTypeFilter? lo)
           (pe/FirstPE? (-> lo :path first))
           (c/AnyHSequential? t))
      t

      ; Update class information based on a call to `class`
      ; eg. (= java.lang.Integer (class a))
      (and (fl/TypeFilter? lo)
           (pe/ClassPE? (-> lo :path first)))
      (let [_ (assert (empty? (rest (:path lo))))
            u (:type lo)]
        (cond 
          ;restrict the obvious case where the path is the same as a Class Value
          ; eg. #(= (class %) Number)
          (and (r/Value? u)
               (class? (:val u)))
          (c/restrict (c/RClass-of-with-unknown-params (:val u)) t)

          ; handle (class nil) => nil
          (r/Nil? u)
          (c/restrict r/-nil t)

          :else
          (do (u/tc-warning "Cannot infer type via ClassPE from type " (prs/unparse-type u))
              t)))

      ; Does not tell us anything.
      ; eg. (= Number (class x)) ;=> false
      ;     does not reveal whether x is a subtype of Number, eg. (= Integer (class %))
      (and (fl/NotTypeFilter? lo)
           (pe/ClassPE? (-> lo :path first)))
      t

      ; keyword invoke of non-hmaps
      ; (let [a (ann-form {} (Map Any Any))]
      ;   (number? (-> a :a :b)))
      ; 
      ; I don't think there's anything interesting worth encoding:
      ; use HMap for accurate updating.
      (and (or (fl/TypeFilter? lo)
               (fl/NotTypeFilter? lo))
           (pe/KeyPE? (first (:path lo))))
      t

      ; calls to `keys` and `vals`
      (and ((some-fn fl/TypeFilter? fl/NotTypeFilter?) lo)
           ((some-fn pe/KeysPE? pe/ValsPE?) (first (:path lo))))
      (let [[fstpth & rstpth] (:path lo)
            u (:type lo)
            ;_ (prn "u" (prs/unparse-type u))

            ; solve for x:  t <: (Seqable x)
            x (gensym)
            subst (free-ops/with-bounded-frees {(r/make-F x) r/no-bounds}
                    (u/handle-cs-gen-failure
                      (cgen/infer {x r/no-bounds} {} 
                                  [u]
                                  [(c/RClass-of clojure.lang.Seqable [(r/make-F x)])]
                                  r/-any)))
            ;_ (prn "subst for Keys/Vals" subst)
            _ (when-not subst
                (err/int-error (str "Cannot update " (if (pe/KeysPE? fstpth) "keys" "vals") " of an "
                                  "IPersistentMap with type: " (pr-str (prs/unparse-type u)))))
            element-t-subst (get subst x)
            _ (assert (crep/t-subst? element-t-subst))
            ; the updated 'keys/vals' type
            element-t (:type element-t-subst)
            ;_ (prn "element-t" (prs/unparse-type element-t))
            _ (assert element-t)]
        (assert (empty? rstpth) (str "Further path NYI keys/vals"))
        (if (fl/TypeFilter? lo)
          (c/restrict (if (pe/KeysPE? fstpth)
                        (c/RClass-of IPersistentMap [element-t r/-any])
                        (c/RClass-of IPersistentMap [r/-any element-t]))
                      t)
          ; can we do anything for a NotTypeFilter?
          t))


      :else (err/int-error (str "update along ill-typed path " (pr-str (prs/unparse-type t)) " " (with-out-str (pr lo))))))))

; f can be a composite filter. bnd-env is a the :l of a PropEnv
; ie. a map of symbols to types
;[(t/Map t/Sym Type) Filter -> PropEnv]
(defn update-composite [bnd-env f]
  {:pre [(lex/lex-env? bnd-env)
         (fl/Filter? f)]
   :post [(lex/lex-env? %)]}
  #_(prn "update-composite" #_bnd-env #_f)
  (cond
    ; At this point, the OrFilter will be simplified. To update
    ; the types we need to make explicit the fact
    ; (| (! ... a) (! ... b))  is shorthand for
    ;
    ; (| (& (! ... a) (is ... b))
    ;    (& (is ... a) (! ... b))
    ;    (& (! ... a) (! ... b)))
    ;
    ;  then use the verbose representation to update the types.
;    ((some-fn fl/AndFilter? fl/OrFilter?) f)
;    (let [; normalise filters to a set of AndFilters, which are disjuncts
;          disjuncts (if (fl/AndFilter? f)
;                      #{f}
;                      (.fs ^OrFilter f))
;          _ (assert (not-any? fl/OrFilter? disjuncts)
;                    disjuncts)
;          ; each disjunct expands can be expanded to more filters
;          ; this is a list of the new, expanded disjuncts
;          expanded-disjucts (mapcat
;                              (fn [inner-f]
;                                (assert (not (fl/OrFilter? inner-f)) inner-f)
;                                (let [conjuncts (if (fl/AndFilter? inner-f)
;                                                  (.fs ^AndFilter inner-f)
;                                                  #{inner-f})]
;                                  (assert (every? fo/atomic-filter? conjuncts)
;                                          (pr-str inner-f))
;                                  (map (fn [positive-filters]
;                                         (let [negative-filters (map fo/negate (set/difference conjuncts positive-filters))
;                                               combination-filter (apply fo/-and (concat positive-filters negative-filters))]
;                                           combination-filter))
;                                       (map set (remove/remove empty? (comb/subsets conjuncts))))))
;                              disjuncts)
;          update-and (fn [init-env ^AndFilter and-f]
;                       {:pre [(fl/AndFilter? and-f)]}
;                       (reduce (fn [env a]
;                                 {:pre [(fo/atomic-filter? a)]}
;                                 ;eagerly merge
;                                 (merge-with c/In env (update-composite env a)))
;                               init-env (.fs ^AndFilter f)))]
;      ;update env with each disjunct. If variables change, capture both old and new types with Un.
;      ; first time around is special. At least 1 of disjuncts must be applied to the
;      ; environment, so we throw away the initial environment instead of merging it.
;      (let [first-time? (atom true)]
;        (reduce (fn [env fl]
;                  (let [updated-env (cond
;                                      (fl/AndFilter? fl) (update-and env fl)
;                                      (fo/atomic-filter? fl) (update-composite env fl)
;                                      :else (throw (Exception. "shouldn't get here")))]
;                    (if @first-time?
;                      (do (reset! first-time? false)
;                          updated-env)
;                      (merge-with c/Un env updated-env))))
;                bnd-env
;                expanded-disjucts)))

;    (fl/AndFilter? f)
;    (reduce (fn [env a]
;              #_(prn "And filter")
;              ;eagerly merge
;              (merge-with c/In env (update-composite env a)))
;            bnd-env (.fs ^AndFilter f))
;
;    (fl/OrFilter? f)
;    (let [fs (.fs ^OrFilter f)]
;      (reduce (fn [env positive-filters]
;                #_(prn "inside orfilter")
;                (let [negative-filters (map fo/negate (set/difference fs positive-filters))
;                      combination-filter (apply fo/-and (concat positive-filters negative-filters))]
;                  (merge-with c/Un env (update-composite env combination-filter))))
;              bnd-env 
;              (map set (remove/remove empty? (comb/subsets fs)))))
;
    (fl/BotFilter? f)
    (do ;(prn "update-composite: found bottom, unreachable")
      (zipmap (keys bnd-env) (repeat (c/Un))))

    (or (fl/TypeFilter? f)
        (fl/NotTypeFilter? f))
    (let [x (:id f)]
      (if-not (bnd-env x)
        bnd-env
        (update-in bnd-env [x] (fn [t]
                                 ;check if var is ever a target of a set!
                                 ; or not currently in scope
                                 (if (or (nil? t)
                                         (r/is-var-mutated? x))
                                   ; if it is, we do nothing
                                   t
                                   ;otherwise, refine the type
                                   (let [t (or t r/-any)
                                         new-t (update t f)]
                                     new-t))))))
    :else bnd-env))


;; sets the flag box to #f if anything becomes (U)
;[PropEnv (Seqable Filter) (Atom Boolean) -> PropEnv]
(defn env+ [env fs flag]
  {:pre [(lex/PropEnv? env)
         (every? fl/Filter? fs)
         (con/boolean? @flag)]
   :post [(lex/PropEnv? %)
          (con/boolean? @flag)]}
  #_(prn 'env+ fs)
  (let [[props atoms] (combine-props fs (:props env) flag)]
    (reduce (fn [env f]
              {:pre [(lex/PropEnv? env)
                     (fl/Filter? f)]}
              (let [new-env (update-in env [:l] update-composite f)]
                ; update flag if a variable is now bottom
                (when-let [bs (seq (filter (comp #{(c/Un)} val) (:l new-env)))]
                  ;(prn "variables are now bottom: " (map key bs))
                  (reset! flag false))
                new-env))
            (assoc env :props (set (concat atoms props)))
            (concat atoms props))))
;  (letfn [(update-env [env f]
;            {:pre [(lex/PropEnv? env)
;                   (fl/Filter? f)]}
;            (let [new-env (update-in env [:l] update-composite f)]
;              ; update flag if a variable is now bottom
;              (when-let [bs (seq (filter (comp #{(c/Un)} val) (:l new-env)))]
;                ;(prn "variables are now bottom: " (map key bs))
;                (reset! flag false))
;              new-env))]
;    (let [[props atoms] (combine-props fs (:props env) flag)
;          all-filters (apply fo/-and (concat props atoms))]
;      (-> env
;          (update-env all-filters)
;          (assoc :props (set (concat atoms props)))))))
