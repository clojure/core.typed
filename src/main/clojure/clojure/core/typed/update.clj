(ns clojure.core.typed.update
  (:refer-clojure :exclude [update defn])
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
            [clojure.core.typed.profiling :as p :refer [defn]]
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

; This is where filters are applied to existing types to generate more specific ones.
; t is the old type
; ft is the new type to update with
; pos? indicates polarity
; - if true, we're updating with a TypeFilter so we use restrict
; - if false, we're updateing with a NotTypeFilter so we use remove
; lo is a sequence of path elements, in the same order as -> (left to right)
;[Type Type Boolean PathElems -> Type]
(defn update* [t ft pos? lo]
  {:pre [(r/Type? t)
         (r/Type? ft)
         (con/boolean? pos?)
         (fl/path-elems? lo)]
   :post [(r/Type? %)]}
  (u/p :check/update
  (let [t (c/fully-resolve-type t)]
    (cond
      ; The easy cases: we have a filter without a further path to travel down.
      ; Just update t with the correct polarity.

      (empty? lo)
      (if pos?
        (c/restrict t ft)
        (remove/remove* t ft))

      ; unwrap unions and intersections to update their members

      (or (r/Union? t)        
          (r/Intersection? t)) 
      (apply (if (r/Union? t) c/Un c/In)
             (map #(update* % ft pos? lo) (:types t)))

      ;from here, t is fully resolved and is not a Union or Intersection

      ;heterogeneous map ops
      ; Positive and negative information down a keyword path
      ; eg. (number? (-> hmap :a :b))
      (and (pe/KeyPE? (first lo))
           (r/HeterogeneousMap? t))
      (let [polarity pos?
            update-to-type ft
            path lo
            [fkeype & rstpth] path
            fpth (cu/KeyPE->Type fkeype)
            update-inner (fn 
                           ([old] (update* old ft pos? rstpth))
                           ([old new] (update* old new pos? rstpth)))
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
              :mandatory (update-in (:types t) [fpth] update-inner)
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
                update-to-mandatory? (r/Bottom? (update-inner r/-nil))]
            (if update-to-mandatory?
              (c/make-HMap 
                :mandatory (assoc-in (:types t) [fpth] (update-inner r/-any))
                :optional (:optional t)
                :absent-keys (:absent-keys t)
                :complete? (c/complete-hmap? t))
              (c/make-HMap 
                :mandatory (:types t)
                :optional (if optional?
                            (update-in (:optional t) [fpth] update-inner)
                            (assoc-in (:optional t) [fpth] (update-inner r/-any)))
                :absent-keys (:absent-keys t)
                :complete? (c/complete-hmap? t))))))

      ; nil returns nil on keyword lookups
      (and (not pos?)
           (pe/KeyPE? (first lo))
           (r/Nil? t))
      (update* r/-nil ft pos? (next lo))

      ; update count information based on a call to `count`
      ; eg. (= 1 (count a))
      (and pos?
           (pe/CountPE? (first lo)))
      (let [u ft]
        (if-let [cnt (cond 
                       ; for (= 1 (count v))
                       (and (r/Value? u) (integer? (:val u)))
                       (r/make-ExactCountRange (:val u))

                       ; for (#{1 2 3} (count v))
                       (and (r/Union? u) 
                            (every? (every-pred r/Value?
                                                (comp integer? :val))
                                    (:types u)))
                       (let [ns (->> (map :val (:types u))
                                     (remove neg?)
                                     sort
                                     vec)]
                         (when (seq ns)
                           (r/make-CountRange (first ns)
                                              (last ns)))))]
          (c/restrict t cnt)
          (do (u/tc-warning "Cannot infer Count from type " (prs/unparse-type u))
              t)))

      ;can't do much without a NotCountRange type or difference type
      (and (not pos?)
           (pe/CountPE? (first lo)))
      t

      (and pos?
           (pe/NthPE? (first lo))
           (c/AnyHSequential? t))
      (let [type ft
            path-expr (first lo)
            idx (:idx path-expr)
            fixed-types (conj (vec (repeat idx r/-any)) type)
            restriction-type (r/-hsequential fixed-types :rest r/-any)]
        (c/restrict t restriction-type))

      (and (not pos?)
           (pe/NthPE? (first lo))
           (c/AnyHSequential? t))
      t

      ; Update class information based on a call to `class`
      ; eg. (= java.lang.Integer (class a))
      (and pos?
           (pe/ClassPE? (first lo)))
      (let [_ (assert (not (next lo)))
            u ft]
        (cond 
          ;restrict the obvious case where the path is the same as a Class Value
          ; eg. #(= (class %) Number)
          (and (r/Value? u)
               (class? (:val u)))
          (update* t (c/RClass-of-with-unknown-params (:val u)) pos? nil)

          ; handle (class nil) => nil
          (r/Nil? u)
          (update* t r/-nil pos? nil)

          :else
          (do (u/tc-warning "Cannot infer type via ClassPE from type " (prs/unparse-type u))
              t)))

      ; Does not tell us anything.
      ; eg. (= Number (class x)) ;=> false
      ;     does not reveal whether x is a subtype of Number, eg. (= Integer (class %))
      (and (not pos?)
           (pe/ClassPE? (first lo)))
      t

      ; keyword invoke of non-hmaps
      ; (let [a (ann-form {} (Map Any Any))]
      ;   (number? (-> a :a :b)))
      ; 
      ; I don't think there's anything interesting worth encoding:
      ; use HMap for accurate updating.
      (pe/KeyPE? (first lo))
      t

      ; calls to `keys` and `vals`
      ((some-fn pe/KeysPE? pe/ValsPE?) (first lo))
      (let [[fstpth & rstpth] lo
            u ft
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
        (if pos?
          (update* t
                   (if (pe/KeysPE? fstpth)
                     (c/RClass-of IPersistentMap [element-t r/-any])
                     (c/RClass-of IPersistentMap [r/-any element-t]))
                   pos? nil)
          ; can we do anything for a NotTypeFilter?
          t))


      :else (err/int-error (str "update along ill-typed path " (pr-str (prs/unparse-type t)) " " (with-out-str (pr lo))))))))

(defn update [t lo]
  {:pre [((some-fn fl/TypeFilter? fl/NotTypeFilter?) lo)]
   :post [(r/Type? %)]}
  (update* t (:type lo) (fl/TypeFilter? lo) (fl/filter-path lo)))

;; sets the flag box to #f if anything becomes (U)
;[PropEnv (Seqable Filter) (Atom Boolean) -> PropEnv]
(defn env+ [env fs flag]
  {:pre [(lex/PropEnv? env)
         (every? fl/Filter? fs)
         (con/boolean? @flag)]
   :post [(lex/PropEnv? %)
          ; flag should be updated by the time this function exits
          (con/boolean? @flag)]}
  (let [[props atoms] (combine-props fs (:props env) flag)]
    (reduce (fn [env f]
              ;post-condition checked in env+
              {:pre [(lex/PropEnv? env)
                     (fl/Filter? f)]}
              (cond
                (fl/BotFilter? f) (do (reset! flag false)
                                      (update-in env [:l] (fn [l] 
                                                            (zipmap (keys l)
                                                                    (repeat r/-nothing)))))
                ((some-fn fl/TypeFilter? fl/NotTypeFilter?) f)
                (let [new-env (update-in env [:l (:id f)]
                                         (fn [t]
                                           (when-not t
                                             (err/int-error (str "Updating local not in scope: " (:id f))))
                                           (update t f)))]
                  ; update flag if a variable is now bottom
                  (when-let [bs (some #{(c/Un)} (vals (:l new-env)))]
                    (reset! flag false))
                  new-env)
                :else env))
            (assoc env :props (set (concat atoms props)))
            (concat atoms props))))
