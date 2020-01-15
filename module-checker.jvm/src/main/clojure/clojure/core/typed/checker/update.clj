;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.update
  (:refer-clojure :exclude [update])
  (:require [clojure.core.typed.checker.filter-rep :as fl]
            [clojure.core.typed.checker.path-rep :as pe]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.checker.filter-ops :as fo]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.core.typed.checker.free-ops :as free-ops]
            [clojure.core.typed.checker.cs-gen :as cgen]
            [clojure.core.typed.checker.cs-rep :as crep]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.path-rep :as pr]
            [clojure.core.typed.checker.lex-env :as lex]
            [clojure.core.typed.checker.jvm.subtype :as sub]
            [clojure.core.typed.checker.remove :as remove]
            [clojure.set :as set])
  (:import (clojure.lang IPersistentMap Keyword)))

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
         (boolean? @flag)]
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
                  new-or (if (some (fn [f] (fo/implied-atomic? p f))
                                   (disj (set/union (set worklist) (set derived-props)) 
                                         p))
                           fl/-top
                           (loop [ps ps
                                  result []]
                             (cond
                               (empty? ps) (apply fo/-or result)
                               (some (fn [other-p] (fo/opposite? (first ps) other-p))
                                     (concat derived-props derived-atoms))
                               (recur (next ps) result)
                               (some (fn [other-p] (fo/implied-atomic? (first ps) other-p))
                                     derived-atoms)
                               fl/-top
                               :else (recur (next ps) (cons (first ps) result)))))]
              (if (fl/OrFilter? new-or)
                (recur (cons new-or derived-props) derived-atoms (next worklist))
                (recur derived-props derived-atoms (cons new-or (next worklist)))))
            (and (fl/TypeFilter? p)
                 (= (c/Un) (:type p)))
            (do 
              ;(prn "Variable set to bottom:" p)
              (reset! flag false)
              [derived-props derived-atoms])
            (fl/TypeFilter? p) (recur derived-props (cons p derived-atoms) (next worklist))
            (and (fl/NotTypeFilter? p)
                 (= r/-any (:type p)))
            (do 
              ;(prn "Variable set to bottom:" p)
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
; - if false, we're updating with a NotTypeFilter so we use remove
; lo is a sequence of path elements, in the same order as -> (left to right)
;[Type Type Boolean PathElems -> Type]
(defn update* [t ft pos? lo]
  {:pre [(r/Type? t)
         (r/Type? ft)
         (boolean? pos?)
         (pr/path-elems? lo)]
   :post [(r/Type? %)]}
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
          (or absent?
              (and (c/complete-hmap? t)
                   (not present?)
                   (not optional?)))
            ; if an absent key is not nil, we have a contradiction
            (if (r/Bottom? (update-inner r/-nil))
              (r/Bottom)
              t)


          ; key is either unspoken for or :optional
          :else
          (let [; KeyPE are only used for `get` operations where `nil` is the
                ; not-found value. If the filter does not hold when updating
                ; it to nil, then we can assume this key path is present.
                update-to-mandatory? (r/Bottom? (update-inner r/-nil))
                old-type (or ((:optional t) fpth) r/-any)]
            (if update-to-mandatory?
              (c/make-HMap 
                :mandatory (assoc-in (:types t) [fpth] (update-inner old-type))
                :optional (dissoc (:optional t) fpth)
                :absent-keys (:absent-keys t)
                :complete? (c/complete-hmap? t))
              (c/make-HMap 
                :mandatory (:types t)
                :optional (assoc-in (:optional t) [fpth] (update-inner old-type))
                :absent-keys (:absent-keys t)
                :complete? (c/complete-hmap? t))))))

      ; nil returns nil on keyword lookups
      (and (not pos?)
           (pe/KeyPE? (first lo))
           (sub/subtype? t r/-nil))
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
           (r/HSequential? t))
      (let [type ft
            path-expr (first lo)
            idx (:idx path-expr)
            fixed-types (conj (vec (repeat idx r/-any)) type)
            restriction-type (r/-hsequential fixed-types :rest r/-any
                                             :kind (:kind t))]
        (c/restrict t restriction-type))

      (and (not pos?)
           (pe/NthPE? (first lo))
           (r/HSequential? t))
      t

      ; Update class information based on a call to `class`
      ; eg. (= java.lang.Integer (class a))
      (pe/ClassPE? (first lo))
      (let [u ft]
        (cond 
          ;restrict the obvious case where the path is the same as a Class Value
          (and pos?
               (r/Value? u)
               (class? (:val u)))
          (update* t (c/RClass-of-with-unknown-params (:val u)) true (next lo))

          ; For this case to be sound, we need to prove there doesn't exist a subclass
          ; of (:val u). Finding subclasses is difficult on the JVM, even after verifying
          ; if a class is final.
          ;(and (not pos?)
          ;     (r/Value? u)
          ;     (class? (:val u)))
          ;(update* t (c/RClass-of-with-unknown-params (:val u)) true (next lo))

          (and pos?
               (sub/subtype? u (c/RClass-of Object)))
          (update* t (c/RClass-of Object) true (next lo))

          (and pos?
               (sub/subtype? u r/-nil))
          (update* t r/-nil true (next lo))

          ;flip polarity in recursive calls
          (and (not pos?)
               (sub/subtype? (c/RClass-of Object) u))
          (update* t r/-nil true (next lo))

          (and (not pos?)
               (sub/subtype? r/-nil u))
          (update* t (c/RClass-of Object) true (next lo))

          ;; t = Any
          :else t))

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
            ]
        (if-not subst
          t
          (let [
            element-t-subst (get subst x)
            _ (assert (crep/t-subst? element-t-subst))
            ; the updated 'keys/vals' type
            element-t (:type element-t-subst)
            ;_ (prn "element-t" (prs/unparse-type element-t))
            _ (assert element-t)]
        ;; FIXME this is easy to implement, just recur update* on rstpth instead of nil.
        ;; should also add a test.
        (assert (empty? rstpth) (str "Further path NYI keys/vals"))
        (if pos?
          (update* t
                   (if (pe/KeysPE? fstpth)
                     (c/RClass-of IPersistentMap [element-t r/-any])
                     (c/RClass-of IPersistentMap [r/-any element-t]))
                   pos? nil)
          ; can we do anything for a NotTypeFilter?
          t))))

      (pe/KeywordPE? (first lo))
      ;; t is the old type, eg. (Val "my-key"). Can also be any type.
      ;; ft is the new type, eg. (Val :my-key). Can also be in (U nil Kw).
      ;;
      ;; eg. (update* (Val "my-key") (Val :my-key) true [KeywordPE])
      ;; Here we have 
      (update* t
               (cond
                 ;; Take the new type and un-keywordify it, then use that to update the old type.
                 (r/Value? ft) (let [{:keys [val]} ft]
                                 (cond
                                   (keyword? val) (let [kstr (str (when (namespace val)
                                                                    (str (namespace val) "/"))
                                                                  (name val))]
                                                    (c/Un (r/-val kstr)
                                                          (r/-val (symbol kstr))
                                                          (r/-val val)))
                                   (nil? val) r/-any
                                   ;; impossible
                                   :else r/-nothing))

                 ;; if the new type is a keyword, old type must be a (U Str Sym Kw).
                 (sub/subtype? ft (c/RClass-of Keyword))
                 (c/Un (c/RClass-of Keyword) 
                       (c/RClass-of String) 
                       (c/RClass-of clojure.lang.Symbol))

                 ;; if the output of `keyword` is at best (U nil Kw), input could be anything
                 (sub/subtype? ft (c/Un r/-nil (c/RClass-of Keyword)))
                 r/-any

                 ;; impossible
                 :else r/-nothing)
               pos? (next lo))

      :else (err/int-error (str "update along ill-typed path " (pr-str (prs/unparse-type t)) " " (mapv prs/unparse-path-elem lo))))))

(defn update [t lo]
  {:pre [((some-fn fl/TypeFilter? fl/NotTypeFilter?) lo)]
   :post [(r/Type? %)]}
  (update* t (:type lo) (fl/TypeFilter? lo) (fl/filter-path lo)))

;; sets the flag box to #f if anything becomes (U)
;[PropEnv (Seqable Filter) (Atom Boolean) -> PropEnv]
(defn env+ [env fs flag]
  {:pre [(lex/PropEnv? env)
         (every? (every-pred fl/Filter? (complement fl/NoFilter?)) 
                 fs)
         (boolean? @flag)]
   :post [(lex/PropEnv? %)
          ; flag should be updated by the time this function exits
          (boolean? @flag)]}
  (let [[props atoms] (combine-props fs (:props env) flag)]
    (reduce (fn [env f]
              ;post-condition checked in env+
              {:pre [(lex/PropEnv? env)
                     (fl/Filter? f)]}
              (cond
                (fl/BotFilter? f) (do ;(prn "Bot filter found in env+")
                                      (reset! flag false)
                                      (update-in env [:l] (fn [l] 
                                                            (zipmap (keys l)
                                                                    (repeat r/-nothing)))))
                ((some-fn fl/TypeFilter? fl/NotTypeFilter?) f)
                (let [;_ (prn "Update filter" f)
                      new-env (update-in env [:l (:id f)]
                                         (fn [t]
                                           (when-not t
                                             (err/int-error (str "Updating local not in scope: " (:id f)
                                                                 " " (keys (get env :l)))))
                                           (update t f)))]
                  ; update flag if a variable is now bottom
                  (when-let [bs (seq (filter (comp #{(c/Un)} val) (:l new-env)))]
                    ;(prn "Variables now bottom:" (keys bs))
                    (reset! flag false))
                  new-env)

                (and (fl/OrFilter? f)
                     (every? (some-fn fl/TypeFilter? fl/NotTypeFilter?) (:fs f))
                     (apply = (map fl/filter-id (:fs f))))
                (let [id (-> f :fs first fl/filter-id)
                      _ (assert (symbol? id))
                      new-env (update-in env [:l id]
                                         (fn [t]
                                           (when-not t
                                             (err/int-error (str "Updating local not in scope: " (:id f))))
                                           (apply c/Un
                                                  (map (fn [f] (update t f)) 
                                                       (:fs f)))))]
                  ; update flag if a variable is now bottom
                  (when-let [bs (seq (filter (comp #{(c/Un)} val) (:l new-env)))]
                    ;(prn "Variables now bottom:" (keys bs))
                    (reset! flag false))
                  new-env)
                :else env))
            (assoc env :props (set (concat atoms props)))
            (concat atoms props))))
