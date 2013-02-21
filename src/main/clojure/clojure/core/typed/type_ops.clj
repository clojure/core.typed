(in-ns 'clojure.core.logic)


;; FIXME much better algorithms around I'm sure
(defn countrange-overlap? 
  [{lowerl :lower upperl :upper :as l}
   {lowerr :lower upperr :upper :as r}]
  {:pre [(CountRange? l)
         (CountRange? r)]}
  (cond 
    (and upperl upperr)
        (or 
          ;; -----
          ;;   -------
          ;; and
          ;;   ---
          ;;   -------
          (<= lowerl lowerr upperl upperr)

          ;;    --
          ;;   -------
          (<= lowerr lowerl upperl upperr)

          ;;     ------
          ;; -------
          ;; and
          ;;     ---
          ;; -------
          (<= lowerr lowerl upperr upperl)

          ;; otherwise no overlap
          false)

    upperl ;; and (not upperr)
      (or 
        ;; ----
        ;;  ----->>
        ;; and
        ;;  ---
        ;;  ----->>
        (<= lowerl lowerr upperl)
        ;;   ---
        ;;  ----->>
        (<= lowerr lowerl)
        ;; otherwise no overlap
        false)
    upperr
      (or
        ;; ------>>
        ;;  ----
        ;; and
        ;;  ----->>
        ;;  ---
        (<= lowerl lowerr)
        
        ;;   --->>
        ;; ----
        (<= lowerr lowerl upperr)

        ;; else no overlap
        false)
    :else ;; (and (not upperl) (not upperr))
    ;; ---->>
    ;;   -->>
    ;; and
    ;;   -->>
    ;; ---->>
    true))

(defn fully-resolve-type 
  ([t seen]
   (let [_ (assert (not (seen t)) "Infinite non-Rec type detected")
         seen (conj seen t)]
     (if (requires-resolving? t)
       (fully-resolve-type (-resolve t) seen)
       t)))
  ([t] (fully-resolve-type t #{})))

;true if types t1 and t2 overlap (NYI)
(defn overlap [t1 t2]
  (let [t1 (fully-resolve-type t1)
        t2 (fully-resolve-type t2)
        eq (= t1 t2)
        hmap-and-seq? (fn [h s] (and (HeterogeneousMap? h)
                                     (RClass? s)
                                     (= (Class->symbol clojure.lang.ISeq) (:the-class s))))
        hvec-and-seq? (fn [h s] (and (HeterogeneousVector? h)
                                     (RClass? s)
                                     (= (Class->symbol clojure.lang.ISeq) (:the-class s))))]
    (cond 
      eq eq

      (and (Value? t1)
           (Value? t2))
      eq

      ;if both are Classes, and at least one isn't an interface, then they must be subtypes to have overlap
      (and (RClass? t1)
           (RClass? t2)
           (let [{t1-flags :flags} (reflect/type-reflect (RClass->Class t1))
                 {t2-flags :flags} (reflect/type-reflect (RClass->Class t2))]
             (some (complement :interface) [t1-flags t2-flags])))
      (or (subtype? t1 t2)
          (subtype? t2 t1))

      (or (Value? t1)
          (Value? t2)) 
      (or (subtype? t1 t2)
          (subtype? t2 t1))

      (and (CountRange? t1)
           (CountRange? t2)) 
      (countrange-overlap? t1 t2)

      (Union? t1)
      (boolean 
        (some #(overlap % t2) (.types ^Union t1)))

      (Union? t2)
      (boolean 
        (some #(overlap t1 %) (.types ^Union t2)))

      (Intersection? t1)
      (every? #(overlap % t2) (.types ^Intersection t1))

      (Intersection? t2)
      (every? #(overlap t1 %) (.types ^Intersection t2))

      (and (HeterogeneousMap? t1)
           (HeterogeneousMap? t2)) 
      (and (= (set (-> t1 :types keys))
              (set (-> t2 :types keys)))
           (every? true?
                   (for [[k1 v1] (:types t1)]
                     (let [v2 ((:types t2) k1)]
                       (overlap v1 v2)))))

      ;for map destructuring mexpansion
      (or (hmap-and-seq? t1 t2)
          (hmap-and-seq? t2 t1))
      false

      ;for vector destructuring mexpansion
      (or (hvec-and-seq? t1 t2)
          (hvec-and-seq? t2 t1))
      false

      :else true))) ;FIXME conservative result

(declare infer subst-all)

; restrict t1 to be a subtype of t2
(defn restrict [t1 t2]
  (cond
    (subtype? t1 t2) t1 ;; already a subtype

    (not (overlap t1 t2)) (Un) ;there's no overlap, so the restriction is empty

    (Union? t1) (apply Un (map (fn [e] (restrict e t2)) (:types t1)))
    (Union? t2) (apply Un (map (fn [e] (restrict t1 e)) (:types t2)))

    (Poly? t2)
    (let [names (repeatedly (:nbound t2) gensym)
          t (Poly-body* names t2)
          bbnds (Poly-bbnds* names t2)
          subst (try 
                  (infer (zipmap names bbnds) {} (list t1) (list t) t1)
                  (catch IllegalArgumentException e
                    (throw e))
                  (catch Exception e))]
      (and subst (restrict t1 (subst-all subst t1))))

    ;TODO other cases
    :else (In t2 t1)))
