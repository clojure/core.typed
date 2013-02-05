(in-ns 'typed.core)


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


;true if types t1 and t2 overlap (NYI)
(defn overlap [t1 t2]
  (let [eq (= t1 t2)]
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
          (Value? t2)) (or (subtype? t1 t2)
                           (subtype? t2 t1))
      (and (CountRange? t1)
           (CountRange? t2)) (countrange-overlap? t1 t2)
      ;    (and (Name? t1)
      ;         (Name? t2)) (overlap (-resolve t1) (-resolve t2))
      ;    (Name? t1) (overlap (-resolve t1) t2)
      ;    (Name? t2) (overlap t1 (-resolve t2))
      (and (HeterogeneousMap? t1)
           (HeterogeneousMap? t2)) (and (= (set (-> t1 :types keys))
                                           (set (-> t2 :types keys)))
                                        (every? true?
                                                (for [[k1 v1] (:types t1)]
                                                  (let [v2 ((:types t2) k1)]
                                                    (overlap v1 v2)))))
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
