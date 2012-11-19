(in-ns 'typed.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polymorphic type instantiation

(defn manual-inst 
  "Poly Type^n -> Type
  Substitute the type parameters of the polymorphic type
  with given types"
  [ptype argtys]
  {:pre [((some-fn Poly? PolyDots?) ptype)
         (seq argtys)
         (every? Type? argtys)]
   :post [(Type? %)]}
  (cond
    (Poly? ptype)
    (let [_ (assert (= (.nbound ptype) (count argtys)) (error-msg "Wrong number of arguments to instantiate polymorphic type"))
          names (repeatedly (.nbound ptype) gensym)
          body (Poly-body* names ptype)
          bbnds (Poly-bbnds* names ptype)]
      (doseq [[nme ty bnds] (map vector names argtys bbnds)]
        (if (.higher-kind bnds)
          (do 
            (if (F? ty)
              (assert (and (TypeFn? (.higher-kind bnds))
                           (let [given-bnds (free-with-name-bnds (.name ty))
                                 _ (assert given-bnds *free-scope*)]
                             (and (.higher-kind given-bnds)
                                  (subtype? (.higher-kind given-bnds) (.higher-kind bnds)))))
                      (error-msg "Must instantitate higher-order type variable with another higher-order type variable, given: "
                                 (unparse-type ty)))
              (do 
                (assert (TypeFn? ty) (error-msg "Must instantiate higher-order type variable with type function, given:"
                                                (unparse-type ty)))
                (assert (subtype? ty (.higher-kind bnds))
                        (error-msg "Higher-order type variable " (unparse-type ty)
                                   " does not match bound " (unparse-type (.higher-kind bnds)))))))
          (let [lower-bound (substitute-many (.lower-bound bnds) argtys names)
                upper-bound (substitute-many (.upper-bound bnds) argtys names)]
            (assert (subtype? lower-bound upper-bound)
                    (error-msg "Lower-bound " (unparse-type lower-bound)
                               " is not below upper-bound " (unparse-type upper-bound)))
            (assert (and (subtype? ty upper-bound)
                         (subtype? lower-bound ty))
                    (error-msg "Manually instantiated type " (unparse-type ty)
                               " is not between bounds " (unparse-type lower-bound)
                               " and " (unparse-type upper-bound))))))
      (substitute-many body argtys names))

    (PolyDots? ptype)
    (let [nrequired-types (dec (.nbound ptype))
          _ (assert (<= nrequired-types (count argtys)) "Insufficient arguments to instantiate dotted polymorphic type")
          names (repeatedly (.nbound ptype) gensym)
          body (PolyDots-body* names ptype)
          bbnds (PolyDots-bbnds* names ptype)]
      (doseq [[nme ty bnds] (map vector names argtys bbnds)]
        (assert (not (.higher-kind bnds)) "NYI")
        (let [lower-bound (substitute-many (.lower-bound bnds) argtys names)
              upper-bound (substitute-many (.upper-bound bnds) argtys names)]
          (assert (subtype? lower-bound upper-bound)
                  (error-msg "Lower-bound " (unparse-type lower-bound)
                             " is not below upper-bound " (unparse-type upper-bound)))
          (assert (and (subtype? ty upper-bound)
                       (subtype? lower-bound ty))
                  (error-msg "Manually instantiated type " (unparse-type ty)
                             " is not between bounds " (unparse-type lower-bound)
                             " and " (unparse-type upper-bound)))))
      (-> body
        ; expand dotted pre-types in body
        (trans-dots (last names) ;the bound
                    (drop (dec (:nbound ptype)) argtys)) ;the types to expand pre-type with
        ; substitute normal variables
        (substitute-many (take nrequired-types argtys) (butlast names))))))
