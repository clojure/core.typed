(ns clojure.core.typed.inst
  (:require [clojure.core.typed
             [utils :as u]
             [parse-unparse :as prs]
             [type-rep :as r]
             [type-ctors :as c]
             [free-ops :as free-ops]
             [subtype :as sub]
             [subst :as subst]
             [trans :as trans]])
  (:import (clojure.core.typed.type_rep Poly Bounds PolyDots F)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polymorphic type instantiation

(defn manual-inst 
  "Poly Type^n -> Type
  Substitute the type parameters of the polymorphic type
  with given types"
  [ptype argtys]
  {:pre [((some-fn r/Poly? r/PolyDots?) ptype)
         (seq argtys)
         (every? r/Type? argtys)]
   :post [(r/Type? %)]}
  (cond
    (r/Poly? ptype)
    (let [^Poly ptype ptype
          _ (assert (= (.nbound ptype) (count argtys)) (u/error-msg "Wrong number of arguments to instantiate polymorphic type"))
          names (repeatedly (.nbound ptype) gensym)
          body (c/Poly-body* names ptype)
          bbnds (c/Poly-bbnds* names ptype)]
      (doseq [[nme ty ^Bounds bnds] (map vector names argtys bbnds)]
        (if (.higher-kind bnds)
          (do 
            (if (r/F? ty)
              (assert (and (r/TypeFn? (.higher-kind bnds))
                           (let [given-bnds (free-ops/free-with-name-bnds (.name ^F ty))
                                 _ (assert given-bnds free-ops/*free-scope*)]
                             (and (.higher-kind given-bnds)
                                  (sub/subtype? (.higher-kind given-bnds) (.higher-kind bnds)))))
                      (u/error-msg "Must instantitate higher-order type variable with another higher-order type variable, given: "
                                 (prs/unparse-type ty)))
              (do 
                (assert (r/TypeFn? ty) (u/error-msg "Must instantiate higher-order type variable with type function, given:"
                                                    (prs/unparse-type ty)))
                (assert (sub/subtype? ty (.higher-kind bnds))
                        (u/error-msg "Higher-order type variable " (prs/unparse-type ty)
                                   " does not match bound " (prs/unparse-type (.higher-kind bnds)))))))
          (let [lower-bound (subst/substitute-many (.lower-bound bnds) argtys names)
                upper-bound (subst/substitute-many (.upper-bound bnds) argtys names)]
            (assert (sub/subtype? lower-bound upper-bound)
                    (u/error-msg "Lower-bound " (prs/unparse-type lower-bound)
                               " is not below upper-bound " (prs/unparse-type upper-bound)))
            (assert (and (sub/subtype? ty upper-bound)
                         (sub/subtype? lower-bound ty))
                    (u/error-msg "Manually instantiated type " (prs/unparse-type ty)
                                 " is not between bounds " (prs/unparse-type lower-bound)
                                 " and " (prs/unparse-type upper-bound))))))
      (subst/substitute-many body argtys names))

    (r/PolyDots? ptype)
    (let [^PolyDots ptype ptype
          nrequired-types (dec (.nbound ptype))
          _ (assert (<= nrequired-types (count argtys)) "Insufficient arguments to instantiate dotted polymorphic type")
          names (repeatedly (.nbound ptype) gensym)
          body (c/PolyDots-body* names ptype)
          bbnds (c/PolyDots-bbnds* names ptype)]
      (doseq [[nme ty ^Bounds bnds] (map vector names argtys bbnds)]
        (assert (not (.higher-kind bnds)) "NYI")
        (let [lower-bound (subst/substitute-many (.lower-bound bnds) argtys names)
              upper-bound (subst/substitute-many (.upper-bound bnds) argtys names)]
          (assert (sub/subtype? lower-bound upper-bound)
                  (u/error-msg "Lower-bound " (prs/unparse-type lower-bound)
                             " is not below upper-bound " (prs/unparse-type upper-bound)))
          (assert (and (sub/subtype? ty upper-bound)
                       (sub/subtype? lower-bound ty))
                  (u/error-msg "Manually instantiated type " (prs/unparse-type ty)
                             " is not between bounds " (prs/unparse-type lower-bound)
                             " and " (prs/unparse-type upper-bound)))))
      (-> body
        ; expand dotted pre-types in body
        (trans/trans-dots (last names) ;the bound
                          (drop (dec (:nbound ptype)) argtys)) ;the types to expand pre-type with
        ; substitute normal variables
        (subst/substitute-many (take nrequired-types argtys) (butlast names))))))
