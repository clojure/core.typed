(ns ^:skip-wiki clojure.core.typed.inst
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.subst :as subst]
            [clojure.core.typed.trans :as trans])
  (:import (clojure.core.typed.type_rep Poly Bounds PolyDots F)))

(alter-meta! *ns* assoc :skip-wiki true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polymorphic type instantiation

(defn same-or-narrower-bounds?
  "True if arg-bounds is inside bounds, false otherwise"
  [bounds arg-bounds]
  {:pre [(r/Bounds? bounds)
         (r/Bounds? arg-bounds)]
   :post [(u/boolean? %)]}
  (and (sub/subtype? (:lower-bound bounds) (:lower-bound arg-bounds))
       (sub/subtype? (:upper-bound arg-bounds) (:upper-bound bounds))
       ; make sure bounds make sense
       (sub/subtype? (:lower-bound bounds) (:upper-bound bounds))
       (sub/subtype? (:lower-bound arg-bounds) (:upper-bound arg-bounds))))

(defn satisfies-bounds?
  "True if type t is inside bounds"
  [t bnds]
  (and (sub/subtype? t (:upper-bound bnds))
       (sub/subtype? (:lower-bound bnds) t)))

(defn manual-inst 
  "Poly Type^n -> Type
  Substitute the type parameters of the polymorphic type
  with given types"
  [ptype argtys]
  {:pre [((some-fn r/Poly? r/PolyDots?) ptype)
         (every? r/Type? argtys)]
   :post [(r/Type? %)]}
  (when-not (seq argtys)
    (u/int-error (str "Must provide arguments to inst")))
  (cond
    (r/Poly? ptype)
    (let [_ (assert (= (:nbound ptype) (count argtys)) 
                    (u/error-msg "Wrong number of arguments to instantiate polymorphic type"))
          names (c/Poly-fresh-symbols* ptype)
          body (c/Poly-body* names ptype)
          bbnds (c/Poly-bbnds* names ptype)]
      (doseq [[nme ty ^Bounds bnds] (map vector names argtys bbnds)]
        (if (.higher-kind bnds)
          (do 
            (if (r/F? ty)
              ; instantiating a higher rank F with another F
              (assert (let [param-bnds bnds
                            arg-bnds (free-ops/free-with-name-bnds (.name ^F ty))
                            _ (assert arg-bnds (str "Type varible " (prs/unparse-type ty) " not in scope"))]
                        ; bounds of arg must be within parameter's bounds
                        (same-or-narrower-bounds? param-bnds arg-bnds))
                      (u/error-msg "Must instantitate higher-order type variable with another higher-order type variable, given: "
                                   (prs/unparse-type ty)))
              (do 
                (assert (r/TypeFn? ty) (u/error-msg "Must instantiate higher-order type variable with type function, given:"
                                                    (prs/unparse-type ty)))
                (assert (satisfies-bounds? ty bnds)
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
    (let [nrequired-types (dec (:nbound ptype))
          _ (assert (<= nrequired-types (count argtys)) 
                    "Insufficient arguments to instantiate dotted polymorphic type")
          names (c/PolyDots-fresh-symbols* ptype)
          body (c/PolyDots-body* names ptype)
          bbnds (c/PolyDots-bbnds* names ptype)]
      (doseq [[nme ty ^Bounds bnds] (map vector names argtys bbnds)]
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
