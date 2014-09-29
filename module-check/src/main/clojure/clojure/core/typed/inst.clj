(ns ^:skip-wiki clojure.core.typed.inst
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.subst :as subst]
            [clojure.core.typed.trans :as trans]
            [clojure.string :as string]
            [clojure.pprint :as pprint])
  (:import (clojure.core.typed.type_rep Poly Bounds PolyDots F)))

(alter-meta! *ns* assoc :skip-wiki true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polymorphic type instantiation

(defn same-or-narrower-bounds?
  "True if arg-bounds is inside bounds, false otherwise"
  [bounds arg-bounds]
  {:pre [(r/Bounds? bounds)
         (r/Bounds? arg-bounds)]
   :post [(con/boolean? %)]}
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
  (cond
    (r/Poly? ptype)
    (let [_ (when-not (= (:nbound ptype) (count argtys)) 
              (err/int-error
                (str "Wrong number of arguments to instantiate polymorphic type (expected " (:nbound ptype)
                     ", actual " (count argtys)
                     "\n\nTarget:\n" (prs/unparse-type ptype)
                     "\n\nActual arguments:\n" (string/join " " (map prs/unparse-type argtys)))))
          names (c/Poly-fresh-symbols* ptype)
          body (c/Poly-body* names ptype)
          bbnds (c/Poly-bbnds* names ptype)]
      (free-ops/with-bounded-frees (zipmap (map r/make-F names) bbnds)
        (doseq [[nme ty bnds] (map vector names argtys bbnds)]
          (assert (not (:higher-kind bnds)))
          (let [lower-bound (subst/substitute-many (:lower-bound bnds) argtys names)
                upper-bound (subst/substitute-many (:upper-bound bnds) argtys names)]
            (when-not (sub/subtype? lower-bound upper-bound)
              (err/int-error
                (str "Lower-bound " (prs/unparse-type lower-bound)
                     " is not below upper-bound " (prs/unparse-type upper-bound))))
            (when-not (and (sub/subtype? ty upper-bound)
                           (sub/subtype? lower-bound ty))
              (err/int-error
                (str "Manually instantiated type " (prs/unparse-type ty)
                     " is not between bounds " (prs/unparse-type lower-bound)
                     " and " (prs/unparse-type upper-bound))))))
        (subst/substitute-many body argtys names)))

    (r/PolyDots? ptype)
    (let [nrequired-types (dec (:nbound ptype))
          _ (when-not (<= nrequired-types (count argtys)) 
              (err/int-error
                (str "Insufficient arguments to instantiate dotted polymorphic type")))
          names (c/PolyDots-fresh-symbols* ptype)
          body (c/PolyDots-body* names ptype)
          bbnds (c/PolyDots-bbnds* names ptype)]
      (free-ops/with-bounded-frees (zipmap (-> (map r/make-F names) butlast) (butlast bbnds))
        (doseq [[nme ty bnds] (map vector names argtys bbnds)]
          (let [lower-bound (subst/substitute-many (:lower-bound bnds) argtys names)
                upper-bound (subst/substitute-many (:upper-bound bnds) argtys names)]
            (when-not (sub/subtype? lower-bound upper-bound)
              (err/int-error
                (str "Lower-bound " (prs/unparse-type lower-bound)
                     " is not below upper-bound " (prs/unparse-type upper-bound))))
            (when-not (and (sub/subtype? ty upper-bound)
                           (sub/subtype? lower-bound ty))
              (err/int-error
                (str "Manually instantiated type " (prs/unparse-type ty)
                     " is not between bounds " (prs/unparse-type lower-bound)
                     " and " (prs/unparse-type upper-bound))))))
        (-> body
          ; expand dotted pre-types in body
          (trans/trans-dots (last names) ;the bound
                            (drop (dec (:nbound ptype)) argtys)) ;the types to expand pre-type with
          ; substitute normal variables
          (subst/substitute-many (take nrequired-types argtys) (butlast names)))))))
