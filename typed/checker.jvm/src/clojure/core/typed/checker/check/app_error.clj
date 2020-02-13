;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.app-error
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.jvm.subtype :as sub]
            [clojure.string :as str]))

;; true if domains of l is a subtype of domains of r.
;; returns false if any of arg-ret-types are a subtype of ;; r.
(defn domain-subtype? [l r arg-ret-types]
  (boolean
    (when (and (not (:kws r))
               (not (:drest r))
               (not (:rest r)))
      (and (== (count (:dom l))
               (count (:dom r))
               (count arg-ret-types))
           (every? identity 
                   (map (fn [ld rd ad]
                          (and (sub/subtype? ld rd)
                               (not (sub/subtype? rd (r/ret-t ad)))))
                        (:dom l) 
                        (:dom r)
                        arg-ret-types))))))

(defn trim-arities [arities arg-ret-types]
  ;try and prune some of the arities
  ; Lots more improvements we can port from Typed Racket:
  ;  typecheck/tc-app-helper.rkt
  (let [matching-arities 
          (remove (fn [{:keys [dom rest drest kws rng]}]
                    ;remove arities that have a differing
                    ; number of fixed parameters than what we
                    ; require
                    (or
                      (and (not rest) (not drest) (not kws)
                           (not= (count dom)
                                 (count arg-ret-types)))
                      ; remove if we don't have even the fixed args
                      (< (count arg-ret-types)
                         (count dom))))
                  arities)
        remove-sub
          (reduce (fn [acc ar]
                    ;; assumes most general arities come last
                    (conj (vec (remove #(domain-subtype? % ar arg-ret-types) acc)) 
                          ar))
                  [] matching-arities)]
    (or (seq remove-sub)
        ;if we remove all the arities, default to all of them
        arities)))

;[Expr (Seqable Expr) (Seqable TCResult) (Option TCResult) Boolean
; -> Any]
(defn ^String app-type-error [fexpr args fin arg-ret-types expected poly?]
  {:pre [(r/FnIntersection? fin)
         (or (not poly?)
             ((some-fn r/Poly? r/PolyDots?) poly?))]
   :post [(r/TCResult? %)]}
  (let [fin (apply r/make-FnIntersection (trim-arities (:types fin) arg-ret-types))
        static-method? (= :static-call (:op fexpr))
        instance-method? (= :instance-call (:op fexpr))
        method-sym (when (or static-method? instance-method?)
                     (cu/MethodExpr->qualsym fexpr))]
    (prs/with-unparse-ns (or prs/*unparse-type-in-ns*
                             (or (when fexpr
                                   (cu/expr-ns fexpr))
                                 (when vs/*current-expr*
                                   (cu/expr-ns vs/*current-expr*))))
      (err/tc-delayed-error
        (str
          (if poly?
            (str "Polymorphic " 
                 (cond static-method? "static method "
                       instance-method? "instance method "
                       :else "function "))
            (cond static-method? "Static method "
                  instance-method? "Instance method "
                  :else "Function "))
          (if (or static-method?
                  instance-method?)  
            method-sym
            (if fexpr
              (ast-u/emit-form-fn fexpr)
              "<NO FORM>"))
          " could not be applied to arguments:\n"
          (when poly?
            (let [names (cond 
                          (r/Poly? poly?) (c/Poly-fresh-symbols* poly?)
                          ;PolyDots
                          :else (c/PolyDots-fresh-symbols* poly?))
                  bnds (if (r/Poly? poly?)
                         (c/Poly-bbnds* names poly?)
                         (c/PolyDots-bbnds* names poly?))
                  dotted (when (r/PolyDots? poly?)
                           (last names))]
              (str "Polymorphic Variables:\n\t"
                   (str/join "\n\t" 
                             (map (partial apply pr-str)
                                  (map (fn [{:keys [lower-bound upper-bound] :as bnd} nme]
                                         {:pre [(r/Bounds? bnd)
                                                (symbol? nme)]}
                                         (cond
                                           (= nme dotted) [nme '...]
                                           :else (concat [nme]
                                                         (when-not (= r/-nothing lower-bound)
                                                           [:> (prs/unparse-type lower-bound)])
                                                         (when-not (= r/-any upper-bound)
                                                           [:< (prs/unparse-type upper-bound)]))))
                                       bnds (map (comp r/F-original-name r/make-F) names)))))))
          "\n\nDomains:\n\t" 
          (str/join "\n\t" 
                    (map (partial apply pr-str) 
                         (map (fn [{:keys [dom rest drest kws prest pdot]}]
                                (concat (map prs/unparse-type dom)
                                        (when rest
                                          [(prs/unparse-type rest) '*])
                                        (when-let [{:keys [pre-type name]} drest]
                                          [(prs/unparse-type pre-type)
                                           '...
                                           (-> name r/make-F r/F-original-name)])
                                        (letfn [(readable-kw-map [m]
                                                  (into {} (for [[k v] m]
                                                             (do (assert (r/Value? k))
                                                                 [(:val k) (prs/unparse-type v)]))))]
                                          (when-let [{:keys [mandatory optional]} kws]
                                            (concat ['&]
                                                    (when (seq mandatory)
                                                      [:mandatory (readable-kw-map mandatory)])
                                                    (when (seq optional)
                                                      [:optional (readable-kw-map optional)]))))
                                        (when prest
                                          [(prs/unparse-type prest) '<*])
                                        (when-let [{:keys [pre-type name]} pdot]
                                          [(prs/unparse-type pre-type)
                                           '<...
                                           (-> name r/make-F r/F-original-name)])))
                              (:types fin))))
          "\n\n"
          "Arguments:\n\t" (apply prn-str (mapv (comp prs/unparse-type r/ret-t) arg-ret-types))
          "\n"
          "Ranges:\n\t"
          (str/join "\n\t" 
                    (map (partial apply pr-str) (map (comp prs/unparse-result :rng) (:types fin))))
          "\n\n"
          (when expected (str "with expected type:\n\t" (pr-str (prs/unparse-type (r/ret-t expected))) "\n\n"))
          #_#_"in: "
          (if fexpr
            (if (or static-method? instance-method?)
              (ast-u/emit-form-fn fexpr)
              (list* (ast-u/emit-form-fn fexpr)
                     (map ast-u/emit-form-fn args)))
            "<NO FORM>"))
        :expected expected
        :return (or expected (r/ret r/Err))))))

(defn ^String polyapp-type-error [fexpr args fexpr-type arg-ret-types expected]
  {:pre [((some-fn r/Poly? r/PolyDots?) fexpr-type)]
   :post [(r/TCResult? %)]}
  (let [fin (if (r/Poly? fexpr-type)
              (c/Poly-body* (c/Poly-fresh-symbols* fexpr-type) fexpr-type)
              (c/PolyDots-body* (c/PolyDots-fresh-symbols* fexpr-type) fexpr-type))]
    (app-type-error fexpr args fin arg-ret-types expected fexpr-type)))

(defn ^String plainapp-type-error [fexpr args fexpr-type arg-ret-types expected]
  {:pre [(r/FnIntersection? fexpr-type)]
   :post [(r/TCResult? %)]}
  (app-type-error fexpr args fexpr-type arg-ret-types expected false))

