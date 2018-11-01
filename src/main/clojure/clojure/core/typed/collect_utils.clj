(ns clojure.core.typed.collect-utils
  (:require [clojure.core.typed.errors :as err]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.name-env :as nme-env]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.type-rep :as r]
            [clojure.math.combinatorics :as comb]
            [clojure.core.typed.ns-deps-utils :as dep-u]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.util-vars :as uvar]
            [clojure.core.typed.protocol-env :as ptl-env]
            [clojure.core.typed.profiling :as p]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed :as t]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.utils :as u]))

(defn protocol-method-var-ann [mt names bnds]
  (cond
    (r/Poly? mt) (let [outer-names names
                       inner-names (concat (c/Poly-fresh-symbols* mt))]
                   (c/Poly* (concat outer-names inner-names)
                            (concat bnds (c/Poly-bbnds* inner-names mt))
                            (c/Poly-body* inner-names mt)
                            :named (:named mt)))

    (r/PolyDots? mt) (let [outer-names names
                           inner-names (concat (c/PolyDots-fresh-symbols* mt))]
                       (c/PolyDots* (concat outer-names inner-names)
                                    (concat bnds (c/PolyDots-bbnds* inner-names mt))
                                    (c/PolyDots-body* inner-names mt)
                                    :named (:named mt)))
    :else (let [outer-names names]
            (c/Poly* outer-names
                     bnds
                     mt))))
