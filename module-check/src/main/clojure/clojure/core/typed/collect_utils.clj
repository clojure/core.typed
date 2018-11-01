(ns clojure.core.typed.collect-utils
  (:require [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.type-rep :as r]))

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
