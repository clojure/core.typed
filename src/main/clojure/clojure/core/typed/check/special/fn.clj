(ns clojure.core.typed.check.special.fn
  (:require [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.subtype :as sub]))

(defn check-special-fn 
  [check {[_ _ {{fn-anns :ann} :val} :as statements] :statements fexpr :ret :as expr} expected]
  {:pre [(#{3} (count statements))]}
  (let [ann-expected
        (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
          (apply
            r/make-FnIntersection
            (doall
              (for [{:keys [dom rest drest ret-type]} fn-anns]
                (r/make-Function (mapv (comp prs/parse-type :type) dom)
                                 (prs/parse-type (:type ret-type))
                                 (when rest
                                   (prs/parse-type (:type rest)))
                                 (when drest
                                   (r/DottedPretype1-maker
                                     (prs/parse-type (:pretype drest))
                                     (:bound drest))))))))

        ; if the t/fn statement looks unannotated, use the expected type if possible
        use-expected (if (every? (fn [{:keys [dom rest drest rng] :as f}]
                                   {:pre [(r/Function? f)]}
                                   (and (every? #{r/-any} dom)
                                        ((some-fn nil? #{r/-any}) rest)
                                        (#{r/-any} (:t rng))))
                                 (:types ann-expected))
                       (or (when expected (r/ret-t expected)) ann-expected)
                       ann-expected)
        cfexpr (check fexpr (r/ret use-expected))
        _ (when expected
            (let [actual (-> cfexpr u/expr-type r/ret-t)]
              (when-not (sub/subtype? actual (r/ret-t expected))
                (cu/expected-error actual (r/ret-t expected)))))]
    (assoc expr
           :ret cfexpr
           u/expr-type (u/expr-type cfexpr))))
