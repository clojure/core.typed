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
                            (c/Poly-body* inner-names mt)))

    (r/PolyDots? mt) (let [outer-names names
                           inner-names (concat (c/PolyDots-fresh-symbols* mt))]
                       (c/PolyDots* (concat outer-names inner-names)
                                    (concat bnds (c/PolyDots-bbnds* inner-names mt))
                                    (c/PolyDots-body* inner-names mt)))
    :else (let [outer-names names]
            (c/Poly* outer-names
                     bnds
                     mt))))

(t/ann collected-ns! [t/Sym -> nil])
(defn- collected-ns! [nsym]
  {:pre [(symbol? nsym)]}
  (if-let [a uvar/*already-collected*]
    (swap! a conj nsym)
    (assert nil "Type system is not set up for namespace collection"))
  nil)

(t/ann already-collected? [t/Sym -> Boolean])
(defn- already-collected? [nsym]
  {:pre [(symbol? nsym)]
   :post [(con/boolean? %)]}
  (if-let [a uvar/*already-collected*]
    (boolean (@a nsym))
    (assert nil "Type system is not set up for namespace collection")))

;(t/ann collect-ns* [t/Sym -> nil])
(defn collect-ns*
  "Collect type annotations and dependency information
  for namespace symbol nsym, and recursively check 
  declared typed namespace dependencies."
  ([nsym {:keys [ast-for-ns collect-asts
                 collect-ns]}]
   {:pre [(symbol? nsym)
          ast-for-ns
          collect-asts
          collect-ns]}
   (p/p :collect-phase-utils/collect-ns
   (if (already-collected? nsym)
     (do #_(println (str "Already collected " nsym ", skipping"))
         #_(flush)
         nil)
     ; assume we're collecting this namespace, but only collect
     ; dependencies if they appear to refer to clojure.core.tyoed
     (do (collected-ns! nsym)
         (println (str "Start collecting " nsym))
         (flush)
         ;collect dependencies
         (let [deps (dep-u/deps-for-ns nsym)]
           (doseq [dep deps
                   :when (dep-u/should-check-ns? dep)]
             (collect-ns dep)))
         ;collect this namespace
         (let [asts (p/p :collect-phase/get-clj-analysis (ast-for-ns nsym))]
           (p/p :collect/collect-form
              (collect-asts asts)))
         (println (str "Finished collecting " nsym))
         (flush))))))

(defn assert-expr-args [{:keys [args] :as expr} cnts]
  {:pre [(set? cnts)]}
  (assert (cnts (count args)))
  (assert (every? #{:quote} (map :op args))
          (mapv :op args)))
