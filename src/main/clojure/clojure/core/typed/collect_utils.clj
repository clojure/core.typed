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

(defn gen-protocol* [current-env current-ns vsym binder mths]
  {:pre [(symbol? current-ns)]}
  (let [_ (when-not (symbol? vsym)
            (err/int-error
              (str "First argument to ann-protocol must be a symbol: " vsym)))
        s (if (namespace vsym)
            (symbol vsym)
            (symbol (str current-ns) (name vsym)))
        ;_ (prn "gen-protocol*" s)
        protocol-defined-in-nstr (namespace s)
        on-class (c/Protocol-var->on-class s)
        ; add a Name so the methods can be parsed
        _ (nme-env/declare-protocol* s)
        ;_ (prn "gen-protocol before parsed-binder")
        parsed-binder (when binder 
                        (binding [prs/*parse-type-in-ns* current-ns]
                          (prs/parse-free-binder-with-variance binder)))
        ;_ (prn "gen-protocol after parsed-binder")
        fs (when parsed-binder
             (map (comp r/make-F :fname) parsed-binder))
        bnds (when parsed-binder
               (map :bnd parsed-binder))
        _ (assert (= (count fs) (count bnds)))
        _ (assert ((some-fn nil? map?) mths))
        _ (when-let [[m] (seq (remove symbol? (keys mths)))]
            (err/int-error (str "Method names to ann-protocol must be symbols: " m)))
        _ (doseq [[n1 n2] (comb/combinations (keys mths) 2)]
            (when (= (munge n1) (munge n2))
              (err/int-error 
                (str "Protocol methods for " vsym " must have distinct representations: "
                     "both " n1 " and " n2 " compile to " (munge n1)))))
        ms (into {} (for [[knq v] mths]
                      (let [_ (when (namespace knq)
                                (err/int-error "Protocol method should be unqualified"))
                            mtype (free-ops/with-bounded-frees (zipmap fs bnds)
                                    (binding [uvar/*current-env* current-env
                                              prs/*parse-type-in-ns* current-ns]
                                      (prs/parse-type v)))]
                         (let [rt (c/fully-resolve-type mtype)
                               fin? (fn [f]
                                      (let [f (c/fully-resolve-type f)]
                                        (boolean
                                          (when (r/FnIntersection? f)
                                            (every? seq (map :dom (:types f)))))))]
                           (when-not 
                             (or
                               (fin? rt)
                               (when (r/Poly? rt) 
                                 (let [names (c/Poly-fresh-symbols* rt)]
                                   (fin? (c/Poly-body* names rt))))
                               (when (r/PolyDots? rt) 
                                 (let [names (c/PolyDots-fresh-symbols* rt)]
                                   (fin? (c/PolyDots-body* names rt)))))
                             (err/int-error (str "Protocol method " knq " should be a possibly-polymorphic function intersection"
                                               " taking at least one fixed argument: "
                                               (prs/unparse-type mtype)))))
                         [knq mtype])))
        ;_ (prn "collect protocol methods" (into {} ms))
        t (c/Protocol* (map :name fs) (map :variance parsed-binder) 
                       fs s on-class ms (map :bnd parsed-binder))]
    (ptl-env/add-protocol s t)
    ; annotate protocol var as Any
    (var-env/add-nocheck-var s)
    (var-env/add-var-type s r/-any)
    (doseq [[kuq mt] ms]
      (assert (not (namespace kuq))
              "Protocol method names should be unqualified")
      ;qualify method names when adding methods as vars
      (let [kq (symbol protocol-defined-in-nstr (name kuq))
            mt-ann (protocol-method-var-ann mt (map :name fs) bnds)]
        (var-env/add-nocheck-var kq)
        (var-env/add-var-type kq mt-ann)))
    ;(prn "end gen-protocol" s)
    nil))

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

;(t/ann collect-ns [t/Sym -> nil])
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

