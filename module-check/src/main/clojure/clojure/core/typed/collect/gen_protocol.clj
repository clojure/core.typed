(ns clojure.core.typed.collect.gen-protocol
  (:require [clojure.core.typed.errors :as err]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.name-env :as nme-env]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.deps.clojure.math.combinatorics :as comb]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.protocol-env :as ptl-env]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.collect-utils :as clt-u]))

(defn gen-protocol* [current-env current-ns vsym binder mths]
  {:pre [(symbol? current-ns)]}
  ;(prn "gen-protocol*")
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
        _ (assert (== (count fs) (count bnds)))
        _ (assert ((some-fn nil? map?) mths))
        _ (when-let [[m] (seq (remove symbol? (keys mths)))]
            (err/int-error (str "Method names to ann-protocol must be symbols: " m)))
        ;_ (prn "gen-protocol after method symbol check")
        _ (doseq [[n1 n2] (comb/combinations (keys mths) 2)]
            (when (= (munge n1) (munge n2))
              (err/int-error 
                (str "Protocol methods for " vsym " must have distinct representations: "
                     "both " n1 " and " n2 " compile to " (munge n1)))))
        ;_ (prn "gen-protocol after distinct rep check")
        ms (into {} (for [[knq v] mths]
                      (let [_ (when (namespace knq)
                                (err/int-error "Protocol method should be unqualified"))
                            mtype (free-ops/with-bounded-frees (zipmap fs bnds)
                                    (binding [vs/*current-env*       current-env
                                              prs/*parse-type-in-ns* current-ns]
                                      ;(prn "parsing" v current-ns *ns*)
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
                             ;(prn "throwing method type")
                             (err/int-error (str "Protocol method " knq " should be a possibly-polymorphic function intersection"
                                               " taking at least one fixed argument: "
                                               (prs/unparse-type mtype)))))
                         [knq mtype])))
        ;_ (prn "collect protocol methods" (into {} ms))
        t (c/Protocol* (map :name fs) (map :variance parsed-binder) 
                       fs s on-class ms (map :bnd parsed-binder))]
    ;(prn "Adding protocol" s t)
    (ptl-env/add-protocol s t)
    ; annotate protocol var as Any
    (var-env/add-nocheck-var s)
    (var-env/add-var-type s r/-any)
    (doseq [[kuq mt] ms]
      (assert (not (namespace kuq))
              "Protocol method names should be unqualified")
      ;qualify method names when adding methods as vars
      (let [kq (symbol protocol-defined-in-nstr (name kuq))
            mt-ann (clt-u/protocol-method-var-ann mt (map :name fs) bnds)]
        (var-env/add-nocheck-var kq)
        (var-env/add-var-type kq mt-ann)))
    ;(prn "end gen-protocol" s)
    nil))
