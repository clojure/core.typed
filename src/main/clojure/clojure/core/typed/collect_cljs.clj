(ns clojure.core.typed.collect-cljs
  (:require [cljs.core.typed :as t]
            [clojure.core.typed.analyze-cljs :as ana]
            [clojure.core.typed.type-rep :as r :refer [ret]]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.check :as chk]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.util-vars :as uvar]
            [clojure.core.typed.name-env :as nme-env]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.protocol-env :as ptl-env]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.declared-kind-env :as decl]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.util-vars :as vs]))

(defn- collected-ns! [nsym]
  (swap! t/*already-collected* conj nsym))

(defn- already-collected? [nsym]
  (boolean (@t/*already-collected* nsym)))

(declare collect)

(defn collect-ns
  "Collect type annotations and dependency information
  for namespace symbol nsym, and recursively check 
  declared typed namespace dependencies."
  ([nsym]
   (if (already-collected? nsym)
     (do #_(println (str "Already collected " nsym ", skipping"))
         #_(flush)
         nil)
     (do (collected-ns! nsym)
         (let [asts (ana/ast-for-ns nsym)]
           (doseq [ast asts]
             (collect ast)))))))

(defmulti collect (fn [expr] (:op expr)))
(defmulti invoke-special-collect (fn [expr]
                                   (when (#{:var} (-> expr :f :op))
                                     (-> expr :f :info :name))))

(defmethod invoke-special-collect 'cljs.core.typed/ann*
  [{[target texpr :as args] :args :keys [env] :as expr}]
  (assert (= (count args) 2) "Wrong arguments to ann")
  (binding [vs/*current-env* env
            vs/*current-expr* expr]
    (let [current-ns (chk/expr-ns expr)
          target-sym (:form target)
          _ (assert (symbol? target-sym) "First argument to ann must be a symbol")
          qsym (if (namespace target-sym)
                 target-sym
                 (symbol (str current-ns) (str target-sym)))
          t (prs/with-parse-ns current-ns
              (prs/parse-type (:form texpr)))]
      (var-env/add-var-type qsym t))))

;copied from CLJ implementation, should be identical
(defn gen-protocol* [current-env current-ns vsym binder mths]
  {:pre [(symbol? current-ns)]}
  (let [parsed-binder (when binder 
                        (map prs/parse-free-with-variance binder))
        s (if (namespace vsym)
            (symbol vsym)
            (symbol (str current-ns) (name vsym)))
        protocol-defined-in-nstr (namespace s)
        on-class (c/Protocol-var->on-class s)
        ; add a Name so the methods can be parsed
        _ (nme-env/declare-protocol* s)
        fs (when parsed-binder
             (map (comp r/make-F :fname) parsed-binder))
        ms (into {} (for [[knq v] mths]
                       (do
                         (assert (not (namespace knq))
                                  "Protocol method should be unqualified")
                          [knq (free-ops/with-frees fs 
                                 (binding [uvar/*current-env* current-env
                                           prs/*parse-type-in-ns* current-ns]
                                   (prs/parse-type v)))])))
        t (c/Protocol* (map :name fs) (map :variance parsed-binder) 
                       fs s on-class ms (map :bnd parsed-binder))]
    (ptl-env/add-protocol s t)
    (doseq [[kuq mt] ms]
      (assert (not (namespace kuq))
              "Protocol method names should be unqualified")
      ;qualify method names when adding methods as vars
      (let [kq (symbol protocol-defined-in-nstr (name kuq))]
        (var-env/add-nocheck-var kq)
        (var-env/add-var-type kq mt)))
    nil))

(defmethod invoke-special-collect 'cljs.core.typed/ann-protocol*
  [{[{vbnd :form} {varsym :form} {mth :form} :as args] :args :keys [env] :as expr}]
  (assert (= (count args) 3) "Wrong arguments to ann-protocol")
  (gen-protocol* env (chk/expr-ns expr) varsym vbnd mth))
  
(defmethod invoke-special-collect 'cljs.core.typed/def-alias*
  [{:keys [args env] :as expr}]
  (assert (#{2} (count args)) "Wrong arguments to def-alias")
  (let [prs-ns (chk/expr-ns expr)
        [sym typesyn] (map :form args)
        qsym (if (namespace sym)
               sym
               (symbol (str prs-ns) (name sym)))
        alias-type (binding [uvar/*current-env* env
                             prs/*parse-type-in-ns* prs-ns]
                     (prs/parse-type typesyn))]
    (nme-env/add-type-name qsym alias-type)
    (when-let [tfn (decl/declared-kind-or-nil qsym)]
      (assert (sub/subtype? alias-type tfn) (u/error-msg "Declared kind " (prs/unparse-type tfn)
                                                         " does not match actual kind " (prs/unparse-type alias-type))))
    nil))

; only collect invocations in arbitrarily nested `do` blocks
(defmethod collect :do
  [{:keys [statements ret]}]
  (doall (map collect (concat statements [ret]))))

(defmethod collect :invoke
  [expr]
  (invoke-special-collect expr))

(defmethod collect :default
  [expr]
  nil)

(defmethod invoke-special-collect :default
  [expr]
  nil)
