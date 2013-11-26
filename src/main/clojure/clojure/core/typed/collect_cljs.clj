(ns ^:skip-wiki clojure.core.typed.collect-cljs
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
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.collect-phase :as coll-clj]
            [clojure.core.typed.datatype-ancestor-env :as ancest]
            [clojure.core.typed.datatype-env :as dt-env]
            [clojure.repl :as repl]
            [clojure.core.typed.util-vars :as vs]))

(alter-meta! *ns* assoc :skip-wiki true)

(defn- collected-ns! [nsym]
  (if-let [a t/*already-collected*]
    (swap! a conj nsym)
    (assert nil "Type system is not set up for namespace collection")))

(defn- already-collected? [nsym]
  (if-let [a t/*already-collected*]
    (boolean (@a nsym))
    (assert nil "Type system is not set up for namespace collection")))

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
         (prn "Collecting " nsym)
         (let [asts (ana/ast-for-ns nsym)]
           (doseq [ast asts]
             (collect ast)))))))

(defmulti collect (fn [expr] (:op expr)))
(defmulti invoke-special-collect (fn [expr]
                                   (when (#{:var} (-> expr :f :op))
                                     (-> expr :f :info :name))))

(defmethod collect :def
  [{:keys [name form env] :as expr}]
  (let [mvar (-> form second meta)
        prs-ns (chk/expr-ns expr)]
    (binding [vs/*current-env* env
              vs/*current-expr* expr
              prs/*parse-type-in-ns* prs-ns]
      (when-let [[_ tsyn] (find mvar :ann)]
        (assert ((some-fn list? seq?) tsyn) 
                (str "Type must be quoted in :ann metadata for def " name ": " tsyn))
        ; CLJS does not currently evaluate metadata attached to
        ; a def. We want to conform to CLJ's behaviour so we "unwrap"
        ; one level of quoting. inline-annotation-test unit test should 
        ; fail if this ever gets "fixed".
        (let [unwrap-one (second tsyn)
              ann-type (prs/parse-type unwrap-one)]
          (var-env/add-var-type name ann-type)))
      (when (:no-check mvar)
        (var-env/add-nocheck-var name)))))

(defmethod invoke-special-collect 'cljs.core.typed/typed-deps*
  [{:keys [args] :as expr}]
  (assert (= (count args) 1) "Wrong arguments to typed-deps*")
  (let [prs-ns (chk/expr-ns expr)
        deps (:form (first args))
        _ (prn "Found typed deps" deps)
        _ (assert (and deps (seq deps) (every? symbol? deps)))]
    (if t/*already-collected*
      (do ;(dep/add-ns-deps prs-ns (set deps))
          (doseq [dep deps]
            (collect-ns dep)))
      (do (println "WARNING: Not collecting namespaces, must call typed-deps via check-ns")
          (flush)))
    nil))

(defmethod invoke-special-collect 'cljs.core.typed/ann*
  [{[target texpr :as args] :args :keys [env] :as expr}]
  (assert (= (count args) 2) "Wrong arguments to ann")
  (binding [vs/*current-env* env
            vs/*current-expr* expr]
    (let [current-ns (chk/expr-ns expr)
          target-sym (:form target)
          {:keys [no-check] :as opt} (meta target-sym)
          _ (assert (symbol? target-sym) "First argument to ann must be a symbol")
          qsym (if (namespace target-sym)
                 target-sym
                 (symbol (str current-ns) (str target-sym)))
          t (prs/with-parse-ns current-ns
              (prs/parse-type (:form texpr)))]
      (when no-check 
        (var-env/add-nocheck-var qsym))
      (var-env/add-var-type qsym t))))

;copied from CLJ implementation, should be identical
(defn gen-protocol* [current-env current-ns vsym binder mths]
  {:pre [(symbol? current-ns)]}
  (let [s (if (namespace vsym)
            (symbol vsym)
            (symbol (str current-ns) (name vsym)))
        protocol-defined-in-nstr (namespace s)
        on-class (c/Protocol-var->on-class s)
        ; add a Name so the methods can be parsed
        _ (nme-env/declare-protocol* s)
        parsed-binder (when binder 
                        (binding [prs/*parse-type-in-ns* current-ns]
                          (prs/parse-free-binder-with-variance binder)))
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

(defn gen-datatype* [current-env current-ns provided-name fields vbnd opt record?]
  {:pre [(symbol? current-ns)]}
  (impl/with-cljs-impl
    (let [{ancests :unchecked-ancestors} opt
          _ (when ancests
              (assert nil "Unchecked ancestors for CLJS NYI"))
          parsed-binders (when vbnd
                           (binding [prs/*parse-type-in-ns* current-ns]
                             (prs/parse-free-binder-with-variance vbnd)))
          ;variances
          vs (seq (map :variance parsed-binders))
          args (seq (map :fname parsed-binders))
          bnds (seq (map :bnd parsed-binders))]
      (let [provided-name-str (str provided-name)
            ;_ (prn "provided-name-str" provided-name-str)
            munged-ns-str (if (some #(= \. %) provided-name-str)
                            (apply str (butlast (apply concat (butlast (partition-by #(= \. %) provided-name-str)))))
                            (str (munge current-ns)))
            ;_ (prn "munged-ns-str" munged-ns-str)
            demunged-ns-str (str (repl/demunge munged-ns-str))
            ;_ (prn "demunged-ns-str" demunged-ns-str)
            local-name (if (some #(= \. %) provided-name-str)
                         (symbol (apply str (last (partition-by #(= \. %) (str provided-name-str)))))
                         provided-name-str)
            ;_ (prn "local-name" local-name)
            s (symbol (str munged-ns-str \. local-name))
            fs (apply array-map (apply concat (free-ops/with-frees (mapv r/make-F args)
                                                (binding [uvar/*current-env* current-env
                                                          prs/*parse-type-in-ns* current-ns]
                                                  (mapv coll-clj/parse-field (partition 3 fields))))))
            ;FIXME unchecked ancestors
            ;as (set (free-ops/with-frees (mapv r/make-F args)
            ;          (binding [uvar/*current-env* current-env
            ;                    prs/*parse-type-in-ns* current-ns]
            ;            (mapv prs/parse-type ancests))))
            ;_ (ancest/add-datatype-ancestors s as)
            pos-ctor-name (symbol demunged-ns-str (str "->" local-name))
            map-ctor-name (symbol demunged-ns-str (str "map->" local-name))
            dt (c/DataType* args vs (map r/make-F args) s bnds fs record?)
            _ (dt-env/add-datatype s dt)
            pos-ctor (if args
                       (c/Poly* args bnds
                                (r/make-FnIntersection
                                  (r/make-Function (vec (vals fs)) (c/DataType-of s (map r/make-F args)))))
                       (r/make-FnIntersection
                         (r/make-Function (vec (vals fs)) (c/DataType-of s))))
            map-ctor (when record?
                       (let [hmap-arg (c/-hmap (zipmap (map (comp r/-val keyword) (keys fs))
                                                       (vals fs)))]
                         (if args
                           (c/Poly* args bnds
                                    (r/make-FnIntersection
                                      (r/make-Function [hmap-arg] (c/DataType-of s (map r/make-F args)))))
                           (r/make-FnIntersection
                             (r/make-Function [hmap-arg] (c/DataType-of s))))))]
        (do 
          (var-env/add-var-type pos-ctor-name pos-ctor)
          (var-env/add-nocheck-var pos-ctor-name)
          (when record?
            (var-env/add-var-type map-ctor-name map-ctor)))))))

(defmethod invoke-special-collect 'cljs.core.typed/ann-datatype*
  [{:keys [args env] :as expr}]
  (assert (= (count args) 4) "Wrong arguments to ann-datatype")
  (let [[binder dname fields opt] (map :form args)]
    (gen-datatype* env (chk/expr-ns expr) dname fields binder opt false)))

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
  (doall (map collect (concat statements [ret])))
  nil)

(defmethod collect :invoke
  [expr]
  (binding [uvar/*current-env* (:env expr)]
    (invoke-special-collect expr)))

(defmethod collect :default
  [expr]
  nil)

(defmethod invoke-special-collect :default
  [expr]
  nil)
