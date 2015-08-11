(ns ^:skip-wiki clojure.core.typed.collect-cljs
  (:require [cljs.core.typed :as t]
            [clojure.core.typed.collect.gen-protocol :as gen-protocol]
            [clojure.core.typed.collect.gen-datatype :as gen-datatype]
            [clojure.core.typed.analyze-cljs :as ana]
            [clojure.core.typed.type-rep :as r :refer [ret]]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.check.utils :as chk]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.util-vars :as uvar]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.name-env :as nme-env]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.protocol-env :as ptl-env]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.declared-kind-env :as decl]
            [clojure.core.typed.collect-utils :as clt-u]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.collect-phase :as coll-clj]
            [clojure.core.typed.datatype-ancestor-env :as ancest]
            [clojure.core.typed.datatype-env :as dt-env]
            [clojure.core.typed.base-env-helper-cljs :as beh-cljs]
            [clojure.repl :as repl]))

(alter-meta! *ns* assoc :skip-wiki true)

(declare collect)

(defn collect-asts [asts]
  (doseq [ast asts]
    (collect ast)))

(defn collect-ns
  "Collect type annotations and dependency information
  for namespace symbol nsym, and recursively check 
  declared typed namespace dependencies."
  [nsym]
  (clt-u/collect-ns*
    nsym
    {:ast-for-ns ana/ast-for-ns
     :collect-asts collect-asts
     :collect-ns collect-ns}))

(defmulti collect (fn [expr] (:op expr)))
(u/add-defmethod-generator collect)
(defmulti invoke-special-collect (fn [expr]
                                   (when (#{:var} (-> expr :f :op))
                                     (-> expr :f :info :name))))

(add-collect-method :def
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
                                                  (mapv gen-datatype/parse-field (partition 3 fields))))))
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
                       (let [hmap-arg (c/make-HMap :mandatory (zipmap (map (comp r/-val keyword) (keys fs))
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

(defmethod invoke-special-collect 'cljs.core.typed/ann-jsnominal*
  [{:keys [args env] :as expr}]
  (let [[sym jsnom] (map :form args)]
    (impl/add-jsnominal-env
      sym
      (second (beh-cljs/jsnominal-entry [sym jsnom])))))

(defmethod invoke-special-collect 'cljs.core.typed/ann-protocol*
  [{[{vbnd :form} {varsym :form} {mth :form} :as args] :args :keys [env] :as expr}]
  (assert (= (count args) 3) "Wrong arguments to ann-protocol")
  (gen-protocol/gen-protocol* env (chk/expr-ns expr) varsym vbnd mth))
  
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
      (when-not (sub/subtype? alias-type tfn) 
        (err/int-error (str "Declared kind " (prs/unparse-type tfn)
                            " does not match actual kind " (prs/unparse-type alias-type)))))
    nil))

; only collect invocations in arbitrarily nested `do` blocks
(add-collect-method :do
  [{:keys [statements ret]}]
  (doall (map collect (concat statements [ret])))
  nil)

(add-collect-method :invoke
  [expr]
  (binding [uvar/*current-env* (:env expr)]
    (invoke-special-collect expr)))

(add-collect-method :default
  [expr]
  nil)

(defmethod invoke-special-collect :default
  [expr]
  nil)
