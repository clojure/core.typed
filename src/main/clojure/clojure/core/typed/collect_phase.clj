(ns ^:skip-wiki clojure.core.typed.collect-phase
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.utils :as u :refer [p profile]]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.name-env :as nme-env]
            [clojure.core.typed.declared-kind-env :as decl]
            [clojure.core.typed.ns-deps :as dep]
            [clojure.core.typed.ns-deps-utils :as dep-u]
            [clojure.core.typed.ns-options :as ns-opts]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.util-vars :as uvar]
            [clojure.core.typed.check.utils :as chk]
            [clojure.core.typed.analyze-clj :as ana-clj]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.frees :as frees]
            [clojure.core.typed.datatype-ancestor-env :as ancest]
            [clojure.core.typed.rclass-env :as rcls]
            [clojure.core.typed.datatype-env :as dt-env]
            [clojure.core.typed.protocol-env :as ptl-env]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.method-override-env :as override]
            [clojure.core.typed.method-return-nilables :as ret-nil]
            [clojure.core.typed.method-param-nilables :as param-nil]
            [clojure.core.typed.subtype :as sub]
            [clojure.repl :as repl]
            [clojure.math.combinatorics :as comb]
            [clojure.tools.namespace.track :as track]
            [clojure.tools.namespace.dir :as dir]
            [clojure.tools.namespace.dependency :as ndep]
            [clojure.tools.namespace.parse :as ns-parse]
            [clojure.tools.namespace.file :as ns-file]
            [clojure.core :as core]))

(t/tc-ignore
(alter-meta! *ns* assoc :skip-wiki true)
)

(defn parse-field [[n _ t]]
  [n (prs/parse-type t)])

(declare collect)

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

(declare collect-asts)

(t/ann collect-ns [t/Sym -> nil])
(defn collect-ns
  "Collect type annotations and dependency information
  for namespace symbol nsym, and recursively check 
  declared typed namespace dependencies."
  ([nsym]
   {:pre [(symbol? nsym)]}
   (p :collect-phase/collect-ns
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
         (let [asts (p :collect-phase/get-clj-analysis (ana-clj/ast-for-ns nsym))]
           (p :collect/collect-form
              (collect-asts asts)))
         (println (str "Finished collecting " nsym))
         (flush))))))

(defn collect-ast [expr]
  (collect expr))

(defn collect-form [form]
  (let [ast (ana-clj/ast-for-form form)]
    (collect-ast ast)))

(defn collect-asts [asts]
  ; phase 1
  ; declare all protocols and datatypes
;  (doseq [ast asts]
;    (collect-declares ast))
  ; phase 2
  ; collect type annotations
  (doseq [ast asts]
    (collect ast)))

(defn collect-ns-setup [nsym]
  {:pre [(symbol? nsym)]}
  (binding [uvar/*already-collected* (atom #{})]
    (collect-ns nsym)))

(defn internal-form? [expr]
  (u/internal-form? expr ::t/special-collect))

(u/special-do-op ::t/special-collect internal-collect-expr)

(defmethod internal-collect-expr :default [& _])

(defn visit-do [{:keys [statements ret] :as expr} f]
  (when (internal-form? expr)
    (internal-collect-expr expr))
  (doseq [expr (concat statements [ret])]
    (f expr)))

(defmethod internal-collect-expr ::core/ns
  [{[_ _ {{ns-form :form} :val :as third-arg} :as statements] :statements fexpr :ret :as expr}]
  {:pre [ns-form
         ('#{clojure.core/ns ns} (first ns-form))]}
  (let [prs-ns (dep-u/ns-form-name ns-form)
        deps   (dep-u/ns-form-deps ns-form)
        tdeps (set (filter dep-u/should-check-ns? deps))]
    (dep/add-ns-deps prs-ns tdeps)
    (doseq [dep tdeps]
      (collect-ns dep))))

(defn assert-expr-args [{:keys [args] :as expr} cnts]
  {:pre [(set? cnts)]}
  (assert (cnts (count args)))
  (assert (every? #{:quote} (map :op args))
          (mapv :op args)))

(defmulti collect (fn [expr] (:op expr)))
(u/add-defmethod-generator collect)
(defmulti invoke-special-collect (fn [expr]
                                   (when-let [var (-> expr :fn :var)]
                                     (coerce/var->symbol var))))

(add-collect-method :do [expr] (visit-do expr collect))

(add-collect-method :def
  [{:keys [var env] :as expr}]
  (let [prs-ns (chk/expr-ns expr)]
    (let [mvar (meta var)
          qsym (coerce/var->symbol var)]
      (when-let [[_ tsyn] (find mvar :ann)]
        (let [ann-type (binding [uvar/*current-env* env
                                 prs/*parse-type-in-ns* prs-ns]
                         (prs/parse-type tsyn))]
          (var-env/add-var-type qsym ann-type)))
      (when (:no-check mvar)
        (var-env/add-nocheck-var qsym)))))

(add-collect-method :invoke
  [{:keys [env] :as expr}]
  (binding [uvar/*current-env* env]
    (invoke-special-collect expr)))

(add-collect-method :default
  [_]
  nil)

(defn gen-datatype* [current-env current-ns provided-name fields vbnd opt record?]
  {:pre [(symbol? current-ns)]}
  (impl/with-clojure-impl
    (let [{ancests :unchecked-ancestors} opt
          ancests (or ancests (:extends opt))
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
                                                  (mapv parse-field (partition 3 fields))))))
            as (set (free-ops/with-frees (mapv r/make-F args)
                      (binding [uvar/*current-env* current-env
                                prs/*parse-type-in-ns* current-ns]
                        (mapv (comp #(c/abstract-many args %) prs/parse-type) ancests))))
            ;_ (prn "collected ancestors" as)
            _ (ancest/add-datatype-ancestors s as)
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
                       (let [hmap-arg ; allow omission of keys if nil is allowed and field is monomorphic
                             (let [{optional true mandatory false} 
                                   (group-by (fn [[_ t]] (and (empty? (frees/fv t))
                                                              (empty? (frees/fi t))
                                                              (sub/subtype? r/-nil t)))
                                             (zipmap (map (comp r/-val keyword) (keys fs))
                                                     (vals fs)))]
                               (c/make-HMap :optional (into {} optional)
                                            :mandatory (into {} mandatory)))]
                         (if args
                           (c/Poly* args bnds
                                    (r/make-FnIntersection
                                      (r/make-Function [hmap-arg] (c/DataType-of s (map r/make-F args)))))
                           (r/make-FnIntersection
                             (r/make-Function [hmap-arg] (c/DataType-of s))))))]
        (do 
          ;(when vs
          ;  (let [f (mapv r/make-F (repeatedly (count vs) gensym))]
          ;    ;TODO replacements and unchecked-ancestors go here
          ;    (rcls/alter-class* s (c/RClass* (map :name f) vs f s {} {} bnds))))
          (var-env/add-var-type pos-ctor-name pos-ctor)
          (var-env/add-nocheck-var pos-ctor-name)
          (when record?
            (override/add-method-override (symbol (str s) "create") map-ctor)
            (var-env/add-var-type map-ctor-name map-ctor)))))))

(defmethod invoke-special-collect 'clojure.core.typed/ann-precord*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{4})
  (let [[dname vbnd fields opt] (ast-u/constant-exprs args)]
    (gen-datatype* env (chk/expr-ns expr) dname fields vbnd opt true)))

(defmethod invoke-special-collect 'clojure.core.typed/ann-pdatatype*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{4})
  (let [[dname vbnd fields opt] (ast-u/constant-exprs args)]
    (assert nil "REMOVED OPERATION: ann-pdatatype, use ann-datatype with binder as first argument, ie. before datatype name")
    #_(gen-datatype* env (chk/expr-ns expr) dname fields vbnd opt false)))

(defmethod invoke-special-collect 'clojure.core.typed/ann-datatype*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{4})
  (let [[binder dname fields opt] (ast-u/constant-exprs args)]
    (gen-datatype* env (chk/expr-ns expr) dname fields binder opt false)))

(defmethod invoke-special-collect 'clojure.core.typed/ann-record*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{4})
  (let [[binder dname fields opt] (ast-u/constant-exprs args)]
    (gen-datatype* env (chk/expr-ns expr) dname fields binder opt true)))

(defmethod invoke-special-collect 'clojure.core.typed/warn-on-unannotated-vars*
  [{:as expr}]
  (assert-expr-args expr #{0})
  (let [prs-ns (chk/expr-ns expr)]
    (ns-opts/register-warn-on-unannotated-vars prs-ns)
    nil))

(defmethod invoke-special-collect 'clojure.core.typed/typed-deps*
  [{:keys [args] :as expr}]
  (assert-expr-args expr #{1})
  (let [prs-ns (chk/expr-ns expr)
        [deps] (ast-u/constant-exprs args)
        _ (assert (and deps (seq deps) (every? symbol? deps)))]
    (if uvar/*already-collected*
      (do (dep/add-ns-deps prs-ns (set deps))
          (doseq [dep deps]
            (if (coerce/ns->URL dep)
              (collect-ns dep)
              (err/int-error (str "Cannot find dependency declared with typed-deps: " dep)))))
      (do (println "WARNING: Not collecting namespaces, must call typed-deps via check-ns")
          (flush)))
    nil))

(defmethod invoke-special-collect 'clojure.core.typed/declare-datatypes*
  [{:keys [args] :as expr}]
  (assert-expr-args expr #{1})
  (let [[syms] (ast-u/constant-exprs args)]
    (doseq [sym syms]
      (assert (not (or (some #(= \. %) (str sym))
                       (namespace sym)))
              (str "Cannot declare qualified datatype: " sym))
      (let [qsym (symbol (str (munge (name (ns-name *ns*))) \. (name sym)))]
        (nme-env/declare-datatype* qsym)))))

;(defmethod invoke-special-collect 'clojure.core.typed/declare-protocols*
;  [{:keys [args] :as expr}]
;  (assert-expr-args expr #{1})
;  (let [[syms] (ast-u/constant-exprs args)]
;    (doseq [sym syms]
;      (let [qsym (if (namespace sym)
;                   sym
;                   (symbol (str (name (ns-name *ns*))) (name sym)))]
;        (nme-env/declare-protocol* qsym)))))
;
;(defmethod invoke-special-collect 'clojure.core.typed/declare-protocols*
;  [{:keys [args env] :as expr}]
;  (assert-expr-args expr #{2})
;  (let [prs-ns (chk/expr-ns expr)
;        [sym tsyn] (ast-u/constant-exprs args)
;        _ (assert ((every-pred symbol? (complement namespace)) sym))
;        ty (binding [uvar/*current-env* env
;                     prs/*parse-type-in-ns* prs-ns]
;             (prs/parse-type tsyn))
;        qsym (symbol (-> prs-ns ns-name str) (str sym))]
;    (nme-env/declare-name* qsym)
;    (decl/declare-alias-kind* qsym ty)
;    nil))

(defmethod invoke-special-collect 'clojure.core.typed/declare-names*
  [{:keys [args] :as expr}]
  (assert-expr-args expr #{1})
  (let [nsym (chk/expr-ns expr)
        [syms] (ast-u/constant-exprs args)
        _ (assert (every? (every-pred symbol? (complement namespace)) syms)
                  "declare-names only accepts unqualified symbols")]
    (doseq [sym syms]
      (nme-env/declare-name* (symbol (str nsym) (str sym))))))

(defmethod invoke-special-collect 'clojure.core.typed/ann*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{3})
  (let [prs-ns (chk/expr-ns expr)
        [qsym typesyn check?] (ast-u/constant-exprs args)
        ;macroexpansion provides qualified symbols
        _ (assert ((every-pred symbol? namespace) qsym))
        expected-type (binding [uvar/*current-env* env
                                prs/*parse-type-in-ns* prs-ns]
                        (prs/parse-type typesyn))]
    (when-not check?
      (var-env/add-nocheck-var qsym))
    (var-env/add-var-type qsym expected-type)
    nil))

(defmethod invoke-special-collect 'clojure.core.typed/def-alias*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{2})
  (let [prs-ns (chk/expr-ns expr)
        _ (assert (symbol? prs-ns))
        [qsym typesyn] (ast-u/constant-exprs args)
        ;FIXME this is too complicated, should just work out qualification here
        ;macroexpansion provides qualified symbols
        _ (assert ((every-pred symbol? namespace) qsym))
        alias-type (binding [uvar/*current-env* env
                             prs/*parse-type-in-ns* prs-ns]
                     (prs/parse-type typesyn))]
    ;var already interned via macroexpansion
    (nme-env/add-type-name qsym alias-type)
    (when-let [tfn (decl/declared-kind-or-nil qsym)]
      (when-not (sub/subtype? alias-type tfn) 
        (err/int-error (str "Declared kind " (prs/unparse-type tfn)
                            " does not match actual kind " (prs/unparse-type alias-type)))))
    nil))

(defmethod invoke-special-collect 'clojure.core.typed/non-nil-return*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{2})
  (let [[msym arities] (ast-u/constant-exprs args)]
    (ret-nil/add-nonnilable-method-return msym arities)
    nil))

(defmethod invoke-special-collect 'clojure.core.typed/nilable-param*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{2})
  (let [[msym mmap] (ast-u/constant-exprs args)]
    (param-nil/add-method-nilable-param msym mmap)
    nil))

(defmethod invoke-special-collect 'clojure.core.typed/override-method*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{2})
  (let [prs-ns (chk/expr-ns expr)
        [msym tsyn] (ast-u/constant-exprs args)
        _ (assert (namespace msym) "Method symbol must be a qualified symbol")
        ty (binding [uvar/*current-env* env
                     prs/*parse-type-in-ns* prs-ns]
             (prs/parse-type tsyn))]
    (override/add-method-override msym ty)
    nil))

(defmethod invoke-special-collect 'clojure.core.typed/override-constructor*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{2})
  (let [prs-ns (chk/expr-ns expr)
        [msym tsyn] (ast-u/constant-exprs args)
        ty (binding [uvar/*current-env* env
                     prs/*parse-type-in-ns* prs-ns]
             (prs/parse-type tsyn))]
    (override/add-method-override msym ty)
    nil))

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

#_(defn parse-protocol-methods [mths]
  (into {} (for [[knq v] mths]
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
               [knq mtype]))))

#_(defn parse-protocol-binder [binder]
  (when binder 
    (binding [prs/*parse-type-in-ns* current-ns]
      (prs/parse-free-binder-with-variance binder))))

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

(defmethod invoke-special-collect 'clojure.core.typed/ann-protocol*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{3})
  (let [[binder varsym mth] (ast-u/constant-exprs args)]
    (gen-protocol* env (chk/expr-ns expr) varsym binder mth)))

(defmethod invoke-special-collect 'clojure.core.typed/ann-pprotocol*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{3})
  (let [[varsym binder mth] (ast-u/constant-exprs args)]
    (assert nil "UNSUPPPORTED OPERATION: ann-pprotocol, use ann-protocol with binder as first argument, ie. before protocol name")
    #_(gen-protocol* env (chk/expr-ns expr) varsym binder mth)))


(defmethod invoke-special-collect :default
  [expr]
  nil)
