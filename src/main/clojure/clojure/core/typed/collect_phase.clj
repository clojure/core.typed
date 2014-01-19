(ns ^:skip-wiki clojure.core.typed.collect-phase
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.utils :as u :refer [constant-exprs p profile]]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.name-env :as nme-env]
            [clojure.core.typed.declared-kind-env :as decl]
            [clojure.core.typed.ns-deps :as dep]
            [clojure.core.typed.ns-options :as ns-opts]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.util-vars :as uvar]
            [clojure.core.typed.check :as chk]
            [clojure.core.typed.analyze-clj :as ana-clj]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.free-ops :as free-ops]
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
            [clojure.tools.namespace.track :as track]
            [clojure.tools.namespace.dir :as dir]
            [clojure.tools.namespace.dependency :as ndep]))

(alter-meta! *ns* assoc :skip-wiki true)

(defonce ns-deps-tracker (atom (track/tracker)))

(defn update-ns-deps! []
  (p :collect/update-ns-deps!
  (try
    (swap! ns-deps-tracker dir/scan)
    (catch StackOverflowError e
      (prn "ERROR COLLECTING DEPENDENCIES: caught StackOverflowError from tools.namespace")))
  ))

(defn parse-field [[n _ t]]
  [n (prs/parse-type t)])

(declare collect)

(defn- collected-ns! [nsym]
  (if-let [a t/*already-collected*]
    (swap! a conj nsym)
    (assert nil "Type system is not set up for namespace collection")))

(defn- already-collected? [nsym]
  (if-let [a t/*already-collected*]
    (boolean (@a nsym))
    (assert nil "Type system is not set up for namespace collection")))

(defn immediate-deps [nsym]
  (ndep/immediate-dependencies (-> @ns-deps-tracker ::track/deps) nsym))

(defn directly-depends? [nsym another-nsym]
  (contains? (immediate-deps nsym) another-nsym))

(defn probably-typed? [nsym]
  (let [demunged (u/demunge-ns nsym)
        ns (or (find-ns demunged)
               (do (require demunged)
                   (find-ns demunged)))
        _ (assert ns (str "Namespace " nsym " not found"))
        {:keys [check] :as opts} (u/typed-ns-opts ns)]
    (or check
        (directly-depends? nsym 'clojure.core.typed))))

(defn infer-typed-ns-deps!
  "Automatically find other namespaces that are likely to
  be typed dependencies to the current ns."
  [nsym]
  (update-ns-deps!)
  (let [all-deps (immediate-deps nsym)
        new-deps (set (filter probably-typed? all-deps))]
    (dep/add-ns-deps nsym new-deps)))

(declare collect-asts)

(defn collect-ns
  "Collect type annotations and dependency information
  for namespace symbol nsym, and recursively check 
  declared typed namespace dependencies."
  ([nsym]
   (p :collect-phase/collect-ns
   (if (already-collected? nsym)
     (do #_(println (str "Already collected " nsym ", skipping"))
         #_(flush)
         nil)
     (do (collected-ns! nsym)
         (println (str "Start collecting " nsym))
         (flush)
         (infer-typed-ns-deps! nsym)
         (let [deps (dep/immediate-deps nsym)]
           ;(prn "collecting immediate deps for " nsym deps)
           (doseq [dep deps]
             ;(prn "collect:" dep)
             (collect-ns dep)))
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
  (binding [t/*already-collected* (atom #{})]
    (collect-ns nsym)))

(defn visit-do [{:keys [exprs] :as expr} f]
  (doseq [expr exprs]
    (f expr)))

(defn assert-expr-args [{:keys [args] :as expr} cnts]
  {:pre [(set? cnts)]}
  (assert (cnts (count args)))
  (assert (every? #{:constant :keyword :number :string :nil :boolean :empty-expr}
                  (map :op args))
          (mapv :op args)))

;; Phase 1

;(defmulti collect-declares :op)
;(u/add-defmethod-generator collect-declares)
;
;(defmulti invoke-special-collect-declares 
;  (fn [expr]
;    (when-let [var (-> expr :fexpr :var)]
;      (u/var->symbol var))))
;
;(defn declare-protocol [current-env current-ns vsym binder mths]
;  {:pre [(symbol? current-ns)]}
;  (let [s (if (namespace vsym)
;            (symbol vsym)
;            (symbol (str current-ns) (name vsym)))
;        on-class (c/Protocol-var->on-class s)
;        variances (when binder
;                    (map (fn [[_ & {:keys [variance]}]] variance) binder))
;
;(defmethod invoke-special-collect-declares 'clojure.core.typed/ann-protocol*
;  [{:keys [args env] :as expr}]
;  (assert-expr-args expr #{3})
;  (let [[binder varsym mth] (constant-exprs args)]
;    (declare-protocol env (chk/expr-ns expr) varsym binder mth)))
;
;
;(add-collect-method :do [expr] (visit-do expr collect))

;; Phase 2

(defmulti collect (fn [expr] (:op expr)))
(u/add-defmethod-generator collect)
(defmulti invoke-special-collect (fn [expr]
                                   (when-let [var (-> expr :fexpr :var)]
                                     (u/var->symbol var))))

(add-collect-method :do [expr] (visit-do expr collect))

(add-collect-method :def
  [{:keys [var env] :as expr}]
  (let [prs-ns (chk/expr-ns expr)]
    (let [mvar (meta var)
          qsym (u/var->symbol var)]
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
                       (let [hmap-arg (c/-hmap (zipmap (map (comp r/-val keyword) (keys fs))
                                                       (vals fs)))]
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
  (let [[dname vbnd fields opt] (constant-exprs args)]
    (gen-datatype* env (chk/expr-ns expr) dname fields vbnd opt true)))

(defmethod invoke-special-collect 'clojure.core.typed/ann-pdatatype*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{4})
  (let [[dname vbnd fields opt] (constant-exprs args)]
    (assert nil "REMOVED OPERATION: ann-pdatatype, use ann-datatype with binder as first argument, ie. before datatype name")
    #_(gen-datatype* env (chk/expr-ns expr) dname fields vbnd opt false)))

(defmethod invoke-special-collect 'clojure.core.typed/ann-datatype*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{4})
  (let [[binder dname fields opt] (constant-exprs args)]
    (gen-datatype* env (chk/expr-ns expr) dname fields binder opt false)))

(defmethod invoke-special-collect 'clojure.core.typed/ann-record*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{3})
  (let [[dname fields opt] (constant-exprs args)]
    (gen-datatype* env (chk/expr-ns expr) dname fields nil opt true)))

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
        [deps] (constant-exprs args)
        _ (assert (and deps (seq deps) (every? symbol? deps)))]
    (if t/*already-collected*
      (do (dep/add-ns-deps prs-ns (set deps))
          (doseq [dep deps]
            (collect-ns dep)))
      (do (println "WARNING: Not collecting namespaces, must call typed-deps via check-ns")
          (flush)))
    nil))

(defmethod invoke-special-collect 'clojure.core.typed/declare-datatypes*
  [{:keys [args] :as expr}]
  (assert-expr-args expr #{1})
  (let [[syms] (constant-exprs args)]
    (doseq [sym syms]
      (assert (not (or (some #(= \. %) (str sym))
                       (namespace sym)))
              (str "Cannot declare qualified datatype: " sym))
      (let [qsym (symbol (str (munge (name (ns-name *ns*))) \. (name sym)))]
        (nme-env/declare-datatype* qsym)))))

;(defmethod invoke-special-collect 'clojure.core.typed/declare-protocols*
;  [{:keys [args] :as expr}]
;  (assert-expr-args expr #{1})
;  (let [[syms] (constant-exprs args)]
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
;        [sym tsyn] (constant-exprs args)
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
        [syms] (constant-exprs args)
        _ (assert (every? (every-pred symbol? (complement namespace)) syms)
                  "declare-names only accepts unqualified symbols")]
    (doseq [sym syms]
      (nme-env/declare-name* (symbol (str nsym) (str sym))))))

(defmethod invoke-special-collect 'clojure.core.typed/ann*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{3})
  (let [prs-ns (chk/expr-ns expr)
        [qsym typesyn check?] (constant-exprs args)
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
        [qsym typesyn] (constant-exprs args)
        ;FIXME this is too complicated, should just work out qualification here
        ;macroexpansion provides qualified symbols
        _ (assert ((every-pred symbol? namespace) qsym))
        alias-type (binding [uvar/*current-env* env
                             prs/*parse-type-in-ns* prs-ns]
                     (prs/parse-type typesyn))]
    ;var already interned via macroexpansion
    (nme-env/add-type-name qsym alias-type)
    (when-let [tfn (decl/declared-kind-or-nil qsym)]
      (assert (sub/subtype? alias-type tfn) (u/error-msg "Declared kind " (prs/unparse-type tfn)
                                                         " does not match actual kind " (prs/unparse-type alias-type))))
    nil))

(defmethod invoke-special-collect 'clojure.core.typed/non-nil-return*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{2})
  (let [[msym arities] (constant-exprs args)]
    (ret-nil/add-nonnilable-method-return msym arities)
    nil))

(defmethod invoke-special-collect 'clojure.core.typed/nilable-param*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{2})
  (let [[msym mmap] (constant-exprs args)]
    (param-nil/add-method-nilable-param msym mmap)
    nil))

(defmethod invoke-special-collect 'clojure.core.typed/override-method*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{2})
  (let [prs-ns (chk/expr-ns expr)
        [msym tsyn] (constant-exprs args)
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
        [msym tsyn] (constant-exprs args)
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
                       (u/int-error "Protocol method should be unqualified"))
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
                   (u/int-error (str "Protocol method " knq " should be a possibly-polymorphic function intersection"
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
            (u/int-error
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
        ms (into {} (for [[knq v] mths]
                      (let [_ (when (namespace knq)
                                (u/int-error "Protocol method should be unqualified"))
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
                             (u/int-error (str "Protocol method " knq " should be a possibly-polymorphic function intersection"
                                               " taking at least one fixed argument: "
                                               (prs/unparse-type mtype)))))
                         [knq mtype])))
        ;_ (prn "collect protocol methods" (into {} ms))
        t (c/Protocol* (map :name fs) (map :variance parsed-binder) 
                       fs s on-class ms (map :bnd parsed-binder))]
    (ptl-env/add-protocol s t)
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
  (let [[binder varsym mth] (constant-exprs args)]
    (gen-protocol* env (chk/expr-ns expr) varsym binder mth)))

(defmethod invoke-special-collect 'clojure.core.typed/ann-pprotocol*
  [{:keys [args env] :as expr}]
  (assert-expr-args expr #{3})
  (let [[varsym binder mth] (constant-exprs args)]
    (assert nil "UNSUPPPORTED OPERATION: ann-pprotocol, use ann-protocol with binder as first argument, ie. before protocol name")
    #_(gen-protocol* env (chk/expr-ns expr) varsym binder mth)))


(defmethod invoke-special-collect :default
  [_]
  nil)
