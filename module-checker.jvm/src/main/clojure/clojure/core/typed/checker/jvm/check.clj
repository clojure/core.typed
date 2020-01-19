;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.jvm.check
  {:skip-wiki true}
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.analyzer.common :as ana2]
            [clojure.core.typed.analyzer.common.env :as env]
            [clojure.core.typed.analyzer.jvm :as jana2]
            [clojure.core.typed.analyzer.passes.beta-reduce :as beta-reduce]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.checker.check-below :as below]
            [clojure.core.typed.checker.check.apply :as apply]
            [clojure.core.typed.checker.check.binding :as binding]
            [clojure.core.typed.checker.check.case :as case]
            [clojure.core.typed.checker.check.catch :as catch]
            [clojure.core.typed.checker.check.cli :as cli]
            [clojure.core.typed.checker.check.const :as const]
            [clojure.core.typed.checker.check.def :as def]
            [clojure.core.typed.checker.check.do :as do]
            [clojure.core.typed.checker.check.fn :as fn]
            [clojure.core.typed.checker.check.fn-method-utils :as fn-method-u]
            [clojure.core.typed.checker.check.fn-methods :as fn-methods]
            [clojure.core.typed.checker.check.funapp :as funapp]
            [clojure.core.typed.checker.check.get :as get]
            [clojure.core.typed.checker.check.if :as if]
            [clojure.core.typed.checker.check.invoke :as invoke]
            [clojure.core.typed.checker.check.invoke-kw :as invoke-kw]
            [clojure.core.typed.checker.check.isa :as isa]
            [clojure.core.typed.checker.check.jvm.host-interop :as host-interop]
            [clojure.core.typed.checker.check.jvm.method :as method]
            [clojure.core.typed.checker.check.jvm.type-hints :as type-hints]
            [clojure.core.typed.checker.check.let :as let]
            [clojure.core.typed.checker.check.letfn :as letfn]
            [clojure.core.typed.checker.check.local :as local]
            [clojure.core.typed.checker.check.loop :as loop]
            [clojure.core.typed.checker.check.map :as map]
            [clojure.core.typed.checker.check.monitor :as monitor]
            [clojure.core.typed.checker.check.multi :as multi]
            [clojure.core.typed.checker.check.multi-utils :as multi-u]
            [clojure.core.typed.checker.check.nth :as nth]
            [clojure.core.typed.checker.check.nthnext :as nthnext]
            [clojure.core.typed.checker.check.print-env :as print-env]
            [clojure.core.typed.checker.check.quote :as quote]
            [clojure.core.typed.checker.check.recur :as recur]
            [clojure.core.typed.checker.check.recur-utils :as recur-u]
            [clojure.core.typed.checker.check.set :as set]
            [clojure.core.typed.checker.check.set-bang :as set!]
            [clojure.core.typed.checker.check.special.ann-form :as ann-form]
            [clojure.core.typed.checker.check.special.cast :as cast]
            [clojure.core.typed.checker.check.special.fn :as special-fn]
            [clojure.core.typed.checker.check.special.loop :as special-loop]
            [clojure.core.typed.checker.check.special.tc-ignore :as tc-ignore]
            [clojure.core.typed.checker.check.throw :as throw]
            [clojure.core.typed.checker.check.try :as try]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.checker.check.vector :as vec]
            [clojure.core.typed.checker.check.with-meta :as with-meta]
            [clojure.core.typed.checker.cs-gen :as cgen]
            [clojure.core.typed.checker.cs-rep :as crep]
            [clojure.core.typed.checker.datatype-ancestor-env :as ancest]
            [clojure.core.typed.checker.datatype-env :as dt-env]
            [clojure.core.typed.checker.dvar-env :as dvar-env]
            [clojure.core.typed.checker.filter-ops :as fo]
            [clojure.core.typed.checker.filter-rep :as fl]
            [clojure.core.typed.checker.fold-rep :as fold]
            [clojure.core.typed.checker.free-ops :as free-ops]
            [clojure.core.typed.checker.frees :as frees]
            [clojure.core.typed.checker.inst :as inst]
            [clojure.core.typed.checker.jvm.analyze-clj :as ana-clj]
            [clojure.core.typed.checker.jvm.array-ops :as arr-ops]
            [clojure.core.typed.checker.jvm.assoc-utils :as assoc-u]
            [clojure.core.typed.checker.jvm.constant-type :as constant-type]
            [clojure.core.typed.checker.jvm.ctor-override-env :as ctor-override]
            [clojure.core.typed.checker.jvm.mm-env :as mm]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.core.typed.checker.jvm.rclass-env :as rcls]
            [clojure.core.typed.checker.jvm.reflect-utils :as reflect-u]
            [clojure.core.typed.checker.jvm.subtype :as sub]
            [clojure.core.typed.checker.jvm.tc-equiv :as equiv]
            [clojure.core.typed.checker.lex-env :as lex]
            [clojure.core.typed.checker.ns-deps-utils :as ns-depsu]
            [clojure.core.typed.checker.ns-options :as ns-opts]
            [clojure.core.typed.checker.object-rep :as obj]
            [clojure.core.typed.checker.open-result :as open-result]
            [clojure.core.typed.checker.path-rep :as pe]
            [clojure.core.typed.checker.protocol-env :as ptl-env]
            [clojure.core.typed.checker.subst :as subst]
            [clojure.core.typed.checker.subst-obj :as subst-obj]
            [clojure.core.typed.checker.tvar-bnds :as tvar-bnds]
            [clojure.core.typed.checker.tvar-env :as tvar-env]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.update :as update]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.var-env :as var-env]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.rules :as rules]
            [clojure.core.typed.special-form :as spec]
            [clojure.core.typed.util-vars :as vs]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.repl :as repl]
            [clojure.set :as cljset]
            [clojure.tools.analyzer.ast :as ast]
            [clojure.tools.analyzer.jvm :as taj]
            [clojure.tools.analyzer.jvm.utils :as jtau]
            [clojure.tools.analyzer.passes.jvm.emit-form :as emit-form]
            [clojure.tools.analyzer.utils :as tau]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers])
  (:import (clojure.lang IPersistentMap Var Seqable)))

(t/ann ^:no-check clojure.core.typed.checker.jvm.parse-unparse/*unparse-type-in-ns* (t/U nil t/Sym))
(t/ann ^:no-check clojure.core.typed.util-vars/*already-checked* (t/U nil (t/Atom1 (t/Set t/Sym))))

;==========================================================
; # Type Checker
;
; The type checker is implemented here.

(declare check-expr)

(t/ann ^:no-check checked-ns! [t/Sym -> nil])
(defn- checked-ns! [nsym]
  (t/when-let-fail [a vs/*already-checked*]
    (swap! a conj nsym))
  nil)

(t/ann already-checked? [t/Sym -> Boolean])
(defn already-checked? [nsym]
  (t/when-let-fail [a vs/*already-checked*]
    (boolean (@a nsym))))

(declare check-ns-and-deps)

(defn check-deps
  ([nsym]
   (when (= :recheck (some-> vs/*check-config* deref :check-ns-dep))
     (checked-ns! nsym)
     ;check normal dependencies
     (doseq [dep (ns-depsu/deps-for-ns nsym)]
       ;; ensure namespace actually exists
       (when (ns-depsu/should-check-ns? nsym)
         (check-ns-and-deps dep))))))

(declare check-top-level)

(defn check-ns1
  "Type checks an entire namespace."
  ([ns] (check-ns1 ns (taj/empty-env)))
  ([ns env]
     (env/ensure (jana2/global-env)
       (let [^java.net.URL res (jtau/ns-url ns)]
         (assert res (str "Can't find " ns " in classpath"))
         (let [filename (str res)
               path     (.getPath res)]
           (binding [*ns*   *ns*
                     *file* filename]
             (with-open [rdr (io/reader res)]
               (let [pbr (readers/indexing-push-back-reader
                           (java.io.PushbackReader. rdr) 1 filename)
                     eof (Object.)
                     read-opts {:eof eof :features #{:clj}}
                     read-opts (if (.endsWith filename "cljc")
                                 (assoc read-opts :read-cond :allow)
                                 read-opts)
                     _ (case (some-> vs/*check-config* deref :type-check-eval)
                         (:simulate :interleave) nil
                         ; process the ns form as a "post-eval", since we
                         ; want the ns side effect to occur *after* we analyze+type check
                         ; the form.
                         :pre-eval
                         (let [ns-form (reader/read read-opts pbr)
                               _ (assert (and (seq? ns-form)
                                              (#{'ns} (first ns-form)))
                                         (str "First form of namespace '" ns "' must be "
                                              "an 'ns' form."))]
                           (check-top-level ns-form nil {:env (assoc env :ns (ns-name *ns*))})
                           (eval ns-form)))]
                 (loop []
                   (let [form (reader/read read-opts pbr)]
                     (when-not (identical? form eof)
                       (check-top-level form nil {:env (assoc env :ns (ns-name *ns*))})
                       (recur))))))))))))


(t/ann check-ns-and-deps [t/Sym -> nil])
(defn check-ns-and-deps
  "Type check a namespace and its dependencies."
  ([nsym]
   {:pre [(symbol? nsym)]
    :post [(nil? %)]}
   (let []
     (cond 
       (already-checked? nsym) (do
                                 ;(println (str "Already checked " nsym ", skipping"))
                                 ;(flush)
                                 nil)
       :else
       ; check deps
       (let [_ (check-deps nsym)]
         ; ignore ns declaration
         (let [ns-form (ns-depsu/ns-form-for-ns nsym)
               check? (boolean (some-> ns-form ns-depsu/should-check-ns-form?))]
           (if-not check?
             (when-not (#{'clojure.core.typed 'cljs.core.typed 'clojure.core 'cljs.core} nsym)
               (println (str "Not checking " nsym 
                             (cond
                               (not ns-form) " (ns form missing)"
                               (ns-depsu/collect-only-ns? ns-form) " (tagged :collect-only in ns metadata)"
                               (not (ns-depsu/requires-tc? ns-form)) " (does not depend on clojure.core.typed)")))
               (flush))
             (let [start (. System (nanoTime))
                   _ (println "Start checking" nsym)
                   _ (flush)
                   _ (check-ns1 nsym)
                   _ (println "Checked" nsym "in" (/ (double (- (. System (nanoTime)) start)) 1000000.0) "msecs")
                   _ (flush)
                   ]
         nil))))))))

(defn eval-top-level
  "Evaluate top-level AST's according to core.typed.analyzer's
  top-level information, while respecting the current :type-check-eval
  mode."
  [expr]
  (case (some-> vs/*check-config* deref :type-check-eval)
    :interleave (ana2/eval-top-level expr)
    :pre-eval expr
    :simulate (if-not (get-in expr [::ana2/config ::real-expr])
                ; an inner form, like `a` in `[a]`
                (do
                  #_
                  (prn "eval-top-level inner form"
                       (emit-form/emit-form expr))
                  expr)
                ; a top-level form, like `a` in top-level `(do a b c)`
                (cond
                  (#{:do} (:op expr))
                  (do (assert (contains? (:ret expr) :result))
                      #_
                      (prn "eval-top-level top-level do"
                           (emit-form/emit-form expr))
                      (merge expr
                             (select-keys (:ret expr) [:result])))

                  :else (let [real-expr (get-in expr [::ana2/config ::real-expr])
                              real-expr (ana2/eval-top-level real-expr)]
                          #_
                          (prn "fake-expr" (emit-form/emit-form expr)
                               (:op expr)
                               (find expr :result))
                          #_
                          (prn "real-expr" (emit-form/emit-form real-expr)
                               (find real-expr :result))
                          (assert (contains? real-expr :result)
                                  (str (:op real-expr)
                                       " "
                                       (ana2/eval-top-level? real-expr)
                                       " "
                                       (emit-form/emit-form real-expr)))
                          (merge expr
                                 (select-keys real-expr [:result])))
                ))))

(defn analyze-outer
  "Analyze the outermost AST node the equivalent of one macroexpansion,
  while respecting the current :type-check-eval mode."
  [expr]
  (case (some-> vs/*check-config* deref :type-check-eval)
    (:interleave :pre-eval) (ana2/analyze-outer expr)
    :simulate (case (:op expr)
                :unanalyzed
                (let [fake-expr (-> expr
                                    ana2/analyze-outer
                                    (#(binding [ana2/macroexpand-1 (impl/impl-case
                                                                     :clojure jana2/macroexpand-1
                                                                     :cljs (throw (Error. "TODO: macroexpand for cljs forms")))]
                                        (update-in % [::ana2/config ::real-expr] ana2/analyze-outer))))
                      real-expr (get-in fake-expr [::ana2/config ::real-expr])
                      ; keep real and fake subexpressions in sync while real is top-level
                      fake-expr (if (and (ana2/top-level? real-expr)
                                         (#{:do} (:op real-expr)))
                                  (do (assert (= (:op fake-expr)
                                                 (:op real-expr))
                                              (mapv (juxt :op emit-form/emit-form) [fake-expr real-expr]))
                                      ; recursively attach real subexpressions to corresponding fake subexpressions
                                      (-> fake-expr
                                          (update :statements
                                                  (fn [fake-statements]
                                                    {:pre [(vector? fake-statements)]}
                                                    (assert (every? :op fake-statements)
                                                            fake-statements)
                                                    (let [real-statements (:statements real-expr)]
                                                      (assert (= (count real-statements)
                                                                 (count fake-statements)))
                                                      (mapv (fn [fake-statement real-statement]
                                                              {:pre [(:op fake-statement)
                                                                     (:op real-statement)]} 
                                                              (assoc-in fake-statement [::ana2/config ::real-expr]
                                                                        real-statement))
                                                            fake-statements
                                                            real-statements))))
                                          (assoc-in [:ret ::ana2/config ::real-expr]
                                                    (:ret real-expr))))
                                  fake-expr)
                      _ (when (ana2/top-level? real-expr)
                          (assert (ana2/top-level? fake-expr)
                                  "Real and fake expansion must coincide: real is top-level, but fake is not")
                          (when (#{:do} (:op real-expr))
                            (assert (= (:op fake-expr)
                                       (:op real-expr))
                                    (str "Real and fake expansion must coincide: real is top-level :do, but fake is top-level "
                                         (or (:op fake-expr)
                                             "(unknown AST node)")))
                            (assert (= (count (:children fake-expr))
                                       (count (:children real-expr)))
                                    (str "Real and fake expansion must coincide: real is :do with "
                                         (count (:children real-expr))
                                         " children, but fake is top-level with"
                                         (count (:children fake-expr))
                                         " children."))))]
                  fake-expr)
                expr)))

(defn unanalyzed-top-level
  "Return an AST node for top-level form in env, while respecting
  the current :type-check-eval mode."
  [form env]
  (case (some-> vs/*check-config* deref :type-check-eval)
    (:interleave :pre-eval) (ana2/unanalyzed-top-level form env)
    :simulate (let [expr (ana2/unanalyzed-top-level form env)]
                (assoc-in expr [::ana2/config ::real-expr] expr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checker

(defmulti -check (fn [expr expected]
                  {:pre [((some-fn nil? r/TCResult?) expected)]}
                  (:op expr)))

(defn check-expr [expr & [expected]]
  {:pre [(:op expr)
         ((some-fn nil? r/TCResult?) expected)]
   :post [(r/TCResult? (u/expr-type %))]}
  ;(prn "check-expr" op)
  ;(clojure.pprint/pprint (emit-form/emit-form expr))
  (let [{:keys [op env] :as expr} (assoc-in expr [:env :ns] (ns-name *ns*))]
    (when vs/*trace-checker*
      (println "Checking line:" (:line env))
      (flush))
    (cond
      ;; TODO: replace this consequent with typing rule handlers
      ;; as in `clojure.core.typed.analyzer.jvm.gilardi-test/check-expr`
      ;; and use standard macroexpand-1. We have access to the expected type
      ;; here, so we can do turnstile-style typing rules where we return
      ;; an "untyped" expansion (ie., (lambda ..) input, (lambda- ..) output).
      (= :unanalyzed op) (let [cexpr (-> expr
                                         analyze-outer
                                         (check-expr expected))]
                           ; if still unanalyzed, this could be a top-level
                           ; form we chose not to traverse with the type checker.
                           ; So we call eval-top-level here for that case.
                           (if (= :unanalyzed (:op cexpr))
                             (eval-top-level cexpr)
                             cexpr))
      :else
      (let []
        (binding [vs/*current-env* (if (:line env) env vs/*current-env*)
                  vs/*current-expr* expr]
          (let [cexpr (-> expr
                          ana2/run-pre-passes
                          (-check expected)
                          ana2/run-post-passes
                          eval-top-level)]
            ;(prn "post" (:op cexpr))
            ;(clojure.pprint/pprint (emit-form/emit-form cexpr))
            cexpr))))))

(defn check-top-level
  ([form expected] (check-top-level form expected {}))
  ([form expected {:keys [env]
                   :as opts}]
  ;(prn "check-top-level" form)
  ;(prn "*ns*" *ns*)
  (with-bindings (dissoc (ana-clj/thread-bindings) #'*ns*) ; *ns* is managed by higher-level ops like check-ns1
    (env/ensure (jana2/global-env)
      (let [_ (case (some-> vs/*check-config* deref :type-check-eval)
                (:interleave :simulate) nil
                :pre-eval (eval form))
            res (-> form
                    (unanalyzed-top-level (or env (taj/empty-env)))
                    (check-expr expected))]
        res)))))

(defmethod -check :const [expr expected]
  (const/check-const constant-type/constant-type false expr expected))

(defmethod -check :quote [expr expected]
  (quote/check-quote check-expr constant-type/constant-type expr expected))

(defmethod -check :map
  [expr expected]
  (map/check-map check-expr expr expected))

(defmethod -check :set
  [expr expected]
  (set/check-set check-expr expr expected))

(defmethod -check :vector
  [expr expected]
  (vec/check-vector check-expr expr expected))

(defn should-infer-vars? [expr]
  (-> (cu/expr-ns expr)
      find-ns
      meta
      :core.typed
      :experimental
      (contains? :infer-vars)))

(defn check-var [{:keys [var] :as expr} expected]
  {:pre [(var? var)]}
  (binding [vs/*current-expr* expr]
    (let [id (coerce/var->symbol var)
          _ (when-not (var-env/used-var? id)
              (var-env/add-used-var id))
          vsym (coerce/var->symbol var)
          ut (var-env/get-untyped-var (cu/expr-ns expr) vsym)
          t (var-env/lookup-Var-nofail vsym)]
      ;(prn " annotation" t)
      ;(prn " untyped annotation" ut)
      (cond
        ;; we have an untyped annotation
        ut
        (if (cu/should-rewrite?)
          (assoc (cu/add-cast expr ut
                              {:positive (str "Annotation for " vsym)
                               :negative (str (cu/expr-ns expr))})
                 u/expr-type (below/maybe-check-below
                               (r/ret ut)
                               expected))
          (err/tc-delayed-error
            (str "Untyped var " id " found, but unable to rewrite to add contract"
            :return (assoc expr
                           u/expr-type (cu/error-ret expected)))))

        ;; we have a typed annotation
        t
        (assoc expr
               u/expr-type (below/maybe-check-below
                             (r/ret t)
                             expected))

        ;; :infer-vars are enabled for this namespace, this
        ;; var dereference is the dynamic type
        (or (should-infer-vars? expr)
            (impl/impl-case
              :clojure (= :unchecked 
                          (some-> vs/*check-config* deref :unannotated-var))
              :cljs nil))
        (do
          (println (str "Inferring " vsym " dereference as Unchecked"))
          (assoc expr
                 u/expr-type (below/maybe-check-below
                               (r/ret (r/-unchecked vsym))
                               expected)))
        (impl/impl-case
          :clojure (= :any (some-> vs/*check-config* deref :unannotated-var))
          :cljs nil)
        (do
          (println (str "Inferring " vsym " dereference as Any"))
          (assoc expr
                 u/expr-type (below/maybe-check-below
                               (r/ret r/-any)
                               expected)))


        :else
        (err/tc-delayed-error
          (str "Unannotated var " id
               "\nHint: Add the annotation for " id
               " via check-ns or cf")
          :return (assoc expr
                         u/expr-type (cu/error-ret expected)))))))

(defn set-erase-atoms [expr cred]
  {:pre [(u/expr-type cred)]}
  (let [_ (some-> expr ::with-meta/erase-atom (reset! true))
        _ (some-> expr ::replace-invoke-atom (reset! cred))]
    nil))

(defn ensure-within-beta-limit []
  (let [state vs/*beta-count*]
    (assert state)
    (if (< (:limit @state) (:count @state))
      (err/int-error
        (str "Exceeded the limit of symbolic beta reductions in a single form "
             "(" (:limit @state) ")"))
      (swap! state update :count inc))))

(defmethod -check :var
  [{:keys [var env] :as expr} expected]
  {:pre [(var? var)]}
  ;(prn " checking var" var)
  (or (when vs/*custom-expansions* 
        (when-let [args (::invoke-args expr)]
          (when-not expected
            (case (coerce/var->symbol var)
              ;; FIXME what if we break up final argument and it has an ann-form around it?
              ;; eg. (apply map [(ann-form identity [Any -> Any]))
              ;; eg. (apply map (ann-form [identity] (Seqable [Any -> Any])))
              ; moved to c.c.t.expand
              ;clojure.core/apply (when-let [red (beta-reduce/maybe-beta-reduce-apply
              ;                                    expr args
              ;                                    {:before-reduce ensure-within-beta-limit})]
              ;                     (let [cred (check-expr red (::invoke-expected expr))]
              ;                       (set-erase-atoms expr cred)
              ;                       cred))
              (let [vsym (ast-u/emit-form-fn expr)
                    form (with-meta (list* vsym (map ast-u/emit-form-fn args))
                                    (meta vsym))
                    mform (ana2/macroexpand-1 form env)]
                (when (not= form mform)
                  (ensure-within-beta-limit)
                  (let [cred (-> mform
                                 (ana2/analyze-form env)
                                 (update-in [:raw-forms] (fnil conj ())
                                            (vary-meta form assoc ::ana2/resolved-op (ana2/resolve-sym (first form) env)))
                                 ana2/run-passes
                                 (check-expr (::invoke-expected expr)))]
                    (set-erase-atoms expr cred)
                    cred)))))))
      (check-var expr expected)))

(defmethod -check :the-var
  [{:keys [^Var var env] :as expr} expected]
  {:pre [(var? var)]}
  (let [id (coerce/var->symbol var)
        macro? (.isMacro var)
        _ (when-not (or macro?
                        (var-env/used-var? id))
            (var-env/add-used-var id))
        t (var-env/lookup-Var-nofail id)
        t (cond
            t t
            macro? r/-any
            ;; :infer-vars are enabled for this namespace, this
            ;; var object is the dynamic type
            (should-infer-vars? expr) (r/-unchecked id)
            :else (err/tc-delayed-error (str "Untyped var reference: " id
                                           "\nHint: Add the annotation for " id
                                           " via check-ns or cf")
                                      :form (ast-u/emit-form-fn expr)
                                      :return (r/TCError-maker)))]
    (assoc expr
           u/expr-type (binding [vs/*current-expr* expr]
                         (below/maybe-check-below
                           (r/ret (c/RClass-of Var [t t])
                                  (fo/-true-filter))
                           expected)))))

(defmulti -invoke-special (fn [{fexpr :fn :keys [op] :as expr} & args] 
                            {:pre [(#{:invoke} op)]
                             :post [((some-fn nil? symbol?) %)]}
                            (when (#{:var} (:op fexpr))
                              (when-let [var (:var fexpr)]
                                (coerce/var->symbol var)))))

(defmulti -invoke-apply (fn [{[fexpr] :args :keys [op] :as expr} & args]
                          {:pre [(#{:invoke} op)]
                           :post [((some-fn nil? symbol?) %)]}
                          (when (#{:var} (:op fexpr))
                            (when-let [var (:var fexpr)]
                              (coerce/var->symbol var)))))

(defmulti -static-method-special (fn [expr & args]
                                   {:post [((some-fn nil? symbol?) %)]}
                                   (cu/MethodExpr->qualsym expr)))

(defn host-call-qname [expr]
  {:pre [(= :host-call (:op expr))]
   :post [((some-fn nil? symbol?) %)]}
  (when-let [tag (:tag (:target expr))]
    (let [tag (if (class? tag)
                (coerce/Class->symbol tag)
                tag)]
      (when (symbol? tag)
        (let [sym (symbol (str tag) (str (:method expr)))]
          sym)))))

(defmulti -host-call-special (fn [expr expected]
                               {:post [((some-fn nil? symbol?) %)]}
                               (host-call-qname expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyword lookups


; only handle special case that the first argument is literal class
(defmethod -invoke-special 'clojure.core/cast
  [{:keys [args] :as expr} & [expected]]
  {:post [(or (#{:default} %)
              (and (r/TCResult? (u/expr-type %))
                   (vector? (:args %))))]}
  (or (when (#{2} (count args))
        (let [cargs (mapv check-expr args)
              ct (-> (first cargs) u/expr-type r/ret-t c/fully-resolve-type)]
          (when (and (r/Value? ct) (class? (:val ct)))
            (let [v-t (-> (check-expr (second args)) u/expr-type r/ret-t)
                  t (c/In v-t (c/Un r/-nil (c/RClass-of-with-unknown-params (:val ct))))]
              (-> expr
                  (update-in [:fn] check-expr)
                  (assoc :args cargs
                         u/expr-type (below/maybe-check-below
                                       (r/ret t)
                                       expected)))))))
      :default))

(defmethod -invoke-special 'clojure.core.typed/var>*
  [expr & [expected]]
  {:post [(and (r/TCResult? (u/expr-type %))
               (vector? (:args %)))]}
  (when-not (#{1} (count (:args expr)))
    (err/int-error (str "Wrong number of arguments to clojure.core.typed/var>,"
                      " expected 1, given " (count (:args expr)))))
  (let [{[sym-expr :as args] :args fexpr :fn :as expr}
        (-> expr
            (update-in [:args 0] ana2/run-passes))
        sym (ast-u/quote-expr-val sym-expr)
        _ (assert (symbol? sym))
        t (var-env/lookup-Var-nofail sym)
        _ (when-not t
            (err/tc-delayed-error (str "Unannotated var: " sym)))]
    (-> expr
        ; var>* is internal, don't check
        #_(update-in [:fn] check-expr)
        (assoc u/expr-type (below/maybe-check-below
                             (r/ret (or t (r/TCError-maker)))
                             expected)))))

; ignore some keyword argument related intersections
(defmethod -invoke-special 'clojure.core/seq?
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  {:post [(and (r/TCResult? (u/expr-type %))
               (vector? (:args %)))]}
  (when-not (#{1} (count args))
    (err/int-error (str "Wrong number of arguments to clojure.core/seq?,"
                      " expected 1, given " (count args))))
  (let [cfexpr (check-expr fexpr)
        [ctarget :as cargs] (mapv check-expr args)]
    (cond 
      ; handle keyword args macroexpansion
      (r/KwArgsSeq? (-> ctarget u/expr-type r/ret-t))
      (assoc expr
             :fn cfexpr
             :args cargs
             u/expr-type (below/maybe-check-below
                           (r/ret r/-true (fo/-true-filter))
                           expected))
      ; records never extend ISeq
      (r/Record? (-> ctarget u/expr-type r/ret-t c/fully-resolve-type))
      (assoc expr
             :fn cfexpr
             :args cargs
             u/expr-type (below/maybe-check-below
                           (r/ret r/-false (fo/-false-filter))
                           expected))
      :else (invoke/normal-invoke check-expr expr fexpr args expected
                           :cargs cargs))))

(defmethod -invoke-special 'clojure.core/extend
  [expr & [expected]]
  {:post [(and (r/TCResult? (u/expr-type %))
               (vector? (:args %)))]}
  (when-not ((every-pred odd? pos?) (count (:args expr)))
    (err/int-error (str "Wrong number of arguments to extend, expected at least one with an even "
                        "number of variable arguments, given " (count (:args expr)))))
  (let [{[catype & protos :as args] :args :as expr}
        (-> expr
            ;atype
            (update-in [:args 0] check-expr))
        expr (-> expr
                 ; don't check extend
                 ;(update-in [:fn] check-expr)
                 (assoc u/expr-type (below/maybe-check-below
                                      (r/ret r/-nil (fo/-false-filter))
                                      expected)))
        ; this is a Value type containing a java.lang.Class instance representing
        ; the type extending the protocol, or (Value nil) if extending to nil
        target-literal-class (r/ret-t (u/expr-type catype))]
    (cond
      (not (and (r/Value? target-literal-class)
                ((some-fn class? nil?) (:val target-literal-class))))
      (err/tc-delayed-error
        (str "Must provide a Class or nil as first argument to extend, "
             "got " (pr-str (prs/unparse-type target-literal-class)))
        :return expr)

      (and expected (not (sub/subtype? r/-any (r/ret-t expected))))
      (do (cu/expected-error r/-any expected)
          expr)
      :else
      (let [; this is the actual core.typed type of the thing extending the protocol
            target-type (let [v (:val target-literal-class)]
                          (if (nil? v)
                            r/-nil
                            (c/RClass-of-with-unknown-params v)))
            ; build expected types for each method map
            extends (for [[prcl-expr mmap-expr] (partition 2 protos)]
                      (let [prcl-expr (ana2/run-pre-passes (ana2/analyze-outer-root prcl-expr))
                            protocol (do (when-not (= :var (:op prcl-expr))
                                           (err/int-error  "Must reference protocol directly with var in extend"))
                                         (ptl-env/resolve-protocol (coerce/var->symbol (:var prcl-expr))))
                            _ (when-not (r/Protocol? protocol)
                                (err/int-error (str "Expecting Protocol type, found " protocol)))
                            expected-mmap (c/make-HMap ;get all combinations
                                                       :optional
                                                       (into {}
                                                             (for [[msym mtype] (:methods protocol)]
                                                               [(r/-val (keyword (name msym))) 
                                                                (cu/extend-method-expected target-type mtype)])))]
                        {:expected-hmap expected-mmap
                         :prcl-expr prcl-expr
                         :mmap-expr mmap-expr}))
            cargs (vec
                    (cons catype
                          (mapcat
                            (fn [{:keys [mmap-expr expected-hmap prcl-expr]}]
                              (let [cprcl-expr (check-expr prcl-expr)
                                    cmmap-expr (check-expr mmap-expr (r/ret expected-hmap))]
                                [cprcl-expr cmmap-expr]))
                            extends)))
            _ (assert (== (count cargs)
                          (count args)))]
        (assoc expr
               :args cargs)))))

;into-array>
;
; Usage: (into-array> javat cljt coll)
;        (into-array> cljt coll)
(defmethod -invoke-special 'clojure.core.typed/into-array>*
  [{:keys [args] :as expr} & [expected]]
  {:post [(and (r/TCResult? (u/expr-type %))
               (vector? (:args %)))]}
  (when-not (#{2 3 4} (count args)) 
    (err/int-error "Wrong number of args to into-array>*"))
  (let [has-java-syn? (#{3 4} (count args))
        [javat-syn cljt-syn coll-expr]
        (cond 
          (= 3 (count args)) args
          (= 4 (count args)) (next args) ;handle temporary hacky case
          :else (cons nil args))

        javat-syn (some-> javat-syn ana2/run-passes)
        cljt-syn (some-> cljt-syn ana2/run-passes)
        javat (let [syn (or (when has-java-syn? (ast-u/quote-expr-val javat-syn))  ; generalise javat-syn if provided, otherwise cljt-syn
                            (ast-u/quote-expr-val cljt-syn))
                    c (-> 
                        (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
                          (prs/parse-type syn))
                        arr-ops/Type->array-member-Class)]
                (assert (class? c))
                c)
        cljt (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
               (prs/parse-type (ast-u/quote-expr-val cljt-syn)))
        ccoll (check-expr coll-expr (r/ret (c/Un r/-nil (c/RClass-of Seqable [cljt]))))]
    (-> expr
        ; into-array>* is internal, don't check it
        #_(update-in [:fn] check-expr)
        ; the coll is always last
        (assoc :args (-> args pop (conj ccoll))
               u/expr-type (below/maybe-check-below
                             (r/ret (r/PrimitiveArray-maker javat cljt cljt))
                             expected)))))

;not
(defmethod -invoke-special 'clojure.core/not
  [{:keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (when-not (= 1 (count args)) 
    (err/int-error "Wrong number of args to clojure.core/not"))
  (let [[ctarget :as cargs] (mapv check-expr args)
        {fs+ :then fs- :else} (-> ctarget u/expr-type r/ret-f)]
    (assoc expr
           :args cargs
           u/expr-type (below/maybe-check-below
                         (r/ret (prs/parse-type 'boolean) 
                                ;flip filters
                                (fo/-FS fs- fs+)
                                obj/-empty)
                         expected))))

;get
(defmethod -invoke-special 'clojure.core/get
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [cargs (mapv check-expr args)
        r (get/invoke-get expr expected :cargs cargs)]
    (if-not (#{cu/not-special} r)
      r
      (invoke/normal-invoke check-expr expr fexpr args expected
                     :cargs cargs))))

(defmethod -static-method-special 'clojure.lang.RT/get
  [{:keys [args] :as expr} & [expected]]
  {:pre [args]
   :post [(-> % u/expr-type r/TCResult?)]}
  (let [cargs (mapv check-expr args)
        r (get/invoke-get expr expected :cargs cargs)]
    (if-not (#{cu/not-special} r)
      r
      (method/check-invoke-method check-expr expr expected
                                  :cargs cargs))))

;FIXME should be the same as (apply hash-map ..) in invoke-apply
(defmethod -static-method-special 'clojure.lang.PersistentHashMap/create
  [{:keys [args] :as expr} & [expected]]
  {:post [(or (#{:default} %)
              (and (-> % u/expr-type r/TCResult?)
                   (vector? (:args %))))]}
  (binding [vs/*current-expr* expr]
    (let [_ (when-not (#{1} (count args)) 
              (err/int-error "Incorrect number of arguments to clojure.lang.PersistentHashMap/create"))
          cargs (mapv check-expr args)
          targett (-> (first cargs) u/expr-type r/ret-t)]
      (cond
        (r/KwArgsSeq? targett)
        (assoc expr
               :args cargs
               u/expr-type (below/maybe-check-below
                             (r/ret (c/KwArgsSeq->HMap targett))
                             expected))
        (r/HeterogeneousSeq? targett)
        (let [res (reduce (fn [t [kt vt]]
                            {:pre [(r/Type? t)]}
                            ;preserve bottom
                            (if (= (c/Un) vt)
                              vt
                              (do (assert (r/HeterogeneousMap? t))
                                  (assoc-in t [:types kt] vt))))
                          (c/-complete-hmap {}) (:types targett))]
          (assoc expr
                 :args cargs
                 u/expr-type (below/maybe-check-below
                               (r/ret res)
                               expected)))
        :else :default))))

(defmethod -check :prim-invoke
  [expr expected]
  (assoc (check-expr (assoc expr :op :invoke))
         :op :prim-invoke))

(defmethod -check :keyword-invoke
  [{kw :keyword :keys [target] :as expr} expected]
  {:pre [(and (#{:const} (:op kw))
              (keyword? (:val kw)))]
   :post [(r/TCResult? (u/expr-type %))]}
  (let [ckw (check-expr kw)
        ctarget (check-expr target)]
    (assoc expr
           :keyword ckw
           :target ctarget
           u/expr-type (invoke-kw/invoke-keyword
                         expr
                         (u/expr-type ckw)
                         (u/expr-type ctarget)
                         nil
                         expected))))

;; TODO refactor into own file
(defn protocol-invoke [check-fn {:keys [protocol-fn target args] :as expr} expected]
  (let [cprotocol-fn (check-fn protocol-fn)
        ctarget (check-fn target)
        cargs (mapv check-fn args)
        ftype (u/expr-type cprotocol-fn)
        argtys (map u/expr-type (concat [ctarget] cargs))
        actual (funapp/check-funapp cprotocol-fn (concat [ctarget] cargs) ftype argtys expected)]
    (assoc expr
           :target ctarget
           :protocol-fn cprotocol-fn
           :args cargs
           u/expr-type actual)))

(defmethod -check :protocol-invoke ; protocol methods
  [expr expected]
  (protocol-invoke check-expr expr expected))

;binding
;FIXME use `check-normal-def`
;FIXME record checked-var-def info
(defmethod -invoke-special 'clojure.core/push-thread-bindings
  [{[bindings-expr :as args] :args :as expr} & [expected]]
  {:post [((every-pred vector? #(= 1 (count %))) (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (when-not (= 1 (count args))
    (err/int-error (str "push-thread-bindings expected one argument, given " (count args))))
  (let [bindings-expr (ana2/run-pre-passes (ana2/analyze-outer-root bindings-expr))
        bindings-expr (if (#{:invoke} (-> bindings-expr :op))
                        (update bindings-expr :fn (comp ana2/run-pre-passes ana2/analyze-outer-root))
                        bindings-expr)
        ; only support (push-thread-bindings (hash-map @~[var bnd ...]))
        ; like `binding`s expansion
        _ (when-not (and (#{:invoke} (-> bindings-expr :op))
                         (#{#'hash-map} (-> bindings-expr :fn :var))
                         (even? (count (-> bindings-expr :args))))
            (err/nyi-error (str "Can only check push-thread-bindings with a well-formed call to hash-map as first argument"
                                " (like bindings expansion)")))
        new-bindings-exprs (partition 2 (-> bindings-expr :args))
        cargs
        [(assoc bindings-expr
                :args
                (vec
                  (apply concat
                         (for [[var-expr bnd-expr] new-bindings-exprs]
                           (let [{:keys [op var] :as var-expr} (ana2/run-pre-passes (ana2/analyze-outer-root var-expr))]
                             (when-not (#{:the-var} op)
                               (err/int-error (str "push-thread-bindings must have var literals for keys")))
                             (let [expected (var-env/type-of (coerce/var->symbol var))
                                   cvar-expr (check-expr var-expr)
                                   cexpr (check-expr bnd-expr (r/ret expected))
                                   actual (-> cexpr u/expr-type r/ret-t)]
                               (when (not (sub/subtype? actual expected))
                                 (err/tc-delayed-error (str "Expected binding for "
                                                          (coerce/var->symbol var)
                                                          " to be: " (prs/unparse-type expected)
                                                          ", Actual: " (prs/unparse-type actual))))
                               [cvar-expr cexpr]))))))]]
    (-> expr
        ; push-thread-bindings is unannotated
        #_(update-in [:fn] check-expr)
        (assoc :args cargs
               u/expr-type (below/maybe-check-below
                             (r/ret r/-nil)
                             expected)))))

(defn typing-rule-opts [expr]
  {:post [(map? %)]}
  (let [opts (:form (nth (:statements expr) 2))]
    (assert (and (seq? opts)
                 (= 2 (count opts))
                 (#{'quote} (first opts)))
            (str "Options of typing rule must be a quoted map literal, "
                 "found: " (pr-str opts)))
    ; (quote {...})
    (second opts)))

(def typing-rule-expr-kw :ret)

(defn invoke-typing-rule
  [vsym {:keys [env] :as expr} expected]
  ;(prn "invoke-typing-rule" vsym)
  (let [unparse-type-verbose #(binding [vs/*verbose-types* false]
                                (prs/unparse-type %))
        subtype? (fn [s t]
                   (let [s (prs/parse-type s)
                         t (prs/parse-type t)]
                     (sub/subtype? s t)))
        solve (fn [t q]
                {:pre [(map? t)
                       (contains? t :type)]
                 :post [((some-fn nil? map?) %)]}
                (let [;; atm only support query = (All [x+] [in :-> out])
                      query (prs/parse-type q)
                      _ (assert (r/Poly? query))
                      names (c/Poly-fresh-symbols* query)
                      bbnds (c/Poly-bbnds* names query)
                      body (c/Poly-body* names query)
                      _ (assert (r/FnIntersection? body))
                      _ (assert (= 1 (count (:types body))))
                      arity (first (:types body))
                      _ (assert (r/Function? arity))
                      _ (assert (= 1 (count (:dom arity))))
                      _ (assert (not-any? #(% arity) [:rest :drest :kws :prest :pdot]))
                      _ (assert (= (fo/-simple-filter) (:fl (:rng arity))))
                      _ (assert (= obj/-empty (:o (:rng arity))))
                      _ (assert (= (r/-flow fl/-top) (:flow (:rng arity))))

                      lhs (prs/parse-type (:type t))
                      rhs (first (:dom arity))
                      out (:t (:rng arity))
                      substitution (cgen/handle-failure
                                     (cgen/infer
                                       (zipmap names bbnds)
                                       {}
                                       [lhs]
                                       [rhs]
                                       out))]
                  (when substitution
                    {:type (unparse-type-verbose
                             (subst/subst-all substitution out))})))
        #_#_
        solve-subtype (fn [vs f]
                        {:pre [(apply distinct? vs)
                               (every? symbol? vs)]}
                        (let [gvs (map gensym vs)
                              gvs->vs (zipmap gvs vs)
                              syns (apply f gvs)
                              [lhs rhs] (tvar-env/with-extended-tvars gvs
                                          (mapv prs/parse-type syns))
                              substitution
                              (cgen/handle-failure
                                (cgen/infer
                                  (zipmap gvs (repeat r/no-bounds))
                                  {}
                                  [lhs]
                                  [rhs]
                                  r/-any))]
                          (when substitution
                            (into {}
                                  (comp (filter (comp crep/t-subst? val))
                                        (map (fn [[k v]]
                                               [(gvs->vs k)
                                                (unparse-type-verbose (:type v))])))
                                  (select-keys substitution gvs)))))
        with-updated-locals (fn [locals f]
                              (let [locals (zipmap (map prs/uniquify-local (keys locals))
                                                   (map prs/parse-type (vals locals)))]
                                (lex/with-locals locals
                                  (f))))
        rule-args {:vsym vsym
                   :opts (typing-rule-opts expr)
                   :expr (typing-rule-expr-kw expr)
                   :locals (:locals env)
                   :expected (some-> expected cu/TCResult->map)
                   ;:uniquify-local prs/uniquify-local
                   :with-updated-locals with-updated-locals
                   :maybe-check-expected (fn [actual expected]
                                           {:pre [(map? actual)
                                                  ((some-fn nil? map?) expected)]
                                            :post [(map? %)]}
                                           (->
                                             (below/maybe-check-below
                                               (cu/map->TCResult actual)
                                               (cu/maybe-map->TCResult expected))
                                             cu/TCResult->map))
                   :check (fn check-fn
                            ([expr] (check-fn expr nil))
                            ([expr expected]
                             {:pre [((some-fn nil? map?) expected)]}
                             (let [ret (some-> expected cu/map->TCResult)
                                   cexpr (check-expr expr ret)]
                               (assoc cexpr ::rules/expr-type (cu/TCResult->map (u/expr-type cexpr))))))
                   ;:solve-subtype solve-subtype
                   :solve solve
                   :subtype? subtype?
                   :emit-form ast-u/emit-form-fn
                   :abbreviate-type (fn [t]
                                      (let [m (prs/parse-type t)]
                                        (binding [vs/*verbose-types* false]
                                          (prs/unparse-type m))))
                   :delayed-error (fn [s opts]
                                    (let [opts (update opts :expected cu/maybe-map->TCResult)
                                          opts (if (contains? opts :actual)
                                                 (update opts :actual prs/parse-type)
                                                 opts)]
                                      (apply err/tc-delayed-error s (apply concat opts))))
                   :expected-error (fn [s t opts]
                                     (let [s (prs/parse-type s)
                                           t (cu/map->TCResult t)
                                           opts (-> opts
                                                    (update :expected cu/map->TCResult))]
                                       (apply cu/expected-error s t (apply concat opts))))
                   :internal-error (fn [s opts]
                                     ;; TODO args
                                     (let [opts (update opts :expected cu/maybe-map->TCResult)]
                                       (apply err/int-error s (apply concat opts))))}

        {out-expr-type ::rules/expr-type :as cexpr} (rules/typing-rule rule-args)
        out-tcresult (cu/map->TCResult out-expr-type)]
    (-> expr
        (assoc u/expr-type out-tcresult)
        (assoc typing-rule-expr-kw (-> cexpr
                                       (dissoc ::rules/expr-type)
                                       (assoc u/expr-type out-tcresult))))))

;=
(defmethod -invoke-special 'clojure.core/= 
  [{:keys [args] :as expr} & [expected]]
  {:post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (let [cargs (mapv check-expr args)]
    (-> expr
        (update-in [:fn] check-expr)
        (assoc :args cargs
               u/expr-type (equiv/tc-equiv := (map u/expr-type cargs) expected)))))

;identical
(defmethod -static-method-special 'clojure.lang.Util/identical
  [{:keys [args] :as expr} & [expected]]
  {:post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (let [cargs (mapv check-expr args)]
    (assoc expr
           :args cargs
           u/expr-type (equiv/tc-equiv :identical? (map u/expr-type cargs) expected))))

;equiv
(defmethod -static-method-special 'clojure.lang.Util/equiv
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (mapv check-expr args)]
    (assoc expr
           :args cargs
           u/expr-type (equiv/tc-equiv := (map u/expr-type cargs) expected))))

;isa? (2 arity is special)
(defmethod -invoke-special 'clojure.core/isa?
  [{:keys [args] :as expr} & [expected]]
  (cond
    (#{2} (count args))
    (let [[cchild-expr cparent-expr :as cargs] (mapv check-expr args)]
      (-> expr
          (update-in [:fn] check-expr)
          (assoc :args cargs
                 u/expr-type (isa/tc-isa? (u/expr-type cchild-expr)
                                          (u/expr-type cparent-expr)
                                          expected))))
    :else :default))

(declare maybe-special-apply)

(defn check-apply [expr expected]
  (let [expr (-> expr
                 (update-in [:args 0] (comp ana2/run-pre-passes ana2/analyze-outer-root)))
        e (-invoke-apply expr expected)
        e (if (= e cu/not-special)
            (maybe-special-apply check-expr expr expected)
            e)]
    (if (= e cu/not-special)
      :default
      e)))

;FIXME need to review if any repeated "check"s happen between -invoke-apply and specials
;apply
(defmethod -invoke-special 'clojure.core/apply
  [{:keys [args env] :as expr} & [expected]]
  ;(prn "special apply:")
  (or (when vs/*custom-expansions*
        (when (<= 2 (count args))
          (let [[f & args] args
                [fixed rst] ((juxt pop peek) (vec args))]
            (when-let [splice (beta-reduce/splice-seqable-expr rst)]
              (let [min-count (apply + (map :min-count splice))
                    max-count (apply + (map :max-count splice))
                    ordered? (:ordered (first splice))
                    max-realized (max min-count max-count)]
                (when (and ordered?
                           (< max-realized 15))
                  (let [gsym (gensym 'args)
                        form `(let* [~gsym ~(ast-u/emit-form-fn rst)]
                                (~(ast-u/emit-form-fn f) ~@fixed ~@(map (fn [i]
                                                                          `(first (nthnext ~gsym ~i)))
                                                                        (range max-realized))))]
                    (-> form
                        (ana2/analyze-form env)
                        ana2/run-passes
                        (check-expr expected)))))))))
      (check-apply expr expected)))


;TODO this should be a special :do op
;manual instantiation
(defmethod -invoke-special 'clojure.core.typed/inst-poly
  [expr & [expected]]
  (when-not (#{2} (count (:args expr)))
    (err/int-error "Wrong arguments to inst"))
  (let [{[pexpr targs-exprs :as args] :args :as expr}
        (-> expr
            (update-in [:args 0] check-expr)
            (update-in [:args 1] ana2/run-passes))
        ptype (c/fully-resolve-type (-> pexpr u/expr-type r/ret-t))
        ; support (inst :kw ...)
        ptype (if (c/keyword-value? ptype)
                (c/KeywordValue->Fn ptype)
                ptype)]
    (if-not ((some-fn r/Poly? r/PolyDots?) ptype)
      (binding [vs/*current-expr* pexpr]
        (err/tc-delayed-error (str "Cannot instantiate non-polymorphic type: " (prs/unparse-type ptype))
                            :return (assoc expr
                                           u/expr-type (cu/error-ret expected))))
      (let [[targs-syn kwargs] (split-with (complement keyword?) (ast-u/quote-expr-val targs-exprs))
            _ (when-not (even? (count kwargs))
                (err/int-error (str "Expected an even number of keyword options to inst, given: " (vec kwargs))))
            _ (when (seq kwargs)
                (when-not (apply distinct? (map first (partition 2 kwargs)))
                  (err/int-error (str "Gave repeated keyword args to inst: " (vec kwargs)))))
            {:keys [named] :as kwargs} kwargs
            _ (let [unsupported (cljset/difference (set (keys kwargs)) #{:named})]
                (when (seq unsupported)
                  (err/int-error (str "Unsupported keyword argument(s) to inst " unsupported))))
            _ (when (contains? kwargs :named)
                (when-not (and (map? named)
                               (every? symbol? (keys named)))
                  (err/int-error (str ":named keyword argument to inst must be a map of symbols to types, given: " (pr-str named)))))
            named (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)
                            vs/*current-expr* expr]
                    (into {}
                          (map (fn [[k v]]
                                 [k (prs/parse-type v)]))
                          named))
            targs (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)
                            vs/*current-expr* expr]
                    (mapv prs/parse-type targs-syn))]
        (assoc expr
               u/expr-type (below/maybe-check-below
                             (r/ret 
                               (binding [prs/*unparse-type-in-ns* (cu/expr-ns expr)
                                         vs/*current-expr* expr]
                                 (inst/manual-inst ptype targs named)))
                             expected))))))

(defonce ^:dynamic *inst-ctor-types* nil)
(t/tc-ignore
(set-validator! #'*inst-ctor-types* (some-fn nil? (con/every-c? r/Type?)))
)

;TODO this should be a special :do op
;manual instantiation for calls to polymorphic constructors
(defmethod -invoke-special 'clojure.core.typed/inst-poly-ctor
  [expr & [expected]]
  (let [{[ctor-expr targs-exprs] :args :as expr} (-> expr
                                                     (update-in [:args 1] ana2/run-passes))
        targs (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
                (mapv prs/parse-type (ast-u/quote-expr-val targs-exprs)))
        cexpr (binding [*inst-ctor-types* targs]
                (check-expr ctor-expr))]
    (-> expr 
        (assoc-in [:args 0] cexpr)
        (assoc u/expr-type (u/expr-type cexpr)))))

;debug printing
(defmethod -invoke-special 'clojure.core.typed/print-env
  [expr & [expected]]
  (when-not (#{1} (count (:args expr)))
    (err/int-error (str "Wrong arguments to print-env, Expected 1, found " (count (:args expr)))))
  (let [{[debug-string :as args] :args :as expr} (-> expr
                                                     (update-in [:args 0] ana2/run-passes))]
    (when-not (= :const (:op debug-string))
      (err/int-error "Must pass print-env a string literal"))
    ;DO NOT REMOVE
    (println (:val debug-string))
    (flush)
    (prs/with-unparse-ns (cu/expr-ns expr)
      (print-env/print-env*))
    ;DO NOT REMOVE
    (assoc expr
           u/expr-type (below/maybe-check-below
                         (r/ret r/-nil (fo/-false-filter) obj/-empty)
                         expected))))

;filter printing
(defmethod -invoke-special 'clojure.core.typed/print-filterset
  [expr & [expected]]
  (when-not (#{2} (count (:args expr)))
    (err/int-error (str "Wrong arguments to print-filterset. Expected 2, found " (count (:args expr)))))
  (let [{[debug-string form :as args] :args :as expr} (-> expr
                                                          (update-in [:args 0] ana2/run-passes))
        _ (when-not (= :const (:op debug-string)) 
            (err/int-error "Must pass print-filterset a string literal as the first argument."))
        cform (check-expr form expected)
        cargs (assoc args 1 cform)
        t (u/expr-type cform)]
    ;DO NOT REMOVE
    (println (:val debug-string))
    (flush)
    ;(prn (:fl t))
    (prs/with-unparse-ns (cu/expr-ns expr)
      (if (fl/FilterSet? (:fl t))
        (do (pprint/pprint (prs/unparse-filter-set (:fl t)))
            (flush))
        (prn (:fl t)))
      (prn (prs/unparse-object (:o t)))
      (prn 'Flow (prs/unparse-filter (-> t :flow r/flow-normal))))
    ;DO NOT REMOVE
    (assoc expr
           :args cargs
           u/expr-type t)))

;unchecked casting
(defmethod -invoke-special 'clojure.core.typed.unsafe/ignore-with-unchecked-cast*
  [expr & [expected]]
  (when-not (#{2} (count (:args expr)))
    (err/int-error (str "Wrong arguments to ignore-with-unchecked-cast Expected 2, found " (count (:args expr)))))
  (let [{[_frm_ quote-expr] :args, :keys [env], :as expr} (-> expr
                                                              ;; all args are ignored by type checker
                                                              (update :args #(mapv ana2/run-passes %)))
        tsyn (ast-u/quote-expr-val quote-expr)
        parsed-ty (binding [vs/*current-env* env
                            prs/*parse-type-in-ns* (cu/expr-ns expr)]
                    (prs/parse-type tsyn))]
    (assoc expr
           u/expr-type (below/maybe-check-below
                         (r/ret parsed-ty)
                         expected))))

;pred
(defmethod -invoke-special 'clojure.core.typed/pred*
  [expr & [expected]]
  (when-not (#{3} (count (:args expr)))
    (err/int-error (str "Wrong arguments to pred Expected 3, found " (count (:args expr)))))
  (let [{[tsyn-expr nsym-expr _pred-fn_ :as args] :args, :keys [env], :as expr}
        (-> expr
            (update-in [:args 0] ana2/run-passes)
            (update-in [:args 1] ana2/run-passes))
        tsyn (ast-u/quote-expr-val tsyn-expr)
        nsym (ast-u/quote-expr-val nsym-expr)
        ptype 
        ; frees are not scoped when pred's are parsed at runtime,
        ; so we simulate the same here.
        (binding [tvar-env/*current-tvars* {}
                  dvar-env/*dotted-scope* {}]
          (prs/with-parse-ns nsym
            (prs/parse-type tsyn)))]
    (assoc expr
           u/expr-type (below/maybe-check-below
                         (r/ret (prs/predicate-for ptype))
                         expected))))

;seq
(defmethod -invoke-special 'clojure.core/seq
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (let [_ (assert (#{1} (count args))
                  "Wrong number of arguments to seq")
        [ccoll :as cargs] (mapv check-expr args)]
    ;(prn "special seq: ccoll type" (prs/unparse-type (r/ret-t (u/expr-type ccoll))))
    (cond
      ; for (apply hash-map (seq kws)) macroexpansion of keyword args
      (r/KwArgsSeq? (r/ret-t (u/expr-type ccoll)))
      (assoc expr
             :args cargs
             u/expr-type (u/expr-type ccoll))

      :else 
      (let [r (nthnext/check-seq check-expr expr expected :cargs cargs)]
        (if-not (#{cu/not-special} r)
          r
          (invoke/normal-invoke check-expr expr fexpr args expected :cargs cargs))))))

;make vector
(defmethod -invoke-special 'clojure.core/vector
  [{:keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (let [cargs (mapv check-expr args)]
    (-> expr
        (update-in [:fn] check-expr)
        (assoc 
          :args cargs
          u/expr-type (below/maybe-check-below
                        (r/ret (r/-hvec (mapv (comp r/ret-t u/expr-type) cargs)
                                        :filters (mapv (comp r/ret-f u/expr-type) cargs)
                                        :objects (mapv (comp r/ret-o u/expr-type) cargs)))
                        expected)))))

;(defmethod -invoke-special 'clojure.core/hash-map
;  [{fexpr :fn :keys [args] :as expr} & [expected]]
;  {:post [(-> % u/expr-type r/TCResult?)
;          (vector? (:args %))]}
;  (let [cargs (mapv check-expr args)]
;    (cond
;      (every? r/Value? (keys (apply hash-map (mapv (comp r/ret-t u/expr-type) cargs))))
;      (-> expr
;        (update-in [:fn] check-expr)
;        (assoc :args cargs
;               u/expr-type (r/ret (c/-complete-hmap
;                                (apply hash-map (mapv (comp r/ret-t u/expr-type) cargs))))))
;      :else (invoke/normal-invoke check-expr expr fexpr args expected :cargs cargs))))

;(apply concat hmap)
(defmethod -invoke-apply 'clojure.core/concat
  [{[_concat-fn_ & args] :args :as expr} & [expected]]
  {:post [(or (and (-> % u/expr-type r/TCResult?)
                   (vector? (:args %)))
              (= % cu/not-special))]}
  (let [cargs (mapv check-expr args)
        tmap (when (#{1} (count cargs))
               (c/fully-resolve-type (r/ret-t (u/expr-type (last cargs)))))]
    (binding [vs/*current-expr* expr]
      (cond
        (r/HeterogeneousMap? tmap)
        (let [r (c/HMap->KwArgsSeq tmap false)]
          (-> expr
              (update-in [:fn] check-expr)
              (assoc u/expr-type (below/maybe-check-below
                                   (r/ret r (fo/-true-filter))
                                   expected))))
        :else cu/not-special))))

;apply hash-map
(defmethod -invoke-apply 'clojure.core/hash-map
  [{[fn-expr & args] :args :as expr} & [expected]]
  {:post [(or 
            (and (-> % u/expr-type r/TCResult?)
                 (vector? (:args %)))
            (= % cu/not-special))]}
  (let [cargs (mapv check-expr args)]
    ;(prn "apply special (hash-map): ")
    (cond
      (and (#{1} (count cargs))
           (r/KwArgsSeq? (u/expr-type (last cargs))))
      (-> expr
          (update-in [:fn] check-expr)
          ;; FIXME add annotation for hash-map to check fn-expr
          (assoc :args (vec (concat [fn-expr] cargs))
                 u/expr-type (below/maybe-check-below
                               (r/ret (c/KwArgsSeq->HMap (-> (u/expr-type (last cargs)) r/ret-t)))
                               expected)))

      (and (seq cargs)
           (r/HSequential?  (r/ret-t (u/expr-type (last cargs))))
           ;; every key must be a Value
           (let [kvs (vec
                       (concat (map (comp r/ret-t u/expr-type) (butlast cargs))
                               (mapcat vector (:types (r/ret-t (u/expr-type (last cargs)))))))]
             (and (even? (count kvs))
                  (every? r/Value? (keys (apply hash-map kvs))))))
      (-> expr
          (update-in [:fn] check-expr)
          ;; FIXME add annotation for hash-map to check fn-expr
          (assoc :args (vec (concat [fn-expr] cargs))
                 u/expr-type (below/maybe-check-below
                               (r/ret (c/-complete-hmap
                                        (apply hash-map (concat (map (comp r/ret-t u/expr-type) (butlast cargs))
                                                                (mapcat vector (:types (r/ret-t (u/expr-type (last cargs)))))))))
                               expected)))
      :else cu/not-special)))


;nth
(defmethod -static-method-special 'clojure.lang.RT/nth
  [{:keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (when-not (#{2 3} (count args))
    (err/int-error (str "'nth' accepts 2 or 3 arguments, found "
                        (count args))))
  (let [cargs (mapv check-expr args)
        r (nth/invoke-nth check-expr expr expected :cargs cargs)]
    (if-not (#{cu/not-special} r)
      r
      (method/check-invoke-method check-expr expr expected
                                  :cargs cargs))))

(defmethod -invoke-special 'clojure.core/nth
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (when-not (#{2 3} (count args))
    (err/int-error (str "'nth' accepts 2 or 3 arguments, found "
                        (count args))))
  (let [cargs (mapv check-expr args)
        r (nth/invoke-nth check-expr expr expected :cargs cargs)]
    (if-not (#{cu/not-special} r)
      r
      (invoke/normal-invoke check-expr expr fexpr args expected :cargs cargs))))

;nthnext
(defmethod -invoke-special 'clojure.core/nthnext
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (when-not (= 2 (count args))
    (err/int-error (str "'nthnext' accepts 2 arguments, found "
                        (count args))))
  (let [cargs (mapv check-expr args)
        r (nthnext/check-nthnext check-expr expr expected :cargs cargs)]
    (if-not (#{cu/not-special} r)
      r
      (invoke/normal-invoke check-expr expr fexpr args expected :cargs cargs))))

;next
(defmethod -invoke-special 'clojure.core/next
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (when-not (= 1 (count args))
    (err/int-error (str "'next' accepts 1 argument, found "
                        (count args))))
  (let [cargs (mapv check-expr args)
        r (nthnext/check-next check-expr expr expected :cargs cargs)]
    (if-not (#{cu/not-special} r)
      r
      (invoke/normal-invoke check-expr expr fexpr args expected :cargs cargs))))

;rest
(defmethod -invoke-special 'clojure.core/rest
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (when-not (= 1 (count args))
    (err/int-error (str "'rest' accepts 1 argument, found "
                        (count args))))
  (let [cargs (mapv check-expr args)
        r (nthnext/check-rest check-expr expr expected :cargs cargs)]
    (if-not (#{cu/not-special} r)
      r
      (invoke/normal-invoke check-expr expr fexpr args expected :cargs cargs))))

;cons
(defmethod -invoke-special 'clojure.core/cons
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  {:post [(or (#{cu/not-special} %)
              (-> % u/expr-type r/TCResult?))]}
  (when-not (= 2 (count args))
    (err/int-error (str "'cons' accepts 2 arguments, found "
                        (count args))))
  (if vs/*custom-expansions*
    (let [cargs (mapv check-expr args)
          ]
      ;;TODO
      #_(assert nil "Implement heterogeneous Cons for every? inlining")
      cu/not-special
      )
    cu/not-special))

(defn first-result [t]
  {:pre [(r/Type? t)]
   :post [((some-fn nil? r/Result?) %)]}
  (let [ftype (fn ftype [t]
                {:pre [(r/Type? t)]
                 :post [((some-fn nil? r/Result?) %)]}
                (let [t (c/fully-resolve-type t)]
                  (cond
                    (r/Union? t) (let [ts (mapv ftype (:types t))]
                                   (when (every? identity ts)
                                     (apply c/union-Results ts)))
                    (r/Intersection? t) (when-let [ts (seq (keep ftype (:types t)))]
                                          (apply c/intersect-Results ts))
                    (r/Nil? t) (r/make-Result r/-nil (fo/-false-filter))
                    (r/HSequential? t) (cond
                                         (seq (:types t))
                                         (r/make-Result (first (:types t))
                                                        (first (:fs t))
                                                        (first (:objects t)))

                                         (:rest t) (r/make-Result (c/Un r/-nil (:rest t)))
                                         (:drest t) (r/make-Result r/-any)

                                         (empty? (:types t)) (r/make-Result (r/ret r/-nil (fo/-false-filter)))))))]
    (ftype (nthnext/seq-type t))))

;first
(defmethod -invoke-special 'clojure.core/first
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  {:post [(or (#{cu/not-special} %)
              (-> % u/expr-type r/TCResult?))]}
  (when-not (= 1 (count args))
    (err/int-error (str "'first' accepts 1 argument, found "
                        (count args))))
  (if vs/*custom-expansions*
    (let [[coll :as cargs] (mapv check-expr args)
          ct (r/ret-t (u/expr-type coll))
          fres (first-result ct)]
      (if fres
        (assoc expr
               :args cargs
               u/expr-type (r/Result->TCResult fres))
        cu/not-special))
    cu/not-special))

;assoc
(defmethod -invoke-special 'clojure.core/assoc
  [{:keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [[target & keyvals] args

        _ (when-not (<= 3 (count args))
            (err/int-error (str "assoc accepts at least 3 arguments, found "
                                     (count args))))
        _ (when-not (even? (count keyvals))
            (err/int-error "assoc accepts an even number of keyvals"))

        ctarget (check-expr target)
        targetun (-> ctarget u/expr-type r/ret-t)
        ckeyvals (mapv check-expr keyvals)
        keypair-types (partition 2 (map u/expr-type ckeyvals))
        cargs (into [ctarget] ckeyvals)]
    (if-let [new-hmaps (apply assoc-u/assoc-type-pairs targetun keypair-types)]
      (-> expr
        (update-in [:fn] check-expr)
        (assoc
          :args cargs
          u/expr-type (below/maybe-check-below
                        (r/ret new-hmaps
                               (fo/-true-filter)) ;assoc never returns nil
                        expected)))
      
      ;; to do: improve this error message
      (err/tc-delayed-error (str "A call to assoc failed to type check with target expression of type:\n\t" (prs/unparse-type targetun)
                                 "\nand key/value pairs of types: \n\t"
                                 (clojure.string/join " " (map (comp pr-str prs/unparse-type :t u/expr-type) ckeyvals)))
                            ;; first argument is to blame, gather any blame information from there
                            :expected (u/expr-type ctarget)
                            :return (-> expr
                                        (update-in [:fn] check-expr)
                                        (assoc
                                          :args cargs
                                          u/expr-type (cu/error-ret expected)))))))

(defmethod -invoke-special 'clojure.core/dissoc
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  {:post [(or (= % :default) (-> % u/expr-type r/TCResult?))]}
  (let [_ (when-not (seq args)
            (err/int-error (str "dissoc takes at least one argument, given: " (count args))))
        [ctarget & cdissoc-args :as cargs] (mapv check-expr args)
        ttarget (-> ctarget u/expr-type r/ret-t)
        targs (map u/expr-type cdissoc-args)]
    (if-let [new-t (assoc-u/dissoc-keys ttarget targs)]
      (-> expr
          (update-in [:fn] check-expr)
          (assoc
            :args cargs
            u/expr-type (below/maybe-check-below
                          (r/ret new-t)
                          expected)))
      (invoke/normal-invoke check-expr expr fexpr args expected
                     :cargs cargs))))

; merge
(defmethod -invoke-special 'clojure.core/merge
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (let [[ctarget & cmerge-args :as cargs] (mapv check-expr args)
        basemap (-> ctarget u/expr-type r/ret-t c/fully-resolve-type)
        targs (map u/expr-type cmerge-args)]
    (if-let [merged (apply assoc-u/merge-types basemap targs)]
      (-> expr
          (update-in [:fn] check-expr)
          (assoc :args cargs
                 u/expr-type (below/maybe-check-below
                               (r/ret merged
                                      (fo/-true-filter) ;assoc never returns nil
                                      obj/-empty)
                               expected)))
      (invoke/normal-invoke check-expr expr fexpr args expected
                     :cargs cargs))))

;conj
(defmethod -invoke-special 'clojure.core/conj
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  (let [[ctarget & cconj-args :as cargs] (mapv check-expr args)
        ttarget (-> ctarget u/expr-type r/ret-t)
        targs (map u/expr-type cconj-args)]
    (if-let [conjed (apply assoc-u/conj-types ttarget targs)]
      (-> expr
          (update-in [:fn] check-expr)
          (assoc :args cargs
                 u/expr-type (below/maybe-check-below
                               (r/ret conjed
                                      (fo/-true-filter) ; conj never returns nil
                                      obj/-empty)
                               expected)))
      (invoke/normal-invoke check-expr expr fexpr args expected
                     :cargs cargs))))

#_(defmethod -invoke-special 'clojure.core/update-in
  [{:keys [args env] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (binding [vs/*current-expr* expr
            vs/*current-env* env]
    (let [error-expr (-> expr
                         (update-in [:fn] check-expr)
                         ;;TODO does this need below/maybe-check-below?
                         (assoc u/expr-type (r/ret (r/TCError-maker))))]
      (cond
        (not (< 3 (count args))) (err/tc-delayed-error (str "update-in takes at least 3 arguments"
                                                          ", actual " (count args))
                                                     :return error-expr)

        :else
        (let [[ctarget-expr cpath-expr cfn-expr & more-exprs] (doall (map check-expr args))
              path-type (-> cpath-expr u/expr-type r/ret-t c/fully-resolve-type)]
          (if (not (HeterogeneousVector? path-type))
            (err/tc-delayed-error (str "Can only check update-in with vector as second argument")
                                :return error-expr)
            (let [path (:types path-type)
                  follow-path (reduce (fn [t pth]
                                        (when t
                                          ))
                                      (-> ctarget-expr u/expr-type r/ret-t)
                                      path)]
              (assert nil))))))))
        

(comment
  (method-expected-type (prs/parse-type '[Any -> Any])
                        (prs/parse-type '(Value :op))
                        (prs/parse-type '(Value :if)))
  ;=> ['{:if Any} -> Any]
  )

; cli
;TODO add cargs to result
(defmethod -invoke-special 'clojure.tools.cli/cli
  [{[args-expr & specs-exprs] :args :keys [env] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (binding [vs/*current-env* env]
    (let [args-expected-ty (prs/parse-type `(t/U nil (clojure.lang.Seqable String)))
          cargs-expr (binding [vs/*current-env* (:env args-expr)]
                       (check-expr args-expr))
          _ (when-not (sub/subtype? 
                        (-> cargs-expr u/expr-type r/ret-t)
                        args-expected-ty)
              (binding [vs/*current-env* (:env args-expr)]
                (cu/expected-error (-> cargs-expr u/expr-type r/ret-t) (r/ret args-expected-ty))))
          spec-map-ty (reduce (fn [t spec-expr]
                                (if-let [[keyt valt] (cli/parse-cli-spec check-expr spec-expr)]
                                  (-> t
                                    (assoc-in [:types keyt] valt))
                                  ; resort to a general type
                                  (do
                                    ;(prn "cli: giving up because of" (ast-u/emit-form-fn spec-expr)
                                         ;"\n" spec-expr)
                                    (reduced 
                                      (c/RClass-of IPersistentMap [(c/RClass-of clojure.lang.Keyword) r/-any])))))
                              (c/-complete-hmap {})
                              specs-exprs)

          actual (r/-hvec [spec-map-ty 
                           (prs/parse-type `(clojure.lang.Seqable String))
                           (prs/parse-type `String)])
          _ (when expected
              (when-not (sub/subtype? actual (r/ret-t expected))
                (cu/expected-error 
                  actual expected)))
          cargs (vec (cons cargs-expr specs-exprs))]
      (-> expr
        (update-in [:fn] check-expr)
        (assoc :args cargs
               u/expr-type (below/maybe-check-below
                             (r/ret actual)
                             expected))))))

(defmethod -invoke-special 'clojure.core.typed.checker.check.utils/special-typed-expression
  [expr & [expected]]
  (let [_ (assert (= 1 (count (:args expr))))
        {[type-expr] :args :keys [env] :as expr} (-> expr
                                                     (update-in [:args 0] ana2/run-passes))
        _ (assert (= :quote (:op type-expr)))
        _ (assert (= :const (-> type-expr :expr :op))
                  (-> type-expr :expr :op))
        t (prs/parse-type (-> type-expr :expr :val))]
    (assoc expr
           u/expr-type (below/maybe-check-below
                         (r/ret t)
                         expected))))

; FIXME this needs a line number from somewhere!
(defmethod -host-call-special 'clojure.lang.MultiFn/addMethod
  [expr expected]
  {:post [#_(every? :post-done (cons (:target %) (:args %)))]}
  (when-not (= 2 (count (:args expr)))
    (err/int-error "Wrong arguments to clojure.lang.MultiFn/addMethod"))
  (let [{[dispatch-val-expr _] :args target :target :keys [env] :as expr}
        (-> expr
            ; don't think this is needed because check-host-call already runs run-passes
            ; on :target
            (update :target (comp ana2/run-pre-passes ana2/analyze-outer-root)))
        _ (when-not (#{:var} (:op target))
            (err/int-error "Must call addMethod with a literal var"))
        var (:var target)
        _ (assert (var? var))
        mmsym (coerce/var->symbol var)
        expr (assoc expr
                    u/expr-type (binding [vs/*current-expr* expr]
                                  (below/maybe-check-below
                                    (r/ret (c/RClass-of clojure.lang.MultiFn))
                                    expected)))
        default? (cu/default-defmethod? var (ast-u/emit-form-fn dispatch-val-expr))
        unannotated-def (some-> vs/*check-config* deref :unannotated-def)]
    (cond
      (and (= :unchecked unannotated-def)
           (not (var-env/lookup-Var-nofail mmsym)))
      (-> expr
          (update :target ana2/run-passes)
          (update-in [:args 0] ana2/run-passes)
          (update-in [:args 1] ana2/run-passes))

      ;skip if warn-on-unannotated-vars is in effect
      (or (and (ns-opts/warn-on-unannotated-vars? (cu/expr-ns expr))
               (not (var-env/lookup-Var-nofail mmsym)))
          (not (var-env/check-var? mmsym)))
      (do (u/tc-warning (str "Not checking defmethod " mmsym " with dispatch value: " 
                             (pr-str (ast-u/emit-form-fn dispatch-val-expr))))
          (-> expr
              (update :target ana2/run-passes)
              (update-in [:args 0] ana2/run-passes)
              (update-in [:args 1] ana2/run-passes)))
      :else
      (let [{[dispatch-val-expr method-expr] :args :as expr}
            (-> expr
                (update :target check-expr)
                (update-in [:args 0] check-expr)
                (update-in [:args 1] (comp ana2/run-pre-passes ana2/analyze-outer-root)))
            _ (assert (#{:var} (:op target)))
            _ (when-not (#{:fn} (:op method-expr))
                (err/int-error (str "Method must be a fn")))
            dispatch-type (mm/multimethod-dispatch-type mmsym)]
        (if-not dispatch-type
          (binding [vs/*current-env* env]
            (err/tc-delayed-error (str "Multimethod requires dispatch type: " mmsym
                                       "\n\nHint: defmulti must be checked before its defmethods")
                                  :return (-> expr
                                              (update-in [:args 1] ana2/run-passes))))
          (let [method-expected (var-env/type-of mmsym)
                cmethod-expr 
                (binding [multi-u/*current-mm* 
                          (when-not default?
                            {:dispatch-fn-type dispatch-type
                             :dispatch-val-ret (u/expr-type dispatch-val-expr)})]
                  (check-expr method-expr (r/ret method-expected)))]
            (-> expr
                (assoc-in [:args 1] cmethod-expr))))))))

(defmethod -invoke-special :default [& args] :default)
(defmethod -static-method-special :default [& args] :default)
(defmethod -host-call-special :default [& args] :default)

(defn maybe-special-apply [check-expr expr expected]
  (let [t (apply/check-apply check-expr expr expected)]
    (if (= t cu/not-special)
      t
      (assoc expr
             u/expr-type t))))

;;TODO attach new :args etc.
;;convert apply to normal function application
(defmethod -invoke-apply :default 
  [expr & [expected]]
  cu/not-special)

(defn visit-tail-pos [ast f]
  (let [rec #(visit-tail-pos % f)]
    (case (:op ast)
      :do (update ast :ret rec)
      ;; how do we ensure both branches reduce?
      :if ast #_(-> ast
              (update :then rec)
              (update :else rec))
      (:let :letfn) (update ast :body rec)
      :with-meta (update ast :expr rec)
      (f ast))))

(def expected-visitor (Exception.))

(defn check-invoke [expr expected]
  (let [{fexpr :fn :keys [args env] :as expr} (update expr :fn (comp ana2/run-pre-passes ana2/analyze-outer))
        e (-invoke-special expr expected)]
    (cond
      (not= :default e) e
      :else
      (let [cfexpr (check-expr fexpr)]
        (cond
          (c/keyword-value? (r/ret-t (u/expr-type cfexpr)))
          (let [[ctarget cdefault :as cargs] (mapv check-expr args)]
            (assert (<= 1 (count args) 2))
            (assoc expr
                   :fn cfexpr
                   :args cargs
                   u/expr-type (invoke-kw/invoke-keyword 
                                 expr
                                 (u/expr-type cfexpr)
                                 (u/expr-type ctarget)
                                 (when cdefault
                                   (u/expr-type cdefault)) 
                                 expected)))

          :else (invoke/normal-invoke check-expr expr fexpr args expected :cfexpr cfexpr))))))

(defmethod -check :invoke
  [{fexpr :fn :keys [args env] :as expr} expected]
  {:post [(r/TCResult? (u/expr-type %))
          (if (= :invoke (:op %))
            (vector? (:args %))
            true)
          #_(-> % :fn u/expr-type r/TCResult?)]}
  ;(prn "invoke:" ((some-fn :var :keyword :op) fexpr))
  (binding [vs/*current-env* env
            vs/*current-expr* expr]
    (or #_(when vs/*custom-expansions*
          (let [replace-atom (atom nil)
                fexpr (visit-tail-pos fexpr
                                      (fn [{:keys [op] :as ast}]
                                        (cond-> ast
                                          (#{:var :fn} op)
                                          (assoc ::invoke-args args
                                                 ::invoke-expected expected
                                                 ::replace-invoke-atom replace-atom))))
                cfexpr (check-expr fexpr)]
            (prn "in invoke" (some-> expr ast-u/emit-form-fn))
            (prn "replace-atom"
                 (some-> @replace-atom ast-u/emit-form-fn))
            ;; instead of inserting cfexpr directly, we instead erase any
            ;; wrapping forms around the fexpr.
            ;; eg. so
            ;;     ((ann-form inc [Int :-> Int]) 1)
            ;;     ;=> (inc 1)
            ;;     not
            ;;     ;=> (ann-form (inc 1) [Int :-> Int])
            @replace-atom))
        (check-invoke expr expected))))

(defn check-rest-fn [remain-dom & {:keys [rest drest kws prest pdot]}]
  {:pre [((some-fn nil? r/Type?) rest)
         ((some-fn nil? r/Type?) prest)
         ((some-fn nil? r/DottedPretype?) drest)
         ((some-fn nil? r/Type?) pdot)
         ((some-fn nil? r/KwArgs?) kws)
         (#{0 1} (count (filter identity [rest drest kws prest pdot])))
         (every? r/Type? remain-dom)]
   :post [(r/Type? %)]}
  (cond
    (or rest drest
        (zero? (count (filter identity [rest drest kws prest pdot]))))
    ; rest argument is always a nilable non-empty seq. Could
    ; be slightly more clever here if we have a `rest`, but what about
    ; `drest`?
    (c/Un r/-nil 
          (c/In (r/-hseq remain-dom
                         :rest rest
                         :drest drest)
                (r/make-CountRange 1)))

    (seq remain-dom) (err/nyi-error "Rest parameter with remaining fixed domain for prest/post/KwArgs")

    prest (c/Un r/-nil prest)
    pdot (c/Un r/-nil pdot)

    ;kws
    :else (c/KwArgs->Type kws)))

(defmacro prepare-check-fn [env expr & body]
  `(let [env# ~env
         expr# ~expr]
     (binding [vs/*current-env* (if (:line env#) env# vs/*current-env*)
               vs/*current-expr* expr#
               ;; TODO pass with normal fn argument
               fn-method-u/*check-fn-method1-checkfn* check-expr
               fn-method-u/*check-fn-method1-rest-type* check-rest-fn]
       ~@body)))

(defmethod -check :fn
  [{:keys [env] :as expr} expected]
  {:pre [((some-fn nil? r/TCResult?) expected)]
   :post [(-> % u/expr-type r/TCResult?)
          (if (= :fn (:op %))
            (vector? (:methods %))
            true)]}
  ;(prn "check :fn" expected)
  (or #_(when vs/*custom-expansions*
        ;; try to beta-expand
        (when-not (:local expr) ;; no recursive functions
          (when-let [args (::invoke-args expr)]
            (if expected
              ; expand ((ann-form (fn* [params*] body) [P* :-> R]) args*)
              ; to     (ann-form body[(ann-form args* P*)/params*] R)
              (let [[t :as ts] (fn-methods/function-types (:t expected))]
                (when (= 1 (count ts))
                  (let [[fin inst-frees bnds poly?] (cu/unwrap-poly (first ts))]
                    (when-not poly?
                      (when (r/FnIntersection? fin)
                        (when-let [matching-method (beta-reduce/find-matching-method expr (count args))]
                          (let [[ft :as relevant-fn-types] (keep #(fn-methods/expected-for-method matching-method % (:methods expr))
                                                                 (:types fin))]
                            (when (= 1 (count relevant-fn-types))
                              (let [{:keys [dom rng]} ft]
                                (when (not-any? #(% ft) [:drest :kws :prest :pdot])
                                  (assert ((if (:rest ft) <= =) (count dom) (count args)))
                                  (when-let [red (beta-reduce/maybe-beta-reduce-fn
                                                   expr
                                                   (mapv (fn [t a]
                                                           (binding [vs/*verbose-types* true]
                                                             (-> `(t/ann-form ~(ast-u/emit-form-fn a) ~(prs/unparse-type t))
                                                                 (ana2/analyze-form env)
                                                                 ana2/run-passes)))
                                                         (concat dom (repeat (:rest ft)))
                                                         args)
                                                   {:before-reduce ensure-within-beta-limit})]
                                    (let [cred (check-expr red (below/maybe-check-below
                                                                 ;; TODO subst arguments in object in result
                                                                 (r/Result->TCResult rng)
                                                                 (::invoke-expected expr)))]
                                      (set-erase-atoms expr cred)
                                      cred))))))))))))
              ; expand ((fn* [params*] body) args*)
              ; to     body[args*/params*]
              (when-let [red (beta-reduce/maybe-beta-reduce-fn expr args
                                                               {:before-reduce ensure-within-beta-limit})]
                (let [cred (check-expr red (::invoke-expected expr))]
                  (set-erase-atoms expr cred)
                  cred))))))
      (prepare-check-fn env expr
                        (if expected
                          (fn/check-fn expr expected)
                          (if r/enable-symbolic-closures?
                            (assoc expr u/expr-type (r/ret (r/symbolic-closure expr vs/*lexical-env*)))
                            (special-fn/check-core-fn-no-expected check-expr expr))))))

;(ann internal-special-form [Expr (U nil TCResult) -> Expr])
(u/special-do-op spec/special-form internal-special-form)

(defmethod internal-special-form ::t/tc-ignore
  [expr expected]
  (if (#{:simulate} (some-> vs/*check-config* deref :type-check-eval))
    (binding [vs/*current-expr* expr]
      (invoke-typing-rule (coerce/kw->symbol (u/internal-dispatch-val expr)) expr expected))
    (tc-ignore/check-tc-ignore check-expr expr expected)))

(defmethod internal-special-form ::t/fn
  [{[_ _ {{fn-anns :ann} :val} :as statements] :statements fexpr :ret :keys [env] :as expr} expected]
  ;(prn "check special :fn" expected)
  (prepare-check-fn env expr
    (special-fn/check-special-fn check-expr expr expected)))

(defmethod internal-special-form ::t/ann-form
  [expr expected]
  (if (#{:simulate} (some-> vs/*check-config* deref :type-check-eval))
    (binding [vs/*current-expr* expr]
      (invoke-typing-rule (coerce/kw->symbol (u/internal-dispatch-val expr)) expr expected))
    (ann-form/check-ann-form check-expr expr expected)))

(defmethod internal-special-form ::t/cast
  [{[_ _ {{tsyn :type} :val} :as statements] :statements frm :ret, :keys [env], :as expr} expected]
  (cast/check-cast check-expr expr expected))

(defmethod internal-special-form ::t/loop
  [{[_ _ {{tsyns :ann} :val} :as statements] :statements frm :ret, :keys [env], :as expr} expected]
  (special-loop/check-special-loop check-expr expr expected))

(defmethod internal-special-form :default
  [expr expected]
  (binding [vs/*current-expr* expr]
    (invoke-typing-rule (coerce/kw->symbol (u/internal-dispatch-val expr)) expr expected)))

(defmethod -check :do
  [expr expected]
  {:post [(r/TCResult? (u/expr-type %))
          (vector? (:statements %))]}
  (do/check-do check-expr internal-special-form expr expected))

(defmethod -check :monitor-enter
  [expr expected]
  (monitor/check-monitor check-expr expr expected))

(defmethod -check :monitor-exit
  [expr expected]
  (monitor/check-monitor check-expr expr expected))

(defmethod -check :local
  [expr expected]
  (local/check-local expr expected))

(defmethod -check :host-interop
  [{:keys [m-or-f target] :as expr} expected]
  (host-interop/check-host-interop check-expr expr expected))

(defmethod -check :host-call
  [expr expected]
  (host-interop/check-host-call check-expr -host-call-special expr expected))

(defmethod -check :host-field
  [expr expected]
  (host-interop/check-host-interop check-expr expr expected))

(defmethod -check :maybe-host-form
  [expr expected]
  (host-interop/check-maybe-host-form check-expr expr expected))

(defn clojure-lang-call? [^String m]
  (or 
    (.startsWith m "clojure.lang")
    (= m "java.lang.Class/getClassLoader")
    (= m "java.lang.AssertionError")
    (= m "java.io.StringWriter")
    (= m "java.lang.Object/getClass")))


(defmethod -check :static-call
  [expr expected]
  {:post [(-> % u/expr-type r/TCResult?)]}
  #_(prn "static-method")
  (u/trace 
    (let [inline? (-> (cu/MethodExpr->qualsym expr)
                      str
                      clojure-lang-call?)]
      (str (when-not inline? "non-inlined ") "static Call: " (cu/MethodExpr->qualsym expr))))
  (let [spec (-static-method-special expr expected)]
    (if (not= :default spec)
      spec
      (method/check-invoke-method check-expr expr expected))))

(defmethod -check :instance-call
  [expr expected]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (method/check-invoke-method check-expr expr expected))

(defmethod -check :static-field
  [expr expected]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (binding [vs/*current-expr* expr]
    (let [field (cu/FieldExpr->Field expr)]
      (u/trace 
        (let [inline? (-> (:type field)
                          str
                          clojure-lang-call?)]
          (str (when-not inline? "non-inlined ") "static field: " (:type field))))
      (assert field)
      (assoc expr
             u/expr-type (below/maybe-check-below
                           (r/ret (cu/Field->Type field))
                           expected)))))

(defmethod -check :instance-field
  [{target :instance target-class :class field-name :field :as expr} expected]
  {:post [(-> % u/expr-type r/TCResult?)]}
  #_(prn "instance-field:" expr)
  (binding [vs/*current-expr* expr]
   (let [field (cu/FieldExpr->Field expr)
         cexpr (check-expr target)]
    (if-not target-class
      ; I think target-class will never be false
      (err/tc-delayed-error (str "Call to instance field "
                               (symbol field-name)
                               " requires type hints."
                               (type-hints/suggest-type-hints 
                                 field-name 
                                 (-> cexpr u/expr-type r/ret-t)
                                 []))
                          :form (ast-u/emit-form-fn expr)
                          :return (assoc expr
                                         :instance cexpr
                                         u/expr-type (cu/error-ret expected)))
      (let [_ (assert (class? target-class))
            fsym (symbol field-name)
            ; check that the hinted class at least matches the runtime class we expect
            _ (let [expr-ty (c/fully-resolve-type (-> cexpr u/expr-type r/ret-t))
                    cls (cond
                          (r/DataType? expr-ty) (coerce/symbol->Class (:the-class expr-ty))
                          (r/RClass? expr-ty) (coerce/symbol->Class (:the-class expr-ty)))]
                (when-not (and cls
                               ; in case target-class has been redefined
                               (sub/class-isa? cls (-> target-class coerce/Class->symbol coerce/symbol->Class)))
                  (err/tc-delayed-error (str "Instance field " fsym " expected "
                                           (pr-str target-class)
                                           ", actual " (pr-str (prs/unparse-type expr-ty)))
                                      :form (ast-u/emit-form-fn expr))))

            ; datatype fields are special
            result-t (if-let [override (when-let [dtp (dt-env/get-datatype (coerce/Class->symbol target-class))]
                                         (let [dt (if (r/Poly? dtp)
                                                    ;generate new names
                                                    (cu/unwrap-datatype dtp (repeatedly (:nbound dtp) gensym))
                                                    dtp)
                                               _ (assert ((some-fn r/DataType? r/Record?) dt))
                                               demunged (symbol (repl/demunge (str fsym)))]
                                           (-> (c/DataType-fields* dt) (get demunged))))]
                       override
                       ; if not a datatype field, convert as normal
                       (if field
                         (cu/Field->Type field)
                         (err/tc-delayed-error (str "Instance field " fsym " needs type hints")
                                             :form (ast-u/emit-form-fn expr)
                                             :return (r/TCError-maker))))] 
        (assoc expr
               :instance cexpr
               u/expr-type (below/maybe-check-below
                             (r/ret result-t)
                             expected)))))))

(defmethod -check :maybe-class
  [expr expected]
  (let [expr (ana2/run-post-passes expr)]
    (if (= :maybe-class (:op expr))
      (err/tc-delayed-error (str "Unresolved host interop: " (:form expr)
                                 "\n\nHint: use *warn-on-reflection* to identify reflective calls")
                            :return (assoc expr u/expr-type r/-error))
      (check-expr expr expected))))

(defmethod -invoke-special 'clojure.core/instance?
  [{:keys [args] :as expr} & [expected]]
  (when-not (#{2} (count args))
    (err/int-error (str "Wrong number of arguments to clojure.core/instance?,"
                      " expected 2, given " (count (:args expr)))))
  (let [[cls-expr cexpr :as cargs] (-> args
                                  (update 0 #(check-expr % (r/ret (c/RClass-of Class))))
                                  (update 1 check-expr))]
    (if-let [cls (when (and (= :const (:op cls-expr))
                            (class? (:val cls-expr)))
                   (:val cls-expr))]
      (let [inst-of (c/RClass-of-with-unknown-params cls)
            expr-tr (u/expr-type cexpr)]
        (assoc expr
               :args cargs
               u/expr-type (below/maybe-check-below
                             (r/ret (c/Un r/-true r/-false)
                                    (fo/-FS (fo/-filter-at inst-of (r/ret-o expr-tr))
                                            (fo/-not-filter-at inst-of (r/ret-o expr-tr))))
                             expected)))
      :default)))

(defmethod -check :instance?
  [{cls :class the-expr :target :as expr} expected]
  ;(assert nil ":instance? node not used")
  (let [inst-of (c/RClass-of-with-unknown-params cls)
        cexpr (check-expr the-expr)
        expr-tr (u/expr-type cexpr)]
    (assoc expr
           :target cexpr
           u/expr-type (below/maybe-check-below
                         (r/ret (c/Un r/-true r/-false)
                                (fo/-FS (fo/-filter-at inst-of (r/ret-o expr-tr))
                                        (fo/-not-filter-at inst-of (r/ret-o expr-tr))))
                         expected))))

(defmulti -new-special (fn [expr & [expected]]
                         {:post [(symbol? %)]}
                         (-> expr
                             ast-u/new-op-class
                             coerce/Class->symbol)))

(defmethod -new-special 'clojure.lang.MultiFn
  [expr & [expected]]
  (when-not (== 4 (count (:args expr)))
    (err/int-error "Wrong arguments to clojure.lang.MultiFn constructor"))
  (let [{[nme-expr dispatch-expr default-expr hierarchy-expr] :args :as expr}
        (-> expr
            ;name
            (update-in [:args 0] check-expr)
            ;default
            (update-in [:args 2] check-expr)
            ;hierarchy
            (update-in [:args 3] check-expr))
        _ (when-not (= (:val hierarchy-expr) #'clojure.core/global-hierarchy)
            (err/int-error "Multimethod hierarchy cannot be customised"))
        _ (when-not (= (:val default-expr) :default)
            (err/int-error "Non :default default dispatch value NYI"))
        mm-name (:val nme-expr)
        _ (when-not (string? mm-name)
            (err/int-error "MultiFn name must be a literal string"))
        mm-qual (symbol (str (cu/expr-ns expr)) mm-name)
        cdisp (check-expr dispatch-expr)
        expected-mm-disp (multi/expected-dispatch-type (or (when expected
                                                             (r/ret-t expected))
                                                           (r/ret-t (u/expr-type cdisp))))
        ;; use the dispatch expected type when expected not available,
        ;; this way the type of the entire multimethod must correspond
        ;; to the inputs accepted by the dispatch function.
        expected-t (or (when expected
                         (r/ret-t expected))
                       expected-mm-disp)
        _ (assert (r/Type? expected-t))
        _ (when-not (sub/subtype? (-> cdisp u/expr-type r/ret-t) expected-mm-disp)
            (binding [vs/*current-expr* cdisp
                      vs/*current-env* (:env cdisp)]
              (cu/expected-error (-> cdisp u/expr-type r/ret-t) (r/ret expected-mm-disp))))
        _ (mm/add-multimethod-dispatch-type mm-qual (r/ret-t (u/expr-type cdisp)))]
    (-> expr
        (assoc-in [:args 1] cdisp)
        (assoc u/expr-type (below/maybe-check-below
                             (r/ret (c/In #_(c/RClass-of clojure.lang.MultiFn) 
                                          expected-t))
                             expected)))))

(defmethod -new-special :default [expr & [expected]] cu/not-special)

(defmethod -check :new
  [expr expected]
  {:post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  ;(prn ":new" (mapv (juxt :op :tag) args))
  (binding [vs/*current-expr* expr
            vs/*current-env* (:env expr)]
    (let [{:keys [args env] :as expr} (-> expr
                                          (update :class ana2/run-passes))
          _ (u/trace 
              (let [inline? (-> expr
                                ast-u/new-op-class 
                                coerce/Class->symbol
                                str
                                clojure-lang-call?)]
                (str (when-not inline? "non-inlined ") "new Call: " (-> expr
                                                                        ast-u/new-op-class 
                                                                        coerce/Class->symbol))))
          spec (-new-special expr expected)]
      (cond
        (not= cu/not-special spec) spec
        :else
        (let [inst-types *inst-ctor-types*
              expr (-> expr
                       (update :args #(binding [*inst-ctor-types* nil]
                                        (mapv check-expr %)))
                       ana2/run-post-passes)
              ;; call when we're convinced there's no way to rewrite this AST node
              ;; in a non-reflective way.
              give-up (fn [expr]
                        (let [clssym (-> expr
                                         ast-u/new-op-class 
                                         coerce/Class->symbol)]
                          (err/tc-delayed-error (str "Unresolved constructor invocation " 
                                                     (type-hints/suggest-type-hints 
                                                       nil 
                                                       nil 
                                                       (map (comp r/ret-t u/expr-type) (:args expr))
                                                       :constructor-call clssym)
                                                     ".\n\nHint: add type hints")
                                                :form (ast-u/emit-form-fn expr)
                                                :return (assoc expr
                                                               u/expr-type (cu/error-ret expected)))))
              ;; returns the function type for this constructor, or nil if
              ;; it is reflective.
              ctor-fn (fn [expr]
                        (when (:validated? expr)
                          (let [clssym (-> expr
                                           ast-u/new-op-class 
                                           coerce/Class->symbol)]
                            (or (ctor-override/get-constructor-override clssym)
                                (and (dt-env/get-datatype clssym)
                                     (cu/DataType-ctor-type clssym))
                                (when-let [ctor (cu/NewExpr->Ctor expr)]
                                  (cu/Constructor->Function ctor))))))
              ;; check a non-reflective constructor
              check-validated (fn [expr]
                                (let [ifn (-> (if inst-types
                                                (inst/manual-inst (ctor-fn expr) inst-types)
                                                (ctor-fn expr))
                                              r/ret)
                                      ;_ (prn "Expected constructor" (prs/unparse-type (r/ret-t ifn)))
                                      res-type (funapp/check-funapp expr (:args expr) ifn (map u/expr-type (:args expr)) expected)]
                                  (assoc expr
                                         u/expr-type res-type)))]
          ;; try to rewrite, otherwise error on reflection
          (cond
            (:validated? expr) (check-validated expr)

            (cu/should-rewrite?) (let [expr (update expr :args #(mapv host-interop/add-type-hints %))
                                       rexpr (host-interop/try-resolve-reflection expr)]
                                   ;; rexpr can only be :new
                                   (case (:op rexpr)
                                     (:new) (if (:validated? rexpr)
                                              (check-validated rexpr)
                                              (give-up rexpr))))
            :else (give-up expr)))))))

(defmethod -check :throw
  [expr expected]
  (throw/check-throw check-expr expr expected (r/ret (c/RClass-of Throwable))))

(defmethod -check :recur
  [{args :exprs :keys [env] :as expr} expected]
  {:post [(vector? (:exprs %))]}
  (recur/check-recur args env expr expected check-expr))

(defmethod -check :binding
  [{:keys [init] :as expr} expected]
  (binding/check-binding check-expr expr expected))

(defmethod -check :loop
  [{binding-inits :bindings :keys [body] :as expr} expected]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:bindings %))]}
  (loop/check-loop check-expr expr expected))

(defmethod -check :let
  [{bindings :bindings :keys [body] :as expr} expected]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:bindings %))]}
  (let/check-let check-expr expr expected))

(defmethod -check :letfn
  [{bindings :bindings :keys [body] :as expr} expected]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:bindings %))]}
  (letfn/check-letfn bindings body expr expected check-expr))

(defmethod -check :with-meta
  [expr expected]
  (with-meta/check-with-meta check-expr expr expected))

(defmethod -check :if
  [{:keys [test then else] :as expr} expected]
  (if/check-if check-expr expr expected))

(defmethod -check :def
  [{:keys [var env] :as expr} expected]
  ; annotation side effect
  (let [prs-ns (cu/expr-ns expr)]
    (let [mvar (meta var)
          qsym (coerce/var->symbol var)]
      (when-let [[_ tsyn] (find mvar :ann)]
        (let [ann-type (binding [vs/*current-env* env
                                 prs/*parse-type-in-ns* prs-ns]
                         (prs/parse-type tsyn))]
          (var-env/add-var-type qsym ann-type)))
      (when (:no-check mvar)
        (var-env/add-nocheck-var qsym))))
  (def/check-def check-expr expr expected))

(defmethod -check :deftype
  [{:keys [fields methods env] :as expr} expected]
  {:pre []
   :post [(-> % u/expr-type r/TCResult?)]}
  ;TODO check fields match, handle extra fields in records
  (binding [vs/*current-env* env]
    (let [compiled-class (:class-name expr)
          ;; taj/validate turns :class-name into a class.
          ;; might not be run at this point.
          compiled-class (if (symbol? compiled-class)
                           (coerce/symbol->Class compiled-class)
                           compiled-class)
          _ (assert (class? compiled-class) (class compiled-class))
          nme (coerce/Class->symbol compiled-class)
          field-syms (map :name fields)
          _ (assert (every? symbol? field-syms))
          ; unannotated datatypes are handled below
          dtp (dt-env/get-datatype nme)
          [nms bbnds dt] (if (r/TypeFn? dtp)
                           (let [nms (c/TypeFn-fresh-symbols* dtp)
                                 bbnds (c/TypeFn-bbnds* nms dtp)
                                 body (c/TypeFn-body* nms dtp)]
                             [nms bbnds body])
                           [nil nil dtp])
          expected-fields (when dt
                            (c/DataType-fields* dt))
          expected-field-syms (vec (keys expected-fields))
          ret-expr (assoc expr
                          u/expr-type (below/maybe-check-below
                                        (r/ret (c/RClass-of Class))
                                        expected))]

      (cond
        (not dtp)
        (err/tc-delayed-error (str "deftype " nme " must have corresponding annotation. "
                                 "See ann-datatype and ann-record")
                            :return ret-expr)

        (not ((some-fn r/DataType? r/Record?) dt))
        (err/tc-delayed-error (str "deftype " nme " cannot be checked against: " (prs/unparse-type dt))
                            :return ret-expr)

        (if (r/Record? dt)
          (c/isa-DataType? compiled-class)
          (c/isa-Record? compiled-class))
        (let [datatype? (c/isa-DataType? compiled-class)]
          #_(prn (c/isa-DataType? compiled-class)
               (c/isa-Record? compiled-class)
               (r/DataType? dt)
               (r/Record? dt))
          (err/tc-delayed-error (str (if datatype? "Datatype " "Record ") nme 
                                   " is annotated as a " (if datatype? "record" "datatype") 
                                   ", should be a " (if datatype? "datatype" "record") ". "
                                   "See ann-datatype and ann-record")
                              :return ret-expr))

        (not= expected-field-syms 
              ; remove implicit __meta and __extmap fields
              (if (c/isa-Record? compiled-class)
                (remove cu/record-hidden-fields field-syms)
                field-syms))
        (err/tc-delayed-error (str (if (c/isa-Record? compiled-class)
                                     "Record "
                                     "Datatype ")
                                   nme " fields do not match annotation. "
                                 " Expected: " (vec expected-field-syms)
                                 ", Actual: " (vec field-syms))
                            :return ret-expr)

        :else
        (let [check-method? (fn [inst-method]
                              (not (and (r/Record? dt)
                                        (cu/record-implicits (symbol (:name inst-method))))))
              maybe-check-method
              (fn [{:keys [env] :as inst-method}]
                ;; returns a vector of checked methods
                {:pre [(#{:method} (:op inst-method))]
                 :post [(vector? %)]}
                (if-not (check-method? inst-method)
                  [inst-method]
                  (do
                    (assert (#{:method} (:op inst-method)))
                    (when vs/*trace-checker*
                      (println "Checking deftype* method: " (:name inst-method))
                      (flush))
                    (binding [vs/*current-env* env]
                      (let [method-nme (:name inst-method)
                            _ (assert (symbol? method-nme))
                            ;_ (prn "method-nme" method-nme)
                            ;_ (prn "inst-method" inst-method)
                            _ (assert (:this inst-method))
                            _ (assert (:params inst-method))
                            ; minus the target arg
                            method-sig (first (filter 
                                                (fn [{:keys [name required-params]}]
                                                  (and (= (count (:parameter-types inst-method))
                                                          (count required-params))
                                                       (#{(munge method-nme)} name)))
                                                (:methods inst-method)))]
                        (if-not method-sig
                          (err/tc-delayed-error (str "Internal error checking deftype " nme " method: " method-nme)
                                                :return [inst-method])
                          (let [expected-ifn (cu/datatype-method-expected dt method-sig)]
                            ;(prn "method expected type" expected-ifn)
                            ;(prn "names" nms)
                            (lex/with-locals expected-fields
                              (free-ops/with-free-mappings 
                                (zipmap (map (comp r/F-original-name r/make-F) nms) 
                                        (map (fn [nm bnd] {:F (r/make-F nm) :bnds bnd}) nms bbnds))
                                ;(prn "lexical env when checking method" method-nme (lex/lexical-env))
                                ;(prn "frees when checking method" 
                                ;     (into {} (for [[k {:keys [name]}] clojure.core.typed.checker.tvar-env/*current-tvars*]
                                ;                [k name])))
                                ;(prn "bnds when checking method" 
                                ;     clojure.core.typed.checker.tvar-bnds/*current-tvar-bnds*)
                                ;(prn "expected-ifn" expected-ifn)
                                (:methods
                                  (fn-methods/check-fn-methods
                                    [inst-method]
                                    expected-ifn
                                    :recur-target-fn
                                    (fn [{:keys [dom] :as f}]
                                      {:pre [(r/Function? f)]
                                       :post [(recur-u/RecurTarget? %)]}
                                      (recur-u/RecurTarget-maker (rest dom) nil nil nil))
                                    :validate-expected-fn
                                    (fn [fin]
                                      {:pre [(r/FnIntersection? fin)]}
                                      (when (some #{:rest :drest :kws} (:types fin))
                                        (err/int-error
                                          (str "Cannot provide rest arguments to deftype method: "
                                               (prs/unparse-type fin))))))))))))))))

              methods 
              (binding [fn-method-u/*check-fn-method1-checkfn* check-expr
                        fn-method-u/*check-fn-method1-rest-type* 
                        (fn [& args] 
                          (err/int-error "deftype method cannot have rest parameter"))]
                (into []
                      (mapcat maybe-check-method)
                      methods))]
          (assoc ret-expr
                 :methods methods))))))

(defmethod -check :import
  [expr expected]
  (assoc expr
         u/expr-type (below/maybe-check-below
                       (r/ret r/-nil)
                       expected)))

(defmethod -check :case-test
  [{:keys [test] :as expr} expected]
  (let [ctest (check-expr test expected)]
    (assoc expr
           :test ctest
           u/expr-type (u/expr-type ctest))))

(defmethod -check :case
  [{target :test :keys [tests thens default] :as expr} expected]
  {:post [((every-pred vector?
                       (con/every-c? (every-pred
                                       (comp #{:case-test} :op)
                                       :test)))
           (:tests %))
          ((every-pred vector?
                       (con/every-c? (every-pred
                                       (comp #{:case-then} :op)
                                       :then)))
           (:thens %))
          (-> % u/expr-type r/TCResult?)]}
  ; tests have no duplicates
  (binding [vs/*current-expr* expr
            vs/*current-env* (:env expr)]
    (let [ctarget (check-expr target)
          target-ret (u/expr-type ctarget)
          _ (assert (r/TCResult? target-ret))
          ctests (mapv check-expr tests)
          tests-rets (map u/expr-type ctests)
          ; Can we derive extra information from 'failed'
          ; tests? Delegate to check-case-thens for future enhancements.
          cthens (case/check-case-thens check-expr target-ret tests-rets thens expected)
          cdefault (let [flag+ (atom true :validator boolean?)
                         neg-tst-fl (let [val-ts (map (comp c/fully-resolve-type r/ret-t) tests-rets)]
                                      (if (every? r/Value? val-ts)
                                        (fo/-not-filter-at (apply c/Un val-ts)
                                                           (r/ret-o target-ret))
                                        fl/-top))
                         env-default (update/env+ (lex/lexical-env) [neg-tst-fl] flag+)
                         _ (when-not @flag+
                             ;; FIXME should we ignore this branch?
                             (u/tc-warning "Local became bottom when checking case default"))]
                     ;(prn "neg-tst-fl" neg-tst-fl)
                     ;(prn "env-default" env-default)
                     (var-env/with-lexical-env env-default
                       (check-expr default expected)))
          ;; FIXME this is a duplicated expected test, already done able
          case-result (let [type (apply c/Un (map (comp :t u/expr-type) (cons cdefault cthens)))
                            ; TODO
                            filter (fo/-FS fl/-top fl/-top)
                            ; TODO
                            object obj/-empty]
                        (below/maybe-check-below
                          (r/ret type filter object)
                          expected))]
      (assoc expr
             :test ctarget
             :tests ctests
             :thens cthens
             :default cdefault
             u/expr-type case-result))))

(defmethod -check :catch
  [expr expected]
  (catch/check-catch check-expr expr expected))

(defmethod -check :try
  [expr expected]
  {:post [(vector? (:catches %))
          (-> % u/expr-type r/TCResult?)]}
  (try/check-try check-expr expr expected))

(defmethod -check :set!
  [expr expected]
  (set!/check-set! check-expr expr expected))
