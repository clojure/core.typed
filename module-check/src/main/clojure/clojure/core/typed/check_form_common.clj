(ns ^:skip-wiki clojure.core.typed.check-form-common
  (:require [clojure.core.typed.profiling :as p]
            [clojure.core.typed.check :as chk]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.reset-caches :as reset-caches]
            [clojure.core.cache :as cache]
            [clojure.core.typed.file-mapping :as file-map]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.parse-unparse :as prs])
  (:import (clojure.lang ExceptionInfo)))

;; (check-form-info config-map form & kw-args)
;; 
;; Takes a configuration map which different implementations can customize
;; (via eg. clojure.core.typed.check-form-{clj,cljs}), a form to type check
;; and keyword arguments propagated from core.typed users
;; (via eg. {clojure,cljs}.core.typed/check-form-info).
;;
;; Also see docstrings for clojure.core.typed/check-form-info
;; and cljs.core.typed/check-form-info.
;;
;; 
;; Takes config-map as first argument:
;;  Mandatory
;; - :impl          keyword passed to impl/with-full-impl, which is wrapped around 
;;                  the entire function.
;; - :ast-for-form  function from form to tools.analyzer AST, taking :bindings-atom as keyword
;;                  argument.
;; - :collect-expr  side-effecting function taking AST and collecting type annotations
;; - :check-expr    function taking AST and expected type and returns a checked AST.
;;
;;  Optional
;; - :eval-out-ast  function taking checked AST which evaluates it and returns the AST
;;                  with a :result entry attached, the result of evaluating it,
;;                  if no type errors occur.
;; - :unparse-ns    namespace in which to pretty-print type.  (FIXME Currently unused)
;; - :emit-form     function from AST to equivalent form, returned in :out-form entry.
;;
;;  (From here, copied from clojure.core.typed/check-form-info)
;; Keyword arguments
;;  Options
;;  - :expected        Type syntax representing the expected type for this form
;;                     type-provided? option must be true to utilise the type.
;;  - :type-provided?  If true, use the expected type to check the form.
;;  - :profile         Use Timbre to profile the type checker. Timbre must be
;;                     added as a dependency.
;;  - :file-mapping    If true, return map provides entry :file-mapping, a hash-map
;;                     of (Map '{:line Int :column Int :file Str} Str).
;;  - :checked-ast     Returns the entire AST for the given form as the :checked-ast entry,
;;                     annotated with the static types inferred after checking.
;;                     If a fatal type error occurs, :checked-ast is nil.
;;  - :no-eval         If true, don't evaluate :out-form. Removes :result return value.
;;                     It is highly recommended to evaluate :out-form manually.
;;  - :bindings-atom   an atom which contains a value suitable for with-bindings.
;;                     Will be updated during macroexpansion and evaluation.
;;  
;;  Default return map
;;  - :delayed-errors  A sequence of delayed errors (ex-info instances)
;;  - :ret             TCResult inferred for the current form
;;  - :out-form        The macroexpanded result of type-checking, if successful. 
;;  - :result          The evaluated result of :out-form, unless :no-eval is provided."
(defn check-form-info
  [{:keys [impl ast-for-form unparse-ns
           check-expr collect-expr eval-out-ast
           emit-form eval-out-ast]}
   form & {:keys [expected-ret expected type-provided? profile file-mapping
                  checked-ast no-eval bindings-atom]}]
  {:pre [((some-fn nil? con/atom?) bindings-atom)]}
  (assert (not (and expected-ret type-provided?)))
  (p/profile-if profile
    (reset-caches/reset-caches)
    (if vs/*checking*
      (err/int-error "Found inner call to check-ns or cf")
      (impl/with-full-impl impl
        (binding [vs/*checking* true
                  vs/*already-collected* (atom #{})
                  vs/*already-checked* (atom #{})
                  vs/*delayed-errors* (err/-init-delayed-errors)
                  vs/*analyze-ns-cache* (cache/soft-cache-factory {})]
          (let [terminal-error? (atom nil)
                expected (or
                           expected-ret
                           (when type-provided?
                             (r/ret (prs/parse-type expected))))
                no-errors? (fn [] (and (empty? @vs/*delayed-errors*)
                                       (not @terminal-error?)))
                stop-analysis (atom nil)
                stop! (fn [] (reset! stop-analysis true))
                eval-ast (fn [{:keys [expected] :as opt} ast]
                           (try
                             (do (when (no-errors?)
                                   (collect-expr ast))
                                 (let [c-ast (if (no-errors?)
                                               (do 
                                                 (reset-caches/reset-caches)
                                                 (check-expr ast expected))
                                               ast)
                                       eval-cexp (or (when-not no-eval
                                                       eval-out-ast)
                                                     identity)]
                                   (if (no-errors?)
                                     (eval-cexp c-ast)
                                     (do
                                       ;; if we don't evaluate this form and we're part of a top-level
                                       ;; `do`, then we have to stop analyze+eval from analyzing the
                                       ;; rest of the do expression. This eval-ast function will not be called again.
                                       (stop!)
                                       c-ast))))
                             (catch ExceptionInfo e
                               (if (err/tc-error? (ex-data e))
                                 (do (stop!)
                                     (reset! terminal-error? e))
                                 (throw e))
                               ast)))
                c-ast (ast-for-form form
                                    {:bindings bindings-atom
                                     :eval-fn eval-ast
                                     :expected expected
                                     :stop-analysis stop-analysis})
                res (some-> c-ast u/expr-type)
                delayed-errors (concat @vs/*delayed-errors*
                                       (some-> @terminal-error? vector))]
            (merge
              {:delayed-errors delayed-errors
               :ret (or res (r/ret r/-error))}
              (when checked-ast
                ;; fatal type error = nil
                {:checked-ast c-ast})
              (when (and (#{impl/clojure} impl)
                         (not no-eval)
                         (empty? delayed-errors))
                {:result (:result c-ast)})
              (when (and c-ast emit-form)
                {:out-form (emit-form c-ast)})
              (when (#{impl/clojure} impl)
                (when file-mapping
                  {:file-mapping (file-map/ast->file-mapping c-ast)})))))))))

(defn check-form*
  [{:keys [impl unparse-ns] :as config} form expected type-provided?]
  (let [{:keys [delayed-errors ret]} (check-form-info config form
                                                      :expected expected 
                                                      :type-provided? type-provided?)]
    (impl/with-full-impl impl
      (if-let [errors (seq delayed-errors)]
        (err/print-errors! errors)
        (binding [vs/*checking* true]
          (prs/unparse-TCResult-in-ns ret unparse-ns))))))
