(ns ^:skip-wiki clojure.core.typed.check-form-common
  (:require [clojure.core.typed.profiling :as p]
            [clojure.core.typed.check :as chk]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.reset-caches :as reset-caches]
            [clojure.core.typed.deps.clojure.core.cache :as cache]
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
;;  - :ret             TCResult inferred for the current form
;;  - :out-form        The macroexpanded result of type-checking, if successful. 
;;  - :result          The evaluated result of :out-form, unless :no-eval is provided.
;;  - :ex              If an exception was thrown during evaluation, this key will be present
;;                     with the exception as the value.
;;  DEPRECATED
;;  - :delayed-errors  A sequence of delayed errors (ex-info instances)
(defn check-form-info
  [{:keys [ast-for-form unparse-ns
           check-expr collect-expr eval-out-ast
           emit-form eval-out-ast env]}
   form & {:keys [expected-ret expected type-provided? profile file-mapping
                  checked-ast no-eval bindings-atom]}]
  {:pre [((some-fn nil? con/atom?) bindings-atom)]}
  (assert (not (and expected-ret type-provided?)))
  (p/profile-if profile
    (reset-caches/reset-caches)
    (binding [vs/*already-collected* (atom #{})
              vs/*already-checked* (atom #{})
              vs/*delayed-errors* (err/-init-delayed-errors)
              vs/*analyze-ns-cache* (cache/soft-cache-factory {})
              vs/*in-check-form* true]
      (let [expected (or
                       expected-ret
                       (when type-provided?
                         (r/ret (prs/parse-type expected))))
            stop-analysis (atom nil)
            stop! (fn [] (reset! stop-analysis true))
            delayed-errors-fn (fn [] (seq @vs/*delayed-errors*))
            file-mapping-atom (atom [])
            eval-ast (fn [{:keys [expected] :as opt} ast]
                       (do (collect-expr ast)
                           (let [c-ast (do 
                                         (reset-caches/reset-caches)
                                         (check-expr ast expected))
                                 eval-cexp (or (when-not no-eval
                                                 eval-out-ast)
                                               identity)
                                 _ (when file-mapping
                                     (swap! file-mapping-atom
                                            (fn [v]
                                              (reduce 
                                                conj v
                                                (file-map/ast->file-mapping c-ast)))))]
                             (or (some-> (seq (delayed-errors-fn)) 
                                         err/print-errors!)
                                 (eval-cexp c-ast)))))
            terminal-error (atom nil)
            c-ast (try
                    (ast-for-form form
                                  {:bindings-atom bindings-atom
                                   :eval-fn eval-ast
                                   :expected expected
                                   :stop-analysis stop-analysis
                                   :env env})
                    (catch Throwable e
                      (let [e (if (some-> e ex-data err/tc-error?)
                                (try
                                  (err/print-errors! (vec (concat (delayed-errors-fn) [e])))
                                  (catch Throwable e
                                    e))
                                e)]
                        (reset! terminal-error e)
                        nil)))
            res (some-> c-ast u/expr-type)
            delayed-errors (delayed-errors-fn)
            ex @terminal-error]
        (merge
          {:delayed-errors (vec delayed-errors)
           :ret (or res (r/ret r/-error))}
          (when ex
            {:ex ex})
          (when checked-ast
            ;; fatal type error = nil
            {:checked-ast c-ast})
          (when (and (impl/checking-clojure?)
                     (not no-eval)
                     (empty? delayed-errors)
                     (not ex))
            {:result (:result c-ast)})
          (when (and c-ast emit-form (not ex))
            {:out-form (emit-form c-ast)})
          (when (impl/checking-clojure?)
            (when file-mapping
              {:file-mapping @file-mapping-atom})))))))

(defn check-form*
  [{:keys [impl unparse-ns] :as config} form expected type-provided?]
  (let [{:keys [delayed-errors ret]} (check-form-info config form
                                                      :expected expected 
                                                      :type-provided? type-provided?)]
    (if-let [errors (seq delayed-errors)]
      (err/print-errors! errors)
      (prs/unparse-TCResult-in-ns ret unparse-ns))))
