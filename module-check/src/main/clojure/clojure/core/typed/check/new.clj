(ns clojure.core.typed.check.new
  (:require [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.check.funapp :as funapp]
            [clojure.core.typed.check.multi :as multi]
            [clojure.core.typed.check.type-hints :as type-hints]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.ctor-override-env :as ctor-override]
            [clojure.core.typed.datatype-env :as dt-env]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.inst :as inst]
            [clojure.core.typed.mm-env :as mm]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.util-vars :as vs]))

(defmulti new-special (fn [check expr expected] 
                        (some-> (ast-u/new-op-class expr) coerce/Class->symbol)))

(defmethod new-special 'clojure.lang.MultiFn
  [check {[nme-expr dispatch-expr default-expr hierarchy-expr :as args] :args :as expr} expected]
  (when-not expected
    (err/int-error "clojure.lang.MultiFn constructor requires an expected type"))
  (when-not (== 4 (count args))
    (err/int-error "Wrong arguments to clojure.lang.MultiFn constructor"))
  (when-not (= (:val hierarchy-expr) #'clojure.core/global-hierarchy)
    (err/int-error "Multimethod hierarchy cannot be customised"))
  (when-not (= (:val default-expr) :default)
    (err/int-error "Non :default default dispatch value NYI"))
  (let [mm-name (:val nme-expr)
        _ (when-not (string? mm-name)
            (err/int-error "MultiFn name must be a literal string"))
        mm-qual (symbol (str (cu/expr-ns expr)) mm-name)
        ;_ (prn "mm-qual" mm-qual)
        ;_ (prn "expected r/ret-t" (prs/unparse-type (r/ret-t expected)))
        ;_ (prn "expected r/ret-t class" (class (r/ret-t expected)))
        expected-mm-disp (multi/expected-dispatch-type (r/ret-t expected))
        cdisp (check dispatch-expr (r/ret expected-mm-disp))
        cargs [(check nme-expr)
               cdisp
               (check default-expr)
               (check hierarchy-expr)]
        _ (assert (== (count cargs) (count args)))
        _ (mm/add-multimethod-dispatch-type mm-qual (r/ret-t (u/expr-type cdisp)))]
    (assoc expr
           :args cargs
           u/expr-type (r/ret (c/In (c/RClass-of clojure.lang.MultiFn) (r/ret-t expected))))))

(defmethod new-special :default [check expr expected] cu/not-special)

(defn check-new
  [check {:keys [args env] :as expr} & [expected]]
  {:post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (binding [vs/*current-expr* expr
            vs/*current-env* env]
    (let [ctor (cu/NewExpr->Ctor expr)
          spec (new-special check expr expected)]
      (cond
        (not= cu/not-special spec) spec
        :else
        (let [inst-types cu/*inst-ctor-types*
              cls (ast-u/new-op-class expr)
              clssym (coerce/ctor-Class->symbol cls)
              cargs (mapv check args)
              ctor-fn (or (@ctor-override/CONSTRUCTOR-OVERRIDE-ENV clssym)
                          (and (dt-env/get-datatype clssym)
                               (cu/DataType-ctor-type clssym))
                          (when ctor
                            (cu/Constructor->Function ctor)))]
          (if-not ctor-fn
            (err/tc-delayed-error (str "Unresolved constructor invocation " 
                                     (type-hints/suggest-type-hints 
                                       nil 
                                       nil 
                                       (map (comp r/ret-t u/expr-type) cargs)
                                       :constructor-call clssym)
                                     ".\n\nHint: add type hints"
                                     "\n\nin: " (ast-u/emit-form-fn expr))
                                :form (ast-u/emit-form-fn expr)
                                :return (assoc expr
                                               :args cargs
                                               u/expr-type (cu/error-ret expected)))
            (let [ctor-fn (if inst-types
                            (inst/manual-inst ctor-fn inst-types)
                            ctor-fn)
                  ifn (r/ret ctor-fn)
                  ;_ (prn "Expected constructor" (prs/unparse-type (r/ret-t ifn)))
                  res-type (funapp/check-funapp expr args ifn (map u/expr-type cargs) nil)
                  _ (when (and expected (not (sub/subtype? (r/ret-t res-type) (r/ret-t expected))))
                      (cu/expected-error (r/ret-t res-type) (r/ret-t expected)))]
              (assoc expr
                     :args cargs
                     u/expr-type res-type))))))))
