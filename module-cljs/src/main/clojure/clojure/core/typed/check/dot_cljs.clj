(ns clojure.core.typed.check.dot-cljs
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.jsnominal-env :as jsnom]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.check.funapp :as funapp]
            [clojure.core.typed.errors :as err]))

(defn check-dot [check {:keys [target field method args] :as dot-expr} expected]
  (let [ctarget (check target)
        target-t (-> ctarget u/expr-type r/ret-t)
        resolved (let [t (c/fully-resolve-type target-t)]
                   ;TODO DataType
                   (when ((some-fn r/JSNominal? 
                                   r/StringCLJS?
                                   #_r/DataType?) t)
                     t))]
    (if resolved
      (cond
        field
        (let [field-type (cond
                           (r/StringCLJS? resolved)
                           (jsnom/get-field 'string nil field)
                           (r/JSNominal? resolved)
                           (jsnom/get-field (:name resolved) (:poly? resolved) field))
              _ (assert field-type (str "Don't know how to get field " field
                                        " from " (prs/unparse-type resolved)))]
          (assoc dot-expr
                 u/expr-type (r/ret field-type)))
        :else
        (let [method-type (cond
                            (r/StringCLJS? resolved)
                            (jsnom/get-method 'string nil method)
                            (r/JSNominal? resolved)
                            (jsnom/get-method (:name resolved) (:poly? resolved) method))
              _ (assert method-type (str "Don't know how to call method " method
                                         " from " (prs/unparse-type resolved)))
              cargs (mapv check args)
              actual (funapp/check-funapp nil cargs (r/ret method-type) (map u/expr-type cargs)
                                          expected)]
          (assoc dot-expr
                 u/expr-type actual)))
      (err/tc-delayed-error (str "Don't know how to use type " (prs/unparse-type target-t)
                                 " with "
                                 (if field (str "field " field)
                                   (str "method " method)))
                            :return 
                            (assoc dot-expr
                                   u/expr-type (r/ret (or (when expected
                                                            (r/ret-t expected))
                                                          (r/TCError-maker))))))))
