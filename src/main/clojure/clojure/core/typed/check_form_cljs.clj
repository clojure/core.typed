(ns clojure.core.typed.check-form-cljs
  (:require [clojure.core.typed.reset-caches :as reset-caches]
            [cljs.env :as env]
            [cljs.compiler :as comp]
            [clojure.core.typed.util-cljs :as ucljs]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.analyze-cljs :as ana-cljs]
            [clojure.core.typed.collect-cljs :as clt-cljs]
            [clojure.core.typed.check-cljs :as chk-cljs]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.parse-unparse :as prs]))

(defn check-form-cljs
  "Check a single form with an optional expected type.
  Intended to be called from Clojure. For evaluation at the Clojurescript
  REPL see cf."
  [form expected expected-provided?]
  (reset-caches/reset-caches)
  (env/ensure
    (comp/with-core-cljs
      (if vs/*checking*
        (throw (Exception. "Found inner call to check-ns or cf"))
        (binding [vs/*checking* true
                  vs/*delayed-errors* (err/-init-delayed-errors)]
          (impl/with-cljs-impl
            (let [ast (ana-cljs/ast-for-form form)
                  ;collect
                  _ (clt-cljs/collect ast)
                  ;check
                  c-ast (chk-cljs/check ast
                         (when expected-provided?
                           (r/ret (prs/parse-type expected))))]
              ;handle errors
              (if-let [errors (seq @vs/*delayed-errors*)]
                (err/print-errors! errors)
                (-> c-ast 
                    u/expr-type
                    (prs/unparse-TCResult-in-ns (ucljs/cljs-ns)))))))))))
