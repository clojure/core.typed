(ns clojure.core.typed.check-ns-cljs
  (:require [cljs.compiler :as comp]
            [clojure.core.typed.current-impl :as impl]
            [cljs.env :as env]
            [cljs.compiler :as comp]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.reset-caches :as reset-caches]
            [clojure.core.typed.reset-env :as reset-env]
            [clojure.core.typed.collect-cljs :as clt-cljs]
            [clojure.core.typed.check-cljs :as chk-cljs]
            [clojure.core.typed.check-ns :as check-ns]
            [clojure.core.typed.errors :as err]))

(defn check-ns-cljs
  [nsym & opt]
  (env/ensure
    (comp/with-core-cljs
      (impl/with-cljs-impl
        (reset-caches/reset-caches)
        (reset-env/reset-envs!)
        (if vs/*checking*
          (throw (Exception. "Found inner call to check-ns or cf"))
          (binding [vs/*checking* true
                    cljs.core.typed/*already-collected* (atom #{})
                    vs/*already-checked* (atom #{})
                    ;vs/*trace-checker* trace
                    vs/*analyze-ns-cache* (atom {})
                    vs/*delayed-errors* (err/-init-delayed-errors)
                    ;vs/*checked-asts* (when (== 1 (count nsym-coll))
                    ;                    (atom {}))]
                    ]
            (let [_ (clt-cljs/collect-ns nsym)
                  _ (chk-cljs/check-ns nsym)]
              ;handle errors
              (if-let [errors (seq @vs/*delayed-errors*)]
                (err/print-errors! errors)
                :ok))))))))

