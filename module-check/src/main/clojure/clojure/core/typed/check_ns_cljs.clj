(ns clojure.core.typed.check-ns-cljs
  (:require [cljs.compiler :as comp]
            [clojure.core.typed.current-impl :as impl]
            [cljs.env :as env]
            [cljs.compiler :as comp]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.util-cljs :as ucljs]
            [clojure.core.typed.reset-caches :as reset-caches]
            [clojure.core.typed.reset-env :as reset-env]
            [clojure.core.typed.collect-cljs :as collect-cljs]
            [clojure.core.typed.check-cljs :as chk-cljs]
            [clojure.core.typed.check-ns-common :as chk-ns]
            [clojure.core.typed.errors :as err]))

; cache of namespaces that should be skipped in check-ns
(def already-checked (atom #{}))

(defn check-ns-info
  [ns-or-syms & opt]
  (ucljs/with-cljs-typed-env
    (comp/with-core-cljs
      (apply chk-ns/check-ns-info impl/clojurescript ns-or-syms 
             :collect-ns collect-cljs/collect-ns
             :check-ns chk-cljs/check-ns
             :already-checked already-checked
             opt))))

(defn check-ns
  [ns-or-syms & opt]
  (ucljs/with-cljs-typed-env
    (comp/with-core-cljs
      (apply chk-ns/check-ns impl/clojurescript ns-or-syms 
             :collect-ns collect-cljs/collect-ns
             :check-ns chk-cljs/check-ns
             :already-checked already-checked
             opt))))
