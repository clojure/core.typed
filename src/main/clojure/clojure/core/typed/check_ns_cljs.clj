(ns clojure.core.typed.check-ns-cljs
  (:require [cljs.compiler :as comp]
            [clojure.core.typed.current-impl :as impl]
            [cljs.env :as env]
            [clojure.core.typed.util-cljs :as ucljs]
            [clojure.core.typed.check-ns-common :as chk-ns]))

(defn check-ns-info
  [ns-or-syms opt]
  (ucljs/with-cljs-typed-env
    (comp/with-core-cljs
      nil
      #(chk-ns/check-ns-info impl/clojurescript ns-or-syms opt))))

(defn check-ns
  [ns-or-syms opt]
  (ucljs/with-cljs-typed-env
    (comp/with-core-cljs
      nil
      #(chk-ns/check-ns impl/clojurescript ns-or-syms opt))))
