(ns ^:skip-wiki clojure.core.typed.impl.js.util-cljs
  (:require [clojure.core.typed.chk.common.current-impl :as impl]
            [cljs.analyzer :as ana]
            [cljs.jvm.tools.analyzer.emit-form :as emit-form]
            [cljs.compiler :as comp]
            [cljs.env :as env]))

(alter-meta! *ns* assoc :skip-wiki true)

(defn resolve-var [nsym sym]
  (env/ensure
    (binding [ana/*cljs-ns* nsym]
      (comp/with-core-cljs
        (ana/resolve-var (ana/empty-env) sym)))))

(defn cljs-ns []
  ana/*cljs-ns*)

(defn emit-form [f]
  (emit-form/emit-form f))
