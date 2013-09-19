(ns ^:skip-wiki clojure.core.typed.util-cljs
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.load-cljs :as load]))

(alter-meta! *ns* assoc :skip-wiki true)

; this namespace should compile fine without CLJS dep

(defn assert-cljs-dep []
  (load/load-cljs)
  (assert (find-ns 'cljs.analyzer) "Clojurescript dependency missing"))

;from cljs.compiler
(defmacro with-core-cljs
  "Ensure that core.cljs has been loaded."
  [& body]
  `(do (assert-cljs-dep)
       (let [analyze-file# (impl/v 'cljs.analyzer/analyze-file)
             namespaces# (impl/v 'cljs.analyzer/namespaces)]
         (when-not (:defs (get @namespaces# 'cljs.core))
           (println "Initializing cljs.core")
           (flush)
           (analyze-file# "cljs/core.cljs")))
       ~@body))

(defn ^:private empty-env [nsym]
  (assert-cljs-dep)
  (with-bindings {(impl/the-var 'cljs.analyzer/*cljs-ns*) nsym}
    (let [empty-env (impl/v 'cljs.analyzer/empty-env)]
      (empty-env))))


(defn resolve-var [nsym sym]
  (assert-cljs-dep)
  (with-core-cljs
    (let [namespaces (impl/v 'cljs.analyzer/namespaces)
          resolve-var (impl/v 'cljs.analyzer/resolve-var)]
      (assert (contains? @namespaces nsym)
              (str "Namespace " nsym " does not exist"))
      (resolve-var (empty-env nsym) sym))))

(defn cljs-ns []
  (assert-cljs-dep)
  (let [*cljs-ns* (impl/v 'cljs.analyzer/*cljs-ns*)]
    *cljs-ns*))

(defn emit-form [f]
  (assert-cljs-dep)
  (let [emit-form (impl/v 'cljs.tools.analyzer.emit-form/emit-form)]
    (emit-form f)))
