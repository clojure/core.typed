(ns clojure.core.typed.util-cljs
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.load-cljs :as load]))

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

(defn ^:private empty-env-in-ns [nsym]
  {:ns {:name nsym} :context :statement :locals {}})

(defn resolve-var [nsym sym]
  (assert-cljs-dep)
  (with-core-cljs
    (let [namespaces (impl/v 'cljs.analyzer/namespaces)
          resolve-var (impl/v 'cljs.analyzer/resolve-var)]
      (assert (contains? @namespaces nsym)
              (str "Namespace " nsym " does not exist"))
      (resolve-var (empty-env-in-ns nsym) sym))))

(defn cljs-ns []
  (assert-cljs-dep)
  (let [*cljs-ns* (impl/v 'cljs.analyzer/*cljs-ns*)]
   *cljs-ns*))


