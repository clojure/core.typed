(ns ^:skip-wiki clojure.core.typed.util-cljs
  (:require [clojure.core.typed.current-impl :as impl]
            [cljs.analyzer :as ana]
            [cljs.jvm.tools.analyzer.emit-form :as emit-form]
            [cljs.compiler :as comp]
            [cljs.env :as env]))

(alter-meta! *ns* assoc :skip-wiki true)

(def default-env (env/default-compiler-env))

(defmacro with-cljs-typed-env [& body]
  `(env/with-compiler-env (or env/*compiler* default-env)
     ~@body))

(defn var-exists? [env prefix suffix]
  (let [compiler env/*compiler*
        _ (assert compiler)]
    (contains? (get-in @compiler [::ana/namespaces prefix :defs])
               suffix)))

(defn resolve-var [nsym sym]
  {:post [((some-fn symbol? nil?) %)]}
  (let [unresolved? (atom false)
        r (with-cljs-typed-env
            (binding [ana/*cljs-ns* nsym]
              (comp/with-core-cljs
                nil
                #(ana/resolve-var (ana/empty-env) sym
                                  (fn [env ns sym]
                                    (when-not (var-exists? env ns sym)
                                      (reset! unresolved? true)))))))
        sym* (when-not @unresolved?
               (:name r))
        _ (when sym*
            (assert (symbol? sym*) sym*)
            (assert (namespace sym*) sym*))]
    ;(prn sym sym*)
    sym*))

(defn cljs-ns []
  ana/*cljs-ns*)

(defn emit-form [f]
  (emit-form/emit-form f))

(defmacro with-core-cljs-typed [& body]
  `(comp/with-core-cljs
     nil
     #(do (when-not (get-in @env/*compiler* [::ana/namespaces 'cljs.core.typed :defs])
            (ana/analyze-file "cljs/core/typed.cljs"))
          ~@body)))
