(ns clojure.core.typed.base-env-common
  "Utilities for all implementations of the type checker")

(defmacro delay-and-cache-env [sym & body]
  (let [generator-sym (symbol (str "generator-" sym))
        cache-sym (symbol (str "cache-" sym))
        interface-sym sym]
    `(do
       (defn ~(with-meta generator-sym {:private true}) []
         ; switch namespace to where this def is defined
         (binding [*ns* (find-ns '~(ns-name *ns*))]
           ~@body))
       ; cache is original nil, then is updated only once
       (def ~(with-meta cache-sym {:private true})
         (atom nil))
       (defn ~interface-sym [] 
         (if-let [hit# (deref ~cache-sym)]
           hit#
           (let [calc# (~generator-sym)]
             (reset! ~cache-sym calc#)))))))
