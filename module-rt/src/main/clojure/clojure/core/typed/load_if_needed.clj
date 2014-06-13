(ns ^:skip-wiki clojure.core.typed.load-if-needed
  (:require [clojure.core.typed.errors :as err]
            [clojure.java.io :as io]
            [clojure.core.typed.util-vars :as vs]))

(alter-meta! *ns* assoc :skip-wiki true)

(defn load-if-needed 
  "Load and initialize all of core.typed if not already"
  []
  (when-not vs/*currently-loading*
    (binding [vs/*currently-loading* true]
      (when-not (io/resource "clojure/core/typed/init.clj")
        (err/int-error "core.typed checker is not found on classpath"))
      (when-not (find-ns 'clojure.core.typed.init)
        (require 'clojure.core.typed.init))
      (let [init-ns (find-ns 'clojure.core.typed.init)]
        (assert init-ns)
        (when-not (@(ns-resolve init-ns 'loaded?))
          (println "Initializing core.typed ...")
          (flush)
          (time (@(ns-resolve init-ns 'load-impl)))
          (println "core.typed initialized.")
          (flush))))))
