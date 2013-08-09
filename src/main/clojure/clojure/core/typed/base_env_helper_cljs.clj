(ns clojure.core.typed.base-env-helper-cljs
  (:refer-clojure :exclude [type])
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.declared-kind-env :as decl-env]
            [clojure.core.typed.rclass-env :as rcls]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.name-env]
            [clojure.pprint :as pprint]))

(defmacro alias-mappings [& args]
  `(impl/with-cljs-impl
     (let [ts# (partition 2 '~args)]
       (into {}
             (doall
               (for [[s# t#] ts#]
                 (let [desc# (-> s# meta :doc)
                       doc# (str (when desc#
                                   (str desc# "\n\n")) 
                                 (with-out-str (pprint/pprint t#)))
                       _# (assert (and (symbol? s#)
                                       (namespace s#))
                                  "Need fully qualified symbol")
                       v# (intern (find-ns (symbol (namespace s#))) (symbol (name s#)))
                       _# (alter-meta! v# merge {:doc doc#})]
                   [(with-meta s# nil) (prs/parse-type t#)])))))))

(defmacro var-mappings [& args]
  `(impl/with-cljs-impl
     (let [ts# (partition 2 '~args)]
       (into {}
             (doall
               (for [[s# t#] ts#]
                 (do
                   (assert (and (symbol? s#)
                                (namespace s#))
                           "Need fully qualified symbol")
                   [s# (prs/parse-type t#)])))))))
