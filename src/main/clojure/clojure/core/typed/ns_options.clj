(ns ^:skip-wiki clojure.core.typed.ns-options
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.env :as env]))

(t/defalias NsOptions
  "Options for namespaces"
  (t/HMap :optional
          {:warn-on-unannotated-vars Boolean}))

(t/defalias OptMap
  (t/Map t/Sym NsOptions))

(t/ann reset-ns-opts! [-> nil])
(defn reset-ns-opts! []
  (env/swap-checker! assoc impl/ns-opts-kw {})
  nil)

(t/ann ^:no-check register-warn-on-unannotated-vars [t/Sym -> nil])
(def register-warn-on-unannotated-vars impl/register-warn-on-unannotated-vars)

(defn get-ns-opts [nsym]
  {:post [(map? %)]}
  (get-in (env/deref-checker) [impl/ns-opts-kw nsym] {}))

(t/ann ^:no-check warn-on-unannotated-vars? [t/Sym -> Boolean])
(defn warn-on-unannotated-vars? [nsym]
  (boolean (:warn-on-unannotated-vars (get-ns-opts nsym))))
