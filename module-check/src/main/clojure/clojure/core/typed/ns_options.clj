(ns ^:skip-wiki clojure.core.typed.ns-options
  (:require [clojure.core.typed :as t]))

(alter-meta! *ns* assoc :skip-wiki true)

(t/defalias NsOptions
  "Options for namespaces"
  (t/HMap :optional
          {:warn-on-unannotated-vars Boolean}))

(t/defalias OptMap
  (t/Map t/Sym NsOptions))

(t/ann init-ns-opts [-> OptMap])
(defn init-ns-opts []
  {})

(t/ann ns-opts (t/Atom1 OptMap))
(defonce ns-opts (atom (init-ns-opts)))

(t/ann reset-ns-opts! [-> nil])
(defn reset-ns-opts! []
  (reset! ns-opts (init-ns-opts))
  nil)

(t/ann ^:no-check register-warn-on-unannotated-vars [t/Sym -> nil])
(defn register-warn-on-unannotated-vars [nsym]
  (swap! ns-opts 
         (t/fn [o :- NsOptions] 
           (update-in o [nsym :warn-on-unannotated-vars] (constantly true))))
  nil)

(t/ann ^:no-check warn-on-unannotated-vars? [t/Sym -> Boolean])
(defn warn-on-unannotated-vars? [nsym]
  (boolean (:warn-on-unannotated-vars (@ns-opts nsym))))
