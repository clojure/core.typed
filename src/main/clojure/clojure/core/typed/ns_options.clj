(ns ^:skip-wiki clojure.core.typed.ns-options
  (:require [clojure.core.typed :as t :refer [fn>]])
  (:import (clojure.lang IPersistentMap Symbol)))

(alter-meta! *ns* assoc :skip-wiki true)

(t/def-alias NsOptions
  "Options for namespaces"
  (HMap :optional
        {:warn-on-unannotated-vars Boolean}))

(t/ann init-ns-opts [-> (IPersistentMap Symbol NsOptions)])
(defn init-ns-opts []
  {})

(t/ann ns-opts (t/Atom1 (IPersistentMap Symbol NsOptions)))
(defonce ns-opts (atom (init-ns-opts)))

(t/ann reset-ns-opts! [-> nil])
(defn reset-ns-opts! []
  (reset! ns-opts (init-ns-opts))
  nil)

(t/ann ^:no-check register-warn-on-unannotated-vars [Symbol -> nil])
(defn register-warn-on-unannotated-vars [nsym]
  (swap! ns-opts 
         (fn> [o :- NsOptions] 
           (update-in o [nsym :warn-on-unannotated-vars] (constantly true))))
  nil)

(t/ann ^:no-check warn-on-unannotated-vars? [Symbol -> Boolean])
(defn warn-on-unannotated-vars? [nsym]
  (boolean (:warn-on-unannotated-vars (@ns-opts nsym))))
