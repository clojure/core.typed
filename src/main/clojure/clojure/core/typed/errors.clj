(ns ^:skip-wiki clojure.core.typed.errors
  (:require [clojure.core.typed.util-vars :refer [*current-env*] :as uvs]))

(alter-meta! *ns* assoc :skip-wiki true)

(def int-error-kw ::internal-error)

(defn int-error
  [estr]
  (let [env *current-env*]
    (throw (ex-info (str "Internal Error "
                         "(" (or (:file env) (-> env :ns :name)) ":" 
                         (or (:line env) "<NO LINE>")
                         (when-let [col (:column env)]
                           (str ":" col))
                         ") "
                         estr)
                    {:type-error int-error-kw}))))

