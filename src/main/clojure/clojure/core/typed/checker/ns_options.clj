;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.ns-options
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
