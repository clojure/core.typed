;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.jvm.rclass-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.checker.type-rep :as r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restricted Class

;Class -> RClass
(defonce RESTRICTED-CLASS (atom {}))
(set-validator! RESTRICTED-CLASS (con/hash-c? symbol? r/Type?))

(defn get-rclass 
  "Returns the RClass with class symbol csym.
  Returns nil if not found."
  [csym]
  (@RESTRICTED-CLASS csym))

(defn alter-class* [csym type]
  (assert (r/Type? type)
          (str "alter-class* " csym " not a type: " type))
  (swap! RESTRICTED-CLASS assoc csym type))

(defn reset-rclass-env! [m]
  (reset! RESTRICTED-CLASS m)
  nil)
