;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.def-utils
  (:refer-clojure :exclude [defrecord defprotocol definterface])
  (:require [clojure.core :as core]))

(defmacro defprotocol [name & args]
  ;only define protocol if symbol doesn't resolve, not completely sure if this behaves like defonce
  (when-not (resolve name)
    `(core/defprotocol ~name ~@args)))
