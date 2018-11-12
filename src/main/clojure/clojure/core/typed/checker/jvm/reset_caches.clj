;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.jvm.reset-caches
  (:require [clojure.core.typed.checker.jvm.subtype :as sub]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.cs-gen :as cgen]))

(defn reset-caches 
  "Reset internal type caches."
  []
  (sub/reset-subtype-cache)
  (c/reset-Un-cache)
  (c/reset-In-cache)
  (c/reset-supers-cache!)
  (c/reset-RClass-of-cache!)
  (cgen/reset-dotted-var-store!)
  nil)
