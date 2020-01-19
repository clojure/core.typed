;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;copied from clojure.tools.analyzer.passes.cleanup
(ns clojure.core.typed.analyzer.common.passes.cleanup)

(defn cleanup
  {:pass-info {:walk :any :depends #{}}}
  [ast]
  (-> ast
    (update-in [:env] dissoc :loop-locals-casts)
    (update-in [:env :locals] #(reduce-kv (fn [m k l] (assoc m k (dissoc l :env :init))) {} %))
    (dissoc :atom)))
