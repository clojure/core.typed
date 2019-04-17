;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.invoke
  (:require [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.check.funapp :as funapp]))

(defn normal-invoke [check-fn expr fexpr args expected & {:keys [cfexpr cargs]}]
  (let [cfexpr (or cfexpr
                   (check-fn fexpr))
        cargs (or cargs
                  (mapv check-fn args))
        ftype (u/expr-type cfexpr)
        argtys (map u/expr-type cargs)
        actual (funapp/check-funapp fexpr args ftype argtys expected :check-fn check-fn)]
    (assoc expr
           :fn cfexpr
           :args cargs
           u/expr-type actual)))
