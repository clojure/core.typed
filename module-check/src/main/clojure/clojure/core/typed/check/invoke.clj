(ns clojure.core.typed.check.invoke
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.check.funapp :as funapp]))

(defn normal-invoke [check-fn expr fexpr args expected & {:keys [cfexpr cargs]}]
  (u/p :check/normal-invoke
  (let [cfexpr (or cfexpr
                   (check-fn fexpr))
        cargs (or cargs
                  (mapv check-fn args))
        ftype (u/expr-type cfexpr)
        argtys (map u/expr-type cargs)
        actual (funapp/check-funapp fexpr args ftype argtys expected)]
    (assoc expr
           :fn cfexpr
           :args cargs
           u/expr-type actual))))
