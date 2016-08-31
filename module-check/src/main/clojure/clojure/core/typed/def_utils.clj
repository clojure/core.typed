(ns clojure.core.typed.def-utils
  (:refer-clojure :exclude [defrecord defprotocol definterface])
  (:require [clojure.core :as core]))

(defmacro defprotocol [name & args]
  ;only define protocol if symbol doesn't resolve, not completely sure if this behaves like defonce
  (when-not (resolve name)
    `(core/defprotocol ~name ~@args)))
