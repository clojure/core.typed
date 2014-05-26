(ns clojure.core.typed.errors-ann
  (:require [clojure.core.typed :as t]))

(t/ann ^:no-check clojure.core.typed.errors/deprecated-warn [String -> nil])
(t/ann ^:no-check clojure.core.typed.errors/int-error [String -> t/Nothing])

