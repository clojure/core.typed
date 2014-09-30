(ns ^:skip-wiki clojure.core.typed.indirect-ann
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.type-rep :as r]
            clojure.core.typed.indirect-ops))

(t/ann clojure.core.typed.indirect-ops/unparse-type [r/Type -> t/Any])
