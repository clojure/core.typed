(ns clojure.core.typed.hset-utils
  (:require [clojure.core.typed.contract-utils :as con]))

(def valid-fixed? (some-fn string? symbol? keyword? nil? number?
                           con/character? con/boolean?))
