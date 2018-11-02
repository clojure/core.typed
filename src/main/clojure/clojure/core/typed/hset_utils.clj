(ns clojure.core.typed.hset-utils)

(def valid-fixed? (some-fn string? symbol? keyword? nil? number?
                           char? boolean?))
