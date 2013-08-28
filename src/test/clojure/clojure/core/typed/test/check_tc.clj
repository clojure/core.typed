(ns clojure.core.typed.test.check-tc
  (:require [clojure.test :refer :all]
            [clojure.core.typed :refer [check-ns]]))

; re-enable when recursive protocols have been resolved.
#_(deftest check-tc
  (is (check-ns 'clojure.core.typed.utils))
  (is (check-ns 'clojure.core.typed.type-rep))
  (is (check-ns 'clojure.core.typed.cs-rep))
  (is (check-ns 'clojure.core.typed.name-env))
  (is (check-ns 'clojure.core.typed.type-ctors)))
