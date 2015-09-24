(ns clojure.core.typed.test.cljc
  (:require [clojure.test :refer :all]
            [clojure.core.typed :as t]))

(deftest check-ns-should-load-cljc-files
  (is (t/check-ns 'clojure.core.typed.test.dummy-cljc :collect-only true)))

(deftest check-ns-should-load-clj-files-before-cljc
  (is (t/check-ns 'clojure.core.typed.test.duplicated :collect-only true)))
