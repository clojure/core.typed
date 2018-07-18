(ns clojure.core.typed.test.cljc
  (:require [clojure.test :refer :all]
            [clojure.core.typed :as t]
            [clojure.core.typed.load :as load]))

(deftest check-ns-should-load-cljc-files
  (is (t/check-ns 'clojure.core.typed.test.dummy-cljc)))

(deftest check-ns-should-load-clj-files-before-cljc
  (is (t/check-ns 'clojure.core.typed.test.duplicated)))

(deftest CTYP-285-ns-form-in-reader-cond-should-work
  (is (thrown?
        clojure.lang.ExceptionInfo
        (load/typed-load1 "clojure/core/typed/test/ns_reader_cond"))))

(deftest allow-reader-conditional-in-ns-form
  (is (t/check-ns 'clojure.core.typed.test.reader-cond)))
