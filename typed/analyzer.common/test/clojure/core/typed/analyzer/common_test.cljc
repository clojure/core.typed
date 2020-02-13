(ns clojure.core.typed.analyzer.common-test
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t]
               :default (throw (ex-info "Unknown impl" {})))
            [clojure.core.typed.analyzer.common :as ana]))

(prn "common test")

(t/deftest fake-test
  (t/is (= 1 1)))
