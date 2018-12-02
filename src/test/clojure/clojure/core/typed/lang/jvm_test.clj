(ns clojure.core.typed.lang.jvm-test
  (:require [clojure.core.typed.lang.jvm :as lang]
            [clojure.test :refer :all]))

(deftest stub
  (is (map? lang/lang-dispatch)))
