(ns clojure.core.typed.test.file-mapping
  (:require 
    ; this loads the type system, must go first
    [clojure.core.typed.test.test-utils :refer [check clj]]
    [clojure.core.typed.checker.jvm.analyze-clj :as ana]
    [clojure.core.typed :as t]
    [clojure.core.typed.checker.jvm.check :as chk]))

#_(-> (ana/ast-for-form '(let [a (+ 1 2)] a)) :env ((juxt :line :column)))

#_(-> (ana/ast-for-form '(let [{:as a} {}] a)) :env ((juxt :line :column)))


#_(clj
(clojure.pprint/pprint
           (:file-mapping
             (t/check-ns-info 'clojure.core.typed.test.mapping-test-file
                              :file-mapping true))))
