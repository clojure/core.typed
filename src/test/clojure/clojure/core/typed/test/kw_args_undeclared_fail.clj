(ns clojure.core.typed.test.kw-args-undeclared-fail
  (:require [clojure.core.typed :refer [ann check-ns ann-form cf]]
            [clojure.tools.analyzer :refer [ast]]
            [clojure.tools.analyzer.emit-form :refer [emit-form]]))

;; invoking a kw-fn with undeclared keywords is an error
(ann undeclared-kw-invoke-test [& {:foo Any} -> nil])
(defn undeclared-kw-invoke-test [& a])

(undeclared-kw-invoke-test :blah 'a)
