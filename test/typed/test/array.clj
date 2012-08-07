(ns typed.test.pomegranate
  (:import (clojure.lang DynamicClassLoader Named Seqable IPersistentVector))
  (:require [typed.core :refer [ann check-ns override-method tc-pr-env ann-protocol
                                tc-ignore non-nil-return nilable-param]]
            [clojure.repl :refer [pst]]))

(

(defn readable-array [a]
