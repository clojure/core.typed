(ns clojure.core.typed.test.cljs
  (:require [clojure.test :refer :all]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.parse-unparse :as prs])
  (:import (clojure.lang ISeq ASeq IPersistentVector Atom IPersistentMap
                         Keyword ExceptionInfo Symbol Var)))

(defmacro cljs [& body]
  `(impl/with-cljs-impl
     ~@body))

;(deftest parse-number-cljs
;  (is (prs/parse-cljs 'number)))
