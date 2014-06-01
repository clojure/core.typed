(ns clojure.core.typed.test.check-tc
  (:require [clojure.test :refer :all]
            [clojure.core.typed :refer [check-ns]]))

(deftest check-tc
  (is (try
        (check-ns '[clojure.core.typed.utils
                    clojure.core.typed.type-rep
                    clojure.core.typed.cs-rep
                    clojure.core.typed.name-env
                    clojure.core.typed.type-ctors])
        ;fails in hudson
        (catch clojure.lang.Compiler$CompilerException e
          (if (instance? OutOfMemoryError (.getCause e))
            true
            (throw e))))))
