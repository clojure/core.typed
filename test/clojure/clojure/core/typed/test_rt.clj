(ns clojure.core.typed.test-rt
  (:require [clojure.core.typed :as t]
            [cljs.core.typed :as tcljs]
            [clojure.core.typed.errors :as err]
            [clojure.java.io :as io])
  (:use clojure.test))

(deftest typed-clojure-loaded
  (is clojure.core.typed/load-if-needed)
  ;pred forces a few namespaces to load
  (is ((t/pred Number) 1))
  (println "Successfully required TC without dependencies"))

(deftest async-ns
  (is (io/resource "clojure/core/typed/async.clj")))

(deftest checking-ops
  (is (err/tc-error-thrown?
        (t/load-if-needed)))
  (is (err/tc-error-thrown?
        (t/reset-caches)))
  (is (err/tc-error-thrown?
        (t/method-type 'foo)))
  (is (err/tc-error-thrown?
        (t/into-array> 'foo 'bar [1])))
  (is (err/tc-error-thrown?
        (t/cf 1)))
  (is (err/tc-error-thrown?
        (t/check-form* 1)))
  (is (err/tc-error-thrown?
        (t/check-form-info 1)))
  (is (err/tc-error-thrown?
        (t/check-ns 'foo)))
  (is (err/tc-error-thrown?
        (t/check-ns-info 'foo)))
  (is (err/tc-error-thrown?
        (t/statistics ['foo])))
  (is (err/tc-error-thrown?
        (t/var-coverage))))

(defmacro catch-compiler-exception
  [& body]
  `(try (do ~@body
            nil)
        (catch RuntimeException e#
          (if (instance? clojure.lang.ExceptionInfo e#)
            ; before clojure 1.7.0-alpha2
            (err/tc-error-thrown?
              (throw e#))
            ; clojure 1.7.0-alpha2
            (err/tc-error-thrown?
              (throw (.getCause e#)))))))

(deftest checking-cljs-ops
  (is (err/tc-error-thrown?
        (tcljs/load-if-needed)))
  (is (err/tc-error-thrown?
        (tcljs/reset-caches)))
  (is (err/tc-error-thrown?
        (tcljs/cf* 1 nil nil)))
  (is (err/tc-error-thrown?
        (tcljs/cf 1)))
  (is (err/tc-error-thrown?
        (tcljs/check-ns*)))
  (is (err/tc-error-thrown?
        (tcljs/check-ns* 'foo)))
  ; these throw at macroexpansion time
  (is (catch-compiler-exception
        (eval '(cljs.core.typed/check-ns))))
  (is (catch-compiler-exception
        (eval '(cljs.core.typed/check-ns foo)))))
