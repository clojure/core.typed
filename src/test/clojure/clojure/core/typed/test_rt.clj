(ns clojure.core.typed.test-rt
  (:require [clojure.core.typed :as t]
            [cljs.core.typed :as tcljs]
            [clojure.core.typed.errors :as err]
            [clojure.java.io :as io])
  (:use clojure.test))

(deftest typed-clojure-loaded
  (is (nil? (require 'clojure.core.typed))))

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

(defmacro thrown-blame? [& e]
  `(try (try (do ~@e)
             false
             (catch clojure.lang.Compiler$CompilerException e#
               (throw (.source e#))))
        (catch clojure.lang.ExceptionInfo e#
          (boolean (-> e# ex-data :blame)))))

; commented - these require c.t.lib.clojure which is not a dependency of this project atm

#_
(deftest pred-test
  ;pred forces a few namespaces to load
  (is ((t/pred Number) 1)))

#_
(deftest cast-test
  (is (= 1 (t/cast t/Int 1)))
  (is (= nil (t/cast nil nil)))
  (is (= 1 (t/cast t/Int 1)))
  ;; unions
  (is (thrown-blame? (t/cast t/Int nil 
                             {:positive '+ve 
                              :negative '-ve
                              :file "my/file.clj"
                              :line 20
                              :column 30})))
  (is (= 1 (t/cast (t/U t/Int) 1)))
  (is (thrown-blame? (t/cast (t/U t/Int) nil)))
  (is (thrown-blame? (t/cast (t/U t/Bool t/Int) nil)))
  (is (= 1 (t/cast (t/U (t/U t/Int)) 1)))
  (is (thrown-blame? (t/cast (t/U (t/U t/Int)) nil)))
  ;; intersections
  (is (= 1 (t/cast (t/I t/Int) 1)))
  (is (thrown-blame? (t/cast (t/I t/Int) nil)))
  (is (= 1 (t/cast (t/I (t/I t/Int)) 1)))
  (is (thrown-blame? (t/cast (t/I (t/I t/Int)) nil)))

  (is (thrown-blame? (t/cast (t/I (t/I t/Int)) nil)))
  )
