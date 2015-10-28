(ns clojure.core.typed.test.ctyp-284
  (:require [clojure.test :refer :all]
            [clojure.core.typed.load :as load]
            [clojure.core.typed.lang :as lang]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.test.test-utils :refer :all]))

(deftest transitive-no-reload
  (is 
    ;; doing this twice seems to help
    (do
      ;; first time, throw away error
      (try 
        (with-typed-load 
          (and
            (try
              (require 'clojure.core.typed.test.typed-load.uses-bad-dep :reload-all)
              (catch Throwable _ true))
            (nil?
              (require 'clojure.core.typed.test.typed-load.uses-bad-dep))))
        (catch Throwable _ true))
      ;; second time
      (with-typed-load 
        (and
          ;; reload-all should throw a type error in bad.dep
          (try
            (require 'clojure.core.typed.test.typed-load.uses-bad-dep :reload-all)
            (catch Throwable _ true))
          ;; normal require doesn't reload
          (nil?
            (require 'clojure.core.typed.test.typed-load.uses-bad-dep)))))))
