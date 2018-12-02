(ns clojure.core.typed.test.ctyp-284
  (:require [clojure.test :refer :all]
            [clojure.core.typed.test.test-utils :refer :all]))

(deftest transitive-no-reload
  (is 
    (with-typed-load 
      (and
        ; bad dep throws a type error
        (try
          (require 'clojure.core.typed.test.typed-load.bad-dep :reload)
          nil
          (catch Throwable _ true))
        ; but isn't rechecked by transitive dep
        (nil? (require 'clojure.core.typed.test.typed-load.uses-bad-dep :reload))))))
