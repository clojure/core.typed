(ns clojure.core.typed.test.jvm.array
  (:require [clojure.core.typed :refer [ann check-ns into-array> cf print-env ann-form]
             :as t]
            [clojure.core.typed.test.test-utils :refer :all]
            [clojure.test :refer :all]
            ))

(deftest array-test
  (is-clj (= (Class/forName "[I") 
             (eval `(class (t/into-array> ~'int [1])))))
  (is-clj (clj (= (Class/forName "[Ljava.lang.Object;") 
                  (eval `(class (t/into-array> (t/U nil ~'int) [1]))))))
  (is-clj (clj (= (Class/forName "[Ljava.lang.Number;") 
                  (eval `(class (t/into-array> (t/U nil Number) [1]))))))
  (is-clj (clj (= (Class/forName "[Ljava.lang.Object;") 
                  (eval `(class (t/into-array> (t/U t/Sym Number) [1]))))))
  (is-clj (= (Class/forName "[Ljava.lang.Object;") 
             (eval `(class (t/into-array> Object (t/U t/Sym Number) [1])))))
  )

(deftest array-primitive-hint-test
  (is-tc-e (let [^ints a (t/into-array> int [(int 1)])]
             (alength a))))

; FIXME this is wrong, should not just be nil
#_(deftest array-first-test
    (is-cf (let [a (clojure.core.typed/into-array> Long [1 2])]
             (first a))))

(deftest array-reflection-test
  (is-tc-e (fn make-process [script]
             {:post [%]}
             (let [^Runtime r (Runtime/getRuntime)
                   _ (assert r)
                   ^"[Ljava.lang.String;" arr (into-array> String ["echo 'hello'"])]
               (.exec r arr)))
           [String -> java.lang.Process]))
