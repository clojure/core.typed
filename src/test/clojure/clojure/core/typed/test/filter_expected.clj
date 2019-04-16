(ns clojure.core.typed.test.filter-expected
  (:refer-clojure :exclude [update cast])
  (:require 
    ; this loads the type system, must go first
    [clojure.core.typed.test.test-utils :refer :all]
            [clojure.core.typed.checker.type-rep :refer :all]
            [clojure.core.typed.checker.jvm.parse-unparse :refer :all]
            [clojure.core.typed.checker.filter-rep :refer :all]
            [clojure.core.typed.checker.filter-ops :refer :all]
            [clojure.core.typed.checker.object-rep :refer :all]
            [clojure.core.typed.checker.path-rep :refer :all]
            [clojure.test :refer :all])
  (:use [clojure.core.typed :as tc :exclude [Seqable loop fn defprotocol let dotimes
                                             for doseq def remove filter defn atom ref]]))

(deftest filter-expected-test
  (testing "integers are truthy"
    (is-tc-e 1
             :expected-ret (ret (parse-clj `Num)
                                (-FS -top -bot)
                                -empty))
    (is-tc-err 1 
               :expected-ret (ret (parse-clj `Num)
                                  (-FS -bot -top)
                                  -empty))
    (is-tc-err 1 
               :expected-ret (ret (parse-clj `Num)
                                  (-FS -bot -bot)
                                  -empty))
    (testing "checks object"
      (is-tc-err 1 
                 :expected-ret (ret (parse-clj `Num)
                                    (-FS -top -top)
                                    (-path nil 'a))))
    (is-tc-err 1
               :expected-ret (ret (parse-clj `Sym)))
    (is (= (ret (parse-clj `Num)
                (-true-filter))
           (tc-e 1
                 :expected-ret
                 (ret (parse-clj `Num)
                      (-true-filter)
                      -no-object)))))
  (testing "nil is falsy"
    (is-tc-e nil
             :expected-ret (ret -nil
                                (-FS -bot -top)
                                -empty))
    (is-tc-err nil
               :expected-ret (ret -nil
                                  (-FS -top -bot)
                                  -empty)))
  (testing "false is falsy"
    (is-tc-e false
             :expected-ret (ret -false
                                (-FS -bot -top)))
    (is-tc-e false
             :expected-ret (ret -false
                                (-FS -bot -top)))
    (is-tc-err false
             :expected-ret (ret -false
                                (-FS -top -bot))))
  (testing "conditionals"
    (is-tc-e (if 1 2 3) Num)
    (is-tc-err (if 1 2 3) Sym)
    (is-tc-e (if 1 2 3) 
             :expected-ret (ret (parse-clj `Num)
                                (-FS -top -bot)))
    (is-tc-e (if 1 nil nil) 
             :expected-ret (ret (parse-clj `nil)
                                (-FS -bot -top)))
    (is-tc-err (if 1 2 3) 
               :expected-ret (ret (parse-clj `Num)
                                  (-FS -bot -top)))
    (is-tc-e (fn [a] (if a nil nil))
             [Any -> nil :filters {:then ff :else tt}])
    (is-tc-e (fn [a b] (if a a a))
             [Any Any -> Any :object {:id 0}])
    (is-tc-err (fn [a b] (if b b b))
               [Any Any -> Any :object {:id 0}]))
  (testing "functions are truthy"
    (is-tc-e (fn [])
             :expected-ret (ret -any
                                (-true-filter)))
    (is-tc-err (fn [])
             :expected-ret (ret -any
                                (-false-filter)))
    ;TODO
    #_(is-tc-e (core/fn [])
             :expected-ret (ret -any
                                (-false-filter))))
  (testing "quote"
    (is-tc-e 'a 
             :expected-ret
             (ret (parse-clj `Sym)
                  (-true-filter)))
    (is-tc-err 'a 
             :expected-ret
             (ret (parse-clj `Sym)
                  (-false-filter)))
    (is-tc-err 'a 
             :expected-ret
             (ret (parse-clj `Sym)
                  (-true-filter)
                  (-path nil 'a)))
    (is-tc-e ''a 
             :expected-ret
             (ret (parse-clj `(Coll Sym))
                  (-true-filter)
                  ))
    (is-tc-e '''a 
             :expected-ret
             (ret (parse-clj `(Coll (U Sym (Coll Sym))))
                  (-true-filter)))
    )
  (testing "do"
    (is-tc-e (do 1 2)
             :expected-ret
             (ret (parse-clj `Num)
                  (-true-filter)))
    (is-tc-err (do 1 2)
             :expected-ret
             (ret (parse-clj `Num)
                  (-false-filter))))
  (testing "let"
    (is-tc-e (let [] 1)
             :expected-ret
             (ret (parse-clj `Num)
                  (-true-filter)))
    (is-tc-err (let [] 1)
             :expected-ret
             (ret (parse-clj `Num)
                  (-false-filter)))
    (is-tc-err (let [] 1)
             :expected-ret
             (ret (parse-clj `Num)
                  (-true-filter)
                  (-path nil 'a))))
  (testing "map values"
    (is-tc-e {:a 1}
             :expected-ret
             (ret (parse-clj `'{:a Num})
                  (-true-filter)))
    (is-tc-err {:a 1}
             :expected-ret
             (ret (parse-clj `'{:a Num})
                  (-false-filter)))
    (is-tc-err {:a 1}
             :expected-ret
             (ret (parse-clj `'{:a Num})
                  (-true-filter)
                  (-path nil 'a)))
    )
  (testing "map expressions"
    (is-tc-e (let [a 1] {:a a})
             :expected-ret
             (ret (parse-clj `'{:a Num})
                  (-true-filter)))
    (is-tc-err (let [a 1] {:a a})
             :expected-ret
             (ret (parse-clj `'{:a Num})
                  (-false-filter)))
    (is-tc-err (let [a 1] {:a a})
             :expected-ret
             (ret (parse-clj `'{:a Num})
                  (-true-filter)
                  (-path nil 'a)))
    (is-tc-err (let [a 1] {:a a})
             :expected-ret
             (ret (parse-clj `'{:a Num})
                  (-true-filter)
                  -empty
                  (-flow -bot)))
    )
  (testing "set values"
    (is-tc-e #{1})
    (is-tc-e #{1} (Set Num))
    (is-tc-err #{1} 
               (Set Sym))
    (is-tc-e #{1} 
               :expected-ret
               (ret (parse-clj `(Set Num))
                    (-true-filter)))
    (is-tc-err #{1} 
               :expected-ret
               (ret (parse-clj `(Set Num))
                    (-false-filter)))
    (is-tc-err #{1} 
               :expected-ret
               (ret (parse-clj `(Set Num))
                    (-true-filter)
                    (-path nil 'a)))
    (is-tc-err #{1} 
               :expected-ret
               (ret (parse-clj `(Set Num))
                    (-true-filter)
                    -empty
                    (-flow -bot)))
    )
  (testing "set expression"
    (is-tc-e (let [a 1]
                 #{a}))
    (is-tc-e (let [a 1]
                 #{a}) (Set Num))
    (is-tc-err (let [a 1]
                 #{a})
               (Set Sym))
    (is-tc-e (let [a 1]
                 #{a})
               :expected-ret
               (ret (parse-clj `(Set Num))
                    (-true-filter)))
    (is-tc-err (let [a 1]
                 #{a})
               :expected-ret
               (ret (parse-clj `(Set Num))
                    (-false-filter)))
    (is-tc-err (let [a 1]
                 #{a})
               :expected-ret
               (ret (parse-clj `(Set Num))
                    (-true-filter)
                    (-path nil 'a)))
    (is-tc-err (let [a 1]
                 #{a})
               :expected-ret
               (ret (parse-clj `(Set Num))
                    (-true-filter)
                    -empty
                    (-flow -bot)))
    )
  (testing "vector values"
    (is-tc-e [1])
    (is-tc-e [1] (Vec Num))
    (is-tc-err [1]
               (Vec Sym))
    (is-tc-e [1]
               :expected-ret
               (ret (parse-clj `(Vec Num))
                    (-true-filter)))
    (is-tc-err [1]
               :expected-ret
               (ret (parse-clj `(Vec Num))
                    (-false-filter)))
    (is-tc-err [1]
               :expected-ret
               (ret (parse-clj `(Vec Num))
                    (-true-filter)
                    (-path nil 'a)))
    (is-tc-err [1]
               :expected-ret
               (ret (parse-clj `(Vec Num))
                    (-true-filter)
                    -empty
                    (-flow -bot)))
    )
  (testing "vector expressions"
    (is-tc-e (let [a 1]
               [a]))
    (is-tc-e (let [a 1]
               [a]) (Vec Num))
    (is-tc-err (let [a 1]
                 [a])
               (Vec Sym))
    (is-tc-e (let [a 1]
               [a])
             :expected-ret
             (ret (parse-clj `(Vec Num))
                  (-true-filter)))
    (is-tc-err (let [a 1]
                 [a])
               :expected-ret
               (ret (parse-clj `(Vec Num))
                    (-false-filter)))
    (is-tc-err (let [a 1]
                 [a])
               :expected-ret
               (ret (parse-clj `(Vec Num))
                    (-true-filter)
                    (-path nil 'a)))
    (is-tc-err (let [a 1]
                 [a])
               :expected-ret
               (ret (parse-clj `(Vec Num))
                    (-true-filter)
                    -empty
                    (-flow -bot)))
    )
  (testing "ann-form"
    (is-tc-e (ann-form 1 Num)
             :expected-ret
             (ret (parse-clj `Num)
                  (-true-filter)
                  -empty))
    (is-tc-err (ann-form 1 Num)
             :expected-ret
             (ret (parse-clj `Num)
                  (-false-filter)
                  -empty))
    (is-tc-err (ann-form 1 Num)
             :expected-ret
             (ret (parse-clj `Num)
                  (-true-filter)
                  -empty
                  (-flow -bot)))
)
  (testing "loop"
    (is-tc-e (loop [a :- Num 1] a)
             :expected-ret
             (ret (parse-clj `Num)))
    (is-tc-e (loop [a :- Num 1] a)
             :expected-ret
             (ret (parse-clj `Num)
                  (-true-filter)
                  ))
    (is-tc-err (loop [a :- Num 1] a)
             :expected-ret
             (ret (parse-clj `Num)
                  (-false-filter)
                  ))
    ;TODO better gensyms?
    #_(is-tc-err (loop [a :- Num 1] a)
             :expected-ret
             (ret (parse-clj `Num)
                  (-FS -top -top)
                  (-path nil 'a__#0)))
    )
  (testing "application"
    (is-tc-e ((fn []))
             :expected-ret
             (ret (parse-clj 'nil)))
    (is-tc-err ((fn []))
             :expected-ret
             (ret (parse-clj 'nil)
                  (-true-filter)))
    (is-tc-e ((fn []))
             :expected-ret
             (ret (parse-clj 'nil)
                  (-false-filter)))
    (is-tc-err ((fn []))
             :expected-ret
             (ret (parse-clj 'nil)
                  (-false-filter)
                  (-path nil 'a)))
    )

  (testing "instance method"
    (is-tc-e (.getParent (java.io.File. "a"))
             :expected-ret
             (ret (parse-clj `(U nil Str))))
    (is-tc-err (.getParent (java.io.File. "a"))
             :expected-ret
             (ret (parse-clj `(U nil Str))
                  (-true-filter)))
    (is-tc-err (.getParent (java.io.File. "a"))
             :expected-ret
             (ret (parse-clj `(U nil Str))
                  (-false-filter)))
    (is-tc-err (.getParent (java.io.File. "a"))
             :expected-ret
             (ret (parse-clj `(U nil Str))
                  (-FS -top -top)
                  (-path nil 'a)))
    )
  (testing "static fields"
    (is-tc-e Long/SIZE
             :expected-ret
             (ret (parse-clj `Num)))
    (is-tc-err Long/SIZE
             :expected-ret
             (ret (parse-clj `Sym)))
    (is-tc-err Long/SIZE
             :expected-ret
             (ret (parse-clj `Num)
                  (-false-filter)))
    (is-tc-err Long/SIZE
             :expected-ret
             (ret (parse-clj `Num)
                  (-true-filter)))
    (is-tc-err Long/SIZE
             :expected-ret
             (ret (parse-clj `Num)
                  (-FS -top -top)
                  (-path nil 'a)))
    )
  (testing "instance fields"
    (is-tc-e (do (ann-datatype A [a :- Num])
                 (deftype A [a])
                 (.a (A. 1)))
             :expected-ret
             (ret (parse-clj `Num)))
    ; ctor call in method
    (is-tc-e (do (ann-datatype A [a :- Num])
                 (deftype A [a]
                   Object
                   (toString [this]
                     (A. 1)
                     "foo"))
                 (.a (A. 1)))
             :expected-ret
             (ret (parse-clj `Num)))
    (is-tc-err (do (ann-datatype A [a :- Num])
                 (deftype A [a])
                 (.a (A. 1)))
             :expected-ret
             (ret (parse-clj `Num)
                  (-true-filter)))
    (is-tc-err (do (ann-datatype A [a :- Num])
                 (deftype A [a])
                 (.a (A. 1)))
             :expected-ret
             (ret (parse-clj `Num)
                  (-false-filter)))
    (is-tc-err (do (ann-datatype A [a :- Num])
                 (deftype A [a])
                 (.a (A. 1)))
             :expected-ret
             (ret (parse-clj `Num)
                  (-FS -top -top)
                  (-path nil 'a)))
    )
  (testing "static methods"
    (is-tc-e (Long/valueOf 1)
             :expected-ret
             (ret (parse-clj `(U nil Num))))
    (is-tc-err (Long/valueOf 1)
             :expected-ret
             (ret (parse-clj `(U nil Num))
                  (-true-filter)))
    (is-tc-err (Long/valueOf 1)
             :expected-ret
             (ret (parse-clj `(U nil Num))
                  (-false-filter)))
    (is-tc-err (Long/valueOf 1)
             :expected-ret
             (ret (parse-clj `(U nil Num))
                  (-FS -top -top)
                  (-path nil 'a)))
    )
  (testing "instance? call"
    (is-tc-e (instance? Long 1)
             Boolean)
    ;TODO scoping
    #_(is-tc-err (let [a 1]
                 (instance? Long a))
               :expected-ret
               (ret (parse-clj `Boolean)
                    (-FS (-filter (parse-clj `Long)
                                  'a__#0)
                         (-not-filter (parse-clj `Long)
                                  'a__#0))))
    (is-tc-e (fn [a] (instance? Long a))
             (Pred Long))
    )
  (testing "multifn"
    ;FIXME
    #_(is-tc-e (clojure.lang.MultiFn. 'foo
                                    class
                                    :default
                                    #'clojure.core/global-hierarchy)
             :expected-ret
             (ret (parse-clj `[Any :-> Any]))
             ))
  (testing "new"
    (is-tc-e (Boolean. true)
             Boolean)
    (is-tc-e (Boolean. true)
             :expected-ret
             (ret (parse-clj `Boolean)
                  (-true-filter)))
    (is-tc-err (Boolean. true)
             :expected-ret
             (ret (parse-clj `Boolean)
                  (-false-filter)))
    (is-tc-err (Boolean. true)
             :expected-ret
             (ret (parse-clj `Boolean)
                  (-true-filter)
                  (-path nil 'a)))
    )
  (testing "throw"
    (is-tc-e (fn [a :- Throwable] :- Nothing
               (throw a)))
    (is-tc-err (fn [a :- Any]
                 (throw a)))
    (is-tc-e (fn [a]
               (throw a))
             [Throwable -> Nothing
              :filters {:then ff :else ff}])
    (is-tc-e (fn [a]
               (throw a))
             [Throwable -> Nothing
              :flow ff])
    (is-tc-e (fn [a]
               (throw a))
             [Throwable -> Nothing
              :filters {:then ff :else ff}
              :flow ff])
    (is-tc-err (fn [a]
                 1)
             [Throwable -> Any
              :flow ff])
    (is-tc-err (fn [a]
                 1)
             [Throwable -> Any
              :filters {:then ff :else ff}])
    (is-tc-err (fn [a]
                 1)
               [Throwable -> Any
                :filters {:then ff :else ff}
                :flow ff])
    (is-tc-e (core/fn [a]
               (throw a))
             [Throwable -> Nothing
              :filters {:then ff :else ff}
              :flow ff])
    (is-tc-e (fn [a :- Throwable]
               (throw a))
             [Throwable -> Nothing
              :filters {:then ff :else ff}
              :flow ff])
    (is-tc-err (core/fn [a]
                 1)
             [Throwable -> Any
              :flow ff])
    (is-tc-err (core/fn [a]
                 1)
             [Throwable -> Any
              :filters {:then ff :else ff}])
    (is-tc-err (core/fn [a]
                 1)
               [Throwable -> Any
                :filters {:then ff :else ff}
                :flow ff]))
  (testing "try catch"
    (is-tc-e (try (throw (Exception.))
                  (catch Exception e))
             nil)
    (is-tc-err (try (throw (Exception.))
                  (catch Exception e))
             Num)
    (is-tc-e (try (throw (Exception.))
                  (catch Exception e
                    2))
             Num)
    (is-tc-e (try (throw (Exception.))
                  (catch Exception e))
             :expected-ret
             (ret -nil
                  (-false-filter)))
    (is-tc-err (try (throw (Exception.))
                  (catch Exception e))
             :expected-ret
             (ret -nil
                  (-true-filter)))
    (is-tc-err (try (throw (Exception.))
                  (catch Exception e))
             :expected-ret
             (ret -nil
                  (-FS -top -top)
                  -empty
                  (-flow -bot))))
  (testing "finally"
    (is-tc-e (try (throw (Exception.))
                  (catch Exception e
                    2)
                  (finally nil))
             Num)
    (is-tc-err (try (throw (Exception.))
                  (catch Exception e
                    2)
                  (finally nil))
             nil)
    (is-tc-e (try (throw (Exception.))
                  (catch Exception e
                    2)
                  (finally nil))
             :expected-ret
             (ret (parse-clj `Num)
                  (-true-filter)))
    (is-tc-err (try (throw (Exception.))
                  (catch Exception e
                    2)
                  (finally nil))
             :expected-ret
             (ret (parse-clj `Num)
                  (-false-filter))))
  (testing "var"
    (is-tc-e (do (t/def foo :- Num 1)
                 foo))
    (is-tc-e (do (t/def foo :- Num 1)
                 foo)
             Num)
    (is-tc-err (do (t/def foo :- Num 1)
                 foo)
             nil))
  (testing "set!"
    (is-tc-e (do (t/def ^:dynamic *foo* :- Number 1)
                 (binding [*foo* 1]
                   (set! *foo* 2))))
    (is-tc-err (do (t/def ^:dynamic *foo* :- Number 1)
                   (binding [*foo* 1]
                     (set! *foo* nil))))
    (is-tc-e (do (t/def ^:dynamic *foo* :- Number 1)
                 (binding [*foo* 1]
                   (set! *foo* 2)))
             Num)
    (is-tc-err (do (t/def ^:dynamic *foo* :- Number 1)
                   (binding [*foo* 1]
                     (set! *foo* 2)))
             nil))
  (testing "the var"
    (is-tc-e (do (t/def foo :- Num 1)
                 #'foo))
    (is-tc-e (do (t/def foo :- Num 1)
                 #'foo)
             :expected-ret
             (ret (parse-clj `(Var1 Num))))
    (is-tc-e (do (t/def foo :- Num 1)
                 #'foo)
             :expected-ret
             (ret (parse-clj `(Var1 Num))
                  (-true-filter)))
    (is-tc-err (do (t/def foo :- Num 1)
                 #'foo)
             :expected-ret
             (ret (parse-clj `(Var1 Num))
                  (-false-filter)))
    (is-tc-err (do (t/def foo :- Num 1)
                 #'foo)
             :expected-ret
             (ret (parse-clj `(Var1 Num))
                  (-true-filter)
                  (-path nil 'a)))
    (is-tc-err (do (t/def foo :- Num 1)
                 #'foo)
             :expected-ret
             (ret (parse-clj `(Var1 Num))
                  (-true-filter)
                  -empty
                  (-flow -bot)))
    )
  (testing "cast"
    (is-tc-e (core/cast Number 1))
    (is-tc-e (core/cast Number 1)
             Num)
    (is-tc-err (core/cast Number 1)
             :expected-ret
             (ret (parse-clj `Num)
                  (-true-filter)))
    (is-tc-err (core/cast Number 1)
             :expected-ret
             (ret (parse-clj `Num)
                  (-false-filter)))
    (is-tc-err (core/cast Number 1)
             :expected-ret
             (ret (parse-clj `Num)
                  (-FS -top -top)
                  (-path nil 'a)))
    (is-tc-err (core/cast Number 1)
             :expected-ret
             (ret (parse-clj `Num)
                  (-FS -top -top)
                  -empty
                  (-flow -bot)))
    (is-tc-err (do (core/cast Number 1))
             :expected-ret
             (ret (parse-clj `Num)
                  (-FS -top -top)
                  -empty
                  (-flow -bot)))
    )
  (testing "tc-ignore"
    (is-tc-e (tc-ignore 1)
             Any)
    (is-tc-err (tc-ignore 1)
             Num)
    (is-tc-err (tc-ignore 1)
               :expected-ret
               (ret (parse-clj `Any)
                    (-true-filter)))
    (is-tc-err (tc-ignore 1)
               :expected-ret
               (ret (parse-clj `Any)
                    (-false-filter)))
    (is-tc-err (tc-ignore 1)
               :expected-ret
               (ret (parse-clj `Any)
                    (-FS -top -top)
                    (-path nil 'a)))
    (is-tc-err (tc-ignore 1)
               :expected-ret
               (ret (parse-clj `Any)
                    (-FS -top -top)
                    -empty
                    (-flow -bot)))
    )
  (testing "local"
    (is-tc-e (let [a 1]
               a))
    (is-tc-e (let [a 1]
               a)
             Num)
    (is-tc-err (let [a 1]
               a)
             Sym)
    (is-tc-e (let [a 1]
               a)
             :expected-ret
             (ret (parse-clj `Any)
                  (-true-filter)))
    (is-tc-err (let [a 1]
                 a)
               :expected-ret
               (ret (parse-clj `Any)
                    (-false-filter)
                    ))
    (is-tc-err (let [a 1]
                 a)
               :expected-ret
               (ret (parse-clj `Any)
                    (-FS -top -top)
                    (-path nil 'a)))
    (is-tc-err (let [a 1]
                 a)
               :expected-ret
               (ret (parse-clj `Any)
                    (-FS -top -top)
                    -empty
                    (-flow -bot))))
  (testing "monitor-enter"
    (is-tc-e #(monitor-enter 1))
    (is-tc-err #(monitor-enter nil))
    (is-tc-e #(monitor-enter 1)
             [-> nil :filters {:then ff :else tt}])
    (is-tc-err #(monitor-enter 1)
               [-> nil :filters {:then tt :else ff}])
    (is-tc-err #(monitor-enter 1)
               [-> nil :filters {:then ff :else tt} :flow ff])
    (is-tc-err (fn [a] (monitor-enter 1))
               [Any -> nil :filters {:then ff :else tt} :object {:id 0}]))
  (testing "monitor-exit"
    (is-tc-e #(monitor-exit 1))
    (is-tc-err #(monitor-exit nil))
    (is-tc-e #(monitor-exit 1)
             [-> nil :filters {:then ff :else tt}])
    (is-tc-err #(monitor-exit 1)
               [-> nil :filters {:then tt :else ff}])
    (is-tc-err #(monitor-exit 1)
               [-> nil :filters {:then ff :else tt} :flow ff])
    (is-tc-err (fn [a] (monitor-exit 1))
               [Any -> nil :filters {:then ff :else tt} :object {:id 0}]))
  (testing "def"
    (is-tc-e #(def a 1) [-> (Var1 (Val 1))])
    (testing ":dynamic metadata works"
      (is-tc-e (do (def ^:dynamic *blob* 1)
                   (tc-ignore
                     (assert (-> #'*blob*
                                 meta
                                 :dynamic))))))
    (testing "bad metadata throws static type error"
      (is-tc-err (def ^{:npe (inc nil)} a 1)))
    (is-tc-err #(def a 1) [-> (Var1 (Val 2))])
    (is-tc-e #(def a 1) 
             [-> (Var1 (Val 1)) :filters {:then tt :else ff}])
    (is-tc-err #(def a 1) 
             [-> (Var1 (Val 1)) :filters {:then ff :else tt}])
    (is-tc-err (fn [f] (def a 1))
               [Any -> (Var1 (Val 1)) :filters {:then tt :else ff}
                :object {:id 0}])
    )
)
