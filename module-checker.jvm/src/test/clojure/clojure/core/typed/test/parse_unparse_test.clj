(ns clojure.core.typed.test.parse-unparse-test
  (:require 
    [clojure.core.typed.test.test-utils :refer :all]
    [clojure.test :refer :all]
    [clojure.core.typed.checker.jvm.parse-unparse :refer :all]
    [clojure.core.typed :as t]))

(deftest unparse-free-scoping-test
  (is-clj (= (second
               (unparse-type 
                 (parse-type 
                   `(t/All ~'[a b] [t/Any t/Any :-> t/Any]))))
             '[a b]))
  (is-clj (= (rest (unparse-type (parse-type `(t/TFn ~'[[a :variance :covariant]] ~'a))))
             '([[a :variance :covariant]] a)))
  (is-clj (= (do
               '([a b] [a b -> [a b -> nil :filters {:then ff :else tt}]
                              :filters {:then tt :else ff}]))
             (->
               (tc-e
                 (fn :forall [a b]
                   [f :- a, coll :- b]
                   (fn
                     [x :- a
                      y :- b])))
               :t
               unparse-type
               rest))))

(deftest bad-dots-Poly-test
  ;; no dots in variable
  (is (throws-tc-error?
        (parse-clj '(clojure.core.typed/All [... a] [a -> a]))))
  (is (throws-tc-error?
        (parse-clj '(clojure.core.typed/All [. a] [a -> a]))))
  (is (throws-tc-error?
        (parse-clj '(clojure.core.typed/All [. a] [a -> a]))))
  ; no nil/true/false
  (is (throws-tc-error?
        (parse-clj `(clojure.core.typed/All [~(symbol "nil")] [nil :-> nil]))))
  (is (throws-tc-error?
        (parse-clj `(clojure.core.typed/All [~(symbol "true")] [nil :-> nil]))))
  (is (throws-tc-error?
        (parse-clj `(clojure.core.typed/All [~(symbol "false")] [nil :-> nil]))))
  ; no ns qualified
  (is (throws-tc-error?
        (parse-clj `(clojure.core.typed/All [a/b] [nil :-> nil]))))
  ; non-symbol
  (is (throws-tc-error?
        (parse-clj `(clojure.core.typed/All [:a] [nil :-> nil]))))
  ; bad kw args
  (is (throws-tc-error?
        (parse-clj `(clojure.core.typed/All [:a :b] [nil :-> nil])))))

(deftest poly-named-test
  (is (= (unparse-type
           (parse-clj 
             '(clojure.core.typed/All [:named [a b]] [a -> b])))
         '(clojure.core.typed/All [:named [a b]] [a -> b])))
  (is (= (unparse-type
           (parse-clj 
             '(clojure.core.typed/All [:named [a b]] [a -> b])))
         '(clojure.core.typed/All [:named [a b]] [a -> b])))
  (is (= (unparse-type
           (parse-clj 
             '(clojure.core.typed/All [a ... :named [b c]]
                                      [c b a ... a -> b])))
         '(clojure.core.typed/All [a ... :named [b c]]
                                  [c b a ... a -> b])))
  (is-tc-e (do (t/ann ^:no-check foo 
                      (t/All [:named [a b]]
                             [a -> b]))
               (def foo identity)
               (t/inst foo :named {a t/Num b t/Num}))
           [t/Num :-> t/Num])
  (is-tc-e (do (t/ann ^:no-check foo 
                      (t/All [:named [a b]]
                             [a -> b]))
               (def foo identity)
               (t/inst foo :named {a t/Num}))
           [t/Num :-> t/Any])
  (is-tc-err (do (t/ann ^:no-check foo 
                        (t/All [:named [a b]]
                               [a -> b]))
                 (def foo identity)
                 (t/inst foo :named {a t/Num}))
             [t/Any :-> t/Num])
  (is-tc-e (do (t/ann ^:no-check foo 
                      (t/All [:named [a b]]
                             [a -> b]))
               (def foo identity)
               (t/inst foo))
           [t/Any :-> t/Any])
  (is-tc-e (do (t/ann ^:no-check foo 
                      (t/All [a ...]
                             [a ... a -> t/Any]))
               (defn foo [& args])
               (t/inst foo t/Str t/Bool))
           [t/Str t/Bool :-> t/Any])
  (is-tc-e (do (t/ann ^:no-check foo 
                      (t/All [a ... :named [b c]]
                             [c b a ... a -> b]))
               (defn foo [& args] (second args))
               (t/inst foo t/Str t/Bool :named {c t/Num b t/Sym}))
           [t/Num t/Sym t/Str t/Bool :-> t/Sym]))
