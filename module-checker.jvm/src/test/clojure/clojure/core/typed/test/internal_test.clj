(ns clojure.core.typed.test.internal-test
  "Tests for c.c.t.internal"
  (:require [clojure.test :refer :all]
            [clojure.core.typed.test.test-utils :refer :all]
            [clojure.core.typed.internal :as internal]))

(deftest parse-fn-test
  (is (= (:fn (internal/parse-fn* '(fn [a b c] a)))
         (:fn (internal/parse-fn* '(fn [a :- clojure.core.typed/Any b c] a)))
         (:fn (internal/parse-fn* '(fn [a :- clojure.core.typed/Any b :- clojure.core.typed/Any c] a)))
         (:fn (internal/parse-fn* '(fn [a :- clojure.core.typed/Any b c :- clojure.core.typed/Any] a)))
         (:fn (internal/parse-fn* '(fn [a :- clojure.core.typed/Any b :- clojure.core.typed/Any c :- clojure.core.typed/Any] a)))
         (:fn (internal/parse-fn* '(fn [a :- clojure.core.typed/Any b :- clojure.core.typed/Any c :- clojure.core.typed/Any] :- clojure.core.typed/Any a)))
         '(clojure.core/fn ([a b c] a))))
  (is (= (:fn (internal/parse-fn* '(fn [a] a)))
         (:fn (internal/parse-fn* '(fn [a :- clojure.core.typed/Any] a)))
         (:fn (internal/parse-fn* '(fn [a :- clojure.core.typed/Any] :- clojure.core.typed/Any a)))
         (:fn (internal/parse-fn* '(fn [a] :- clojure.core.typed/Any a)))
         '(clojure.core/fn ([a] a))))
  (is (= (select-keys (internal/parse-fn* '(fn name [a] :- clojure.core.typed/Any a)) [:fn :ann :poly])
         {:fn '(clojure.core/fn name ([a] a))
          :ann [{:dom [{:default true :type 'clojure.core.typed/Any}]
                 :rng {:type 'clojure.core.typed/Any}}]
          :poly nil}))
  (is (= (:fn (internal/parse-fn* '(fn [a :- Number] a)))
         (:fn (internal/parse-fn* '(fn [a :- Number] :- clojure.core.typed/Any a)))
         '(clojure.core/fn ([a] a))))
  (is (= (select-keys (internal/parse-fn* '(fn name [a :- Number] :- clojure.core.typed/Any a))
                      [:fn :ann :poly])
         {:fn '(clojure.core/fn name ([a] a))
          :ann [{:dom [{:type 'Number}]
                 :rng {:type 'clojure.core.typed/Any}}]
          :poly nil}))
  (is (= (select-keys (internal/parse-fn* '(fn [a :- Number] :- Number a)) [:fn :ann :poly])
         {:fn '(clojure.core/fn ([a] a))
          :ann [{:dom [{:type 'Number}]
                 :rng {:type 'Number}}]
          :poly nil}))
  (is (= (:fn (internal/parse-fn* '(fn [a & b] a)))
         (:fn (internal/parse-fn* '(fn [a :- clojure.core.typed/Any & b] a)))
         (:fn (internal/parse-fn* '(fn [a :- clojure.core.typed/Any & b :- clojure.core.typed/Any *] :- clojure.core.typed/Any a)))
         (:fn (internal/parse-fn* '(fn [a :- clojure.core.typed/Any & b] :- clojure.core.typed/Any a)))
         (:fn (internal/parse-fn* '(fn [a :- clojure.core.typed/Any & b] :- clojure.core.typed/Any a)))
         (:fn (internal/parse-fn* '(fn [a :- clojure.core.typed/Any & b] :- clojure.core.typed/Any a)))
         '(clojure.core/fn ([a & b] a))))
  (is (= (select-keys (internal/parse-fn* '(fn [a :- Number & b :- Number *] :- Number b)) [:fn :ann :poly])
         {:fn '(clojure.core/fn ([a & b] b))
          :ann [{:dom [{:type 'Number}]
                 :rest {:type 'Number}
                 :rng {:type 'Number}}]
          :poly nil}))
  (is (= (select-keys (internal/parse-fn* '(fn [a & b :- Number ... x] a)) [:fn :ann :poly])
         {:fn '(clojure.core/fn ([a & b] a))
          :ann [{:dom [{:default true :type 'clojure.core.typed/Any}]
                 :drest {:bound 'x
                         :pretype {:type 'Number}}
                 :rng {:default true :type 'clojure.core.typed/Any}}]
          :poly nil}))

  (is (= (select-keys (internal/parse-fn* '(fn [a :- clojure.core.typed/Any & b :- Number ... x] a)) [:fn :ann :poly])
         {:fn '(clojure.core/fn ([a & b] a))
          :ann [{:dom [{:type 'clojure.core.typed/Any}]
                 :drest {:bound 'x
                         :pretype {:type 'Number}}
                 :rng {:default true :type 'clojure.core.typed/Any}}]
          :poly nil}))
  (is (= (select-keys (internal/parse-fn* '(fn [a :- clojure.core.typed/Any & b :- Number ... x] :- clojure.core.typed/Any a))
                      [:fn :ann :poly])
         {:fn '(clojure.core/fn ([a & b] a))
          :ann [{:dom [{:type 'clojure.core.typed/Any}]
                 :drest {:bound 'x
                         :pretype {:type 'Number}}
                 :rng {:type 'clojure.core.typed/Any}}]
          :poly nil}))
  (is (= (-> (internal/parse-fn* '(fn ^long [b]))
             :fn second first meta)
         '{:tag long})))

(deftest parse-loop-test
  (is (= (internal/parse-loop* '([a []] a))
         {:loop '(clojure.core/loop [a []] a)
          :ann {:params [{:default true :type 'clojure.core.typed/Any}]}}))
  (is (= (internal/parse-loop* '([a :- clojure.core.typed/Any []] a))
         {:loop '(clojure.core/loop [a []] a)
          :ann {:params [{:type 'clojure.core.typed/Any}]}})))

(deftest parse-defprotocol-test
  ;cannot shadow tvars
  (is (thrown? AssertionError
               (internal/parse-defprotocol*
                 '([[x :variance :covariant]] Name ([x] m1 [this t])))))
  ;unannotated
  (is (= (internal/parse-defprotocol*
           '(Name (m1 [this t])
                  (m2 [this t] [this t y])))
         {:defprotocol '(clojure.core/defprotocol Name
                          (m1 [this t])
                          (m2 [this t] [this t y]))
          :ann-protocol '(clojure.core.typed/ann-protocol Name
                           m1
                           (clojure.core.typed/IFn [Name clojure.core.typed/Any -> clojure.core.typed/Any])
                           m2
                           (clojure.core.typed/IFn [Name clojure.core.typed/Any -> clojure.core.typed/Any]
                               [Name clojure.core.typed/Any clojure.core.typed/Any -> clojure.core.typed/Any]))}))
  ; fully annotated, no poly
  (is (= (internal/parse-defprotocol*
           '(Name (m1 [this t :- Foo] :- Bar)
                  (m2 [this t :- Number] :- Baz
                      [this t :- Number, y :- Blah] :- Bar)))
         {:defprotocol '(clojure.core/defprotocol Name
                          (m1 [this t])
                          (m2 [this t] [this t y]))
          :ann-protocol '(clojure.core.typed/ann-protocol Name
                           m1
                           (clojure.core.typed/IFn [Name Foo -> Bar])
                           m2
                           (clojure.core.typed/IFn [Name Number -> Baz]
                               [Name Number Blah -> Bar]))}))
  ; method intersections
  (is (= (internal/parse-defprotocol*
           '(Name (m1 [this t :- Foo] :- Bar
                      [this t :- Foo1] :- Bar1
                      [this e :- Foo2] :- Bar2)))
         {:defprotocol '(clojure.core/defprotocol Name
                          (m1 [this t]))
          :ann-protocol '(clojure.core.typed/ann-protocol Name
                           m1
                           (clojure.core.typed/IFn [Name Foo -> Bar]
                               [Name Foo1 -> Bar1]
                               [Name Foo2 -> Bar2]))}))
  ;polymorphic protocols with doc
  (is (= (internal/parse-defprotocol*
           '([[x :variance :covariant]]
             Name (m1 [this t :- Foo] :- Bar
                      [this t :- Foo1] :- Bar1
                      [this e :- Foo2] :- Bar2
                      "Doc")))
         {:defprotocol '(clojure.core/defprotocol Name
                          (m1 [this t] "Doc"))
          :ann-protocol '(clojure.core.typed/ann-protocol 
                           [[x :variance :covariant]]
                           Name
                           m1
                           (clojure.core.typed/IFn [(Name x) Foo -> Bar]
                               [(Name x) Foo1 -> Bar1]
                               [(Name x) Foo2 -> Bar2]))}))
  ; polymorphic method
  (is (= (internal/parse-defprotocol*
           '([[x :variance :covariant]]
             Name ([y] 
                   m1 
                   [this t :- Foo] :- Bar
                   [this t :- Foo1] :- Bar1
                   [this e :- Foo2] :- Bar2)))
         {:defprotocol '(clojure.core/defprotocol Name
                          (m1 [this t]))
          :ann-protocol '(clojure.core.typed/ann-protocol 
                           [[x :variance :covariant]]
                           Name
                           m1
                           (clojure.core.typed/All [y]
                                (clojure.core.typed/IFn
                                  [(Name x) Foo -> Bar]
                                  [(Name x) Foo1 -> Bar1]
                                  [(Name x) Foo2 -> Bar2])))}))
  (is (= (-> (internal/parse-defprotocol*
               '([[x :variance :covariant]]
                 Name ([y] 
                       m1 
                       ^long [this t :- Foo] :- Bar)))
             :defprotocol next next first second meta)
         '{:tag long}))
  )

(deftest parse-let-test
  (is (= (internal/parse-let* '([a b c d] 1 2 3 4))
         {:let '(clojure.core/let [a b c d] 1 2 3 4)}))
  (is (= (internal/parse-let* '([a :- Foo b c :- Baz d] 1 2 3 4))
         {:let '(clojure.core/let [a (clojure.core.typed/ann-form b Foo) 
                                   c (clojure.core.typed/ann-form d Baz)] 
                  1 2 3 4)}))
  (is (= (-> (internal/parse-let* '(^:test-meta [a :- Foo b c :- Baz d] 1 2 3 4))
             :let second meta)
         '{:test-meta true})))
