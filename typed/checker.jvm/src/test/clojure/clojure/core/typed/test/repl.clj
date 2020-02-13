(ns clojure.core.typed.test.repl
  (:use clojure.test
        [clojure.tools.nrepl :as nrepl])
  (:require (clojure.tools.nrepl [transport :as transport]
                                 [server :as server]
                                 [ack :as ack])
            [clojure.set :as set]
            [clojure.core.typed.checker.jvm.repl :as repl]))

(def ^{:dynamic true} *server* nil)

(defn repl-server-fixture
  [f]
  (with-open [^java.io.Closeable server (server/start-server
                                          :handler (server/default-handler
                                                     #'repl/wrap-clj-repl))]
    (binding [*server* server]
      (let [s (f)]
        (set! *print-length* nil)
        (set! *print-level* nil)
        s))))

;(use-fixtures :each repl-server-fixture)

(deftest eval-switch-ns
  (is (= "blah"
         (:ns 
           (with-open [^java.io.Closeable server (server/start-server
                                :handler (server/default-handler
                                           #'repl/wrap-clj-repl))]
             (with-open [^java.io.Closeable transport (connect :port (:port server))]
               (let [cl (client transport Long/MAX_VALUE)
                     ses (client-session cl)]
                 (combine-responses
                   (message ses
                            {:op :eval :code "(ns blah)"}))
                 (combine-responses
                   (message ses
                            {:op :eval :code "*ns*"})))))))))

(deftest load-file-test
  (is (= ["42"]
         (:value
           (with-open [^java.io.Closeable server (server/start-server
                                                   :handler (server/default-handler
                                                              #'repl/wrap-clj-repl))]
             (with-open [^java.io.Closeable transport (connect :port (:port server))]
               (let [cl (client transport Long/MAX_VALUE)
                     ses (client-session cl)]
                 (combine-responses
                   (message ses
                            {:op :load-file 
                             :file "(ns ^:core.typed foo.bar
                                     (:require [clojure.core.typed :as t]))
                                    (t/ann a t/Sym)
                                    (def a 'a)
                                    42"
                             :file-path "foo/bar.clj"
                             :file-name "bar.clj"}))))))))
  ;;FIXME broke after re-throwing exception in check-form-common
  #_
  (testing "runtime error"
    (is (= "class java.lang.ArithmeticException"
           (:ex
             (with-open [^java.io.Closeable server (server/start-server
                                                     :handler (server/default-handler
                                                                #'repl/wrap-clj-repl))]
               (with-open [^java.io.Closeable transport (connect :port (:port server))]
                 (let [cl (client transport Long/MAX_VALUE)
                       ses (client-session cl)]
                   (combine-responses
                     (message ses
                              {:op :load-file 
                               :file "(ns ^:core.typed foo.bar
                                     (:require [clojure.core.typed :as t]))
                                     (t/ann a t/Sym)
                                     (def a 'a)
                                     (/ 1 0)"
                               :file-path "foo/bar.clj"
                               :file-name "bar.clj"})))))))))
  ;; FIXME
  #_
  (testing "fireplace test"
    (is (= "class clojure.lang.ExceptionInfo"
           (:ex
             (with-open [^java.io.Closeable server (server/start-server
                                                     :handler (server/default-handler
                                                                #'repl/wrap-clj-repl))]
               (with-open [^java.io.Closeable transport (connect :port (:port server))]
                 (let [cl (client transport Long/MAX_VALUE)
                       ses (client-session cl)]
                   (combine-responses
                     (message ses
                              {:op :load-file 
                               :file "(ns ^:core.typed baz.boo)"
                               :file-path "baz/boo.clj"
                               :file-name "boo.clj"}))
                   (combine-responses
                     (message ses
                              {:op :load-file 
                               :file "(in-ns 'baz.boo)
                                      (inc 'a)"
                               :file-path "baz/boo.clj"
                               :file-name "boo.clj"})))))))))
  )

;(with-open [^java.io.Closeable server (server/start-server)]
;  (with-open [transport (connect :port (:port server))]
;    (message (client transport Long/MAX_VALUE)
;             {:op :eval :code "1"})))

(defmacro repl-test
  [& body]
  `(repl-server-fixture
     #(with-open [~(with-meta 'transport {:tag 'java.io.Closeable}) (connect :port (:port *server*))]
        ~@body)))

;(repl-test
;  (combine-responses
;    (message (client transport Long/MAX_VALUE)
;             {:op :eval :code "(ns ^:core.typed foo)"})))
;
;(repl-test
;  (combine-responses
;    (message (client transport Long/MAX_VALUE)
;             {:op :eval :code "(new java.io.File \"a\")"})))
;
;(deftest tst-pkt
;  (repl-test
;    (combine-responses
;      (message (client transport Long/MAX_VALUE)
;               {:op :eval :code "(ns ^:core.typed foo)"}))))

(defn eval-msg [transport c]
  (message (client transport Long/MAX_VALUE)
           {:op :eval :code c}))

(defn eval-val [transport c]
  (-> (eval-msg transport c)
      combine-responses
      (select-keys [:value])))

;(deftest core-typed-test
;  (repl-test
;    (eval-msg transport "(ns bar) ")
;    ;(is (= {:value ["nil"]}
;    ;       (eval-val transport "(ns ^:core.typed foo) ")))
;    ;(is (= {:value ["nil"]}
;    ;       (eval-val transport "*ns*")))
;    ;(is (= {:value ["2"]}
;    ;       (eval-val transport "(inc 1)")))
;    )
;  )
