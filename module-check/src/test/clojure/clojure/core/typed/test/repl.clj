(ns clojure.core.typed.test.repl
  (:use clojure.test
        [clojure.tools.nrepl :as nrepl])
  (:require (clojure.tools.nrepl [transport :as transport]
                                 [server :as server]
                                 [ack :as ack])
            [clojure.set :as set]
            [clojure.core.typed.repl :as repl]))

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
;    (is (= {:value ["nil"]}
;           (eval-val transport "(ns ^:core.typed foo) ")))
;    (is (= {:value ["nil"]}
;           (eval-val transport "*ns*")))
;    (is (= {:value ["2"]}
;           (eval-val transport "(inc 1)")))
;    )
;  )
