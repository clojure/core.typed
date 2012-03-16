(ns typed-clojure.test
  (:use [analyze.core :only [analyze-path analyze-one]]
        [typed-clojure.checker :only [type-check-path reset-type-db type-db]])
  (:require [analyze.util :as util]
            [typed-clojure.flag :as flag]))

;(type-check-path "typed_clojure/tree.clj" 'typed-clojure.tree)
(try
  (swap! flag/type-check-flag (constantly true))
  (reset-type-db)
  (require 'typed-clojure.checker :reload)
  (require 'typed-clojure.simple :reload)
  (type-check-path "typed_clojure/simple.clj" 'typed-clojure.simple)

  (println "Type checking successful")
  (finally 
    (swap! flag/type-check-flag (constantly false)) nil))

(comment
  (analyze-path "typed_clojure/tree.clj" 'typed-clojure.tree)
  (util/print-expr (first (analyze-path "typed_clojure/tree.clj" 'typed-clojure.tree))
                   :children :env :Expr-obj :ObjMethod-obj)
  (util/print-expr (first (analyze-path "typed_clojure/tree.clj" 'typed-clojure.tree))
                   :children :env :Expr-obj :ObjMethod-obj)
  (analyze-one {:ns {:name 'user} :context :eval} (def my-atom (atom {}))
  ))
