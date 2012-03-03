(ns typed-clojure.test
  (:use [analyze.core :only [analyze-path]]
        [typed-clojure.checker :only [type-check-path]])
  (:require [analyze.util :as util]))

(type-check-path "typed_clojure/tree.clj" 'typed-clojure.tree)

(comment
  (analyze-path "typed_clojure/tree.clj" 'typed-clojure.tree)
  (util/print-expr (first (analyze-path "typed_clojure/tree.clj" 'typed-clojure.tree))
                   :children :env :Expr-obj :ObjMethod-obj)
  (util/print-expr (first (analyze-path "typed_clojure/tree.clj" 'typed-clojure.tree))
                   :children :env :Expr-obj :ObjMethod-obj)
  )
