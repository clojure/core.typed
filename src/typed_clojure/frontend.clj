(ns typed-clojure.frontend
  (:use [analyze.core :only [analyze-path]]))

(do
  (analyze-path "typed_clojure/tree.clj" 'typed-clojure.tree)
  nil)
