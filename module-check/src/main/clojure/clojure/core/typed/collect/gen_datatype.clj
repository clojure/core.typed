(ns clojure.core.typed.collect.gen-datatype
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.parse-unparse :as prs]))

(defn parse-field [[n _ t]] 
  [n (prs/parse-type t)])

(def gen-datatype* impl/gen-datatype*)
