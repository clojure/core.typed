(ns mini-typed.test.example
  (:require [mini-typed.core :refer [+T def-type Fn Any All I Case U def-poly-type Inst Seqable
                                     Seq]]))

(def-type StringGenerator (Fn [& String -> String]))

(def-poly-type SomePoly [[a :variance :invariant]
                         [b :variance :invariant]]
               (Fn [& (Inst Seqable a) -> (Inst Seq b)]))

(def-type Something (All [[x :< Object :variance :invariant]]
                      (Inst Seqable x)))

(+T generate-string StringGenerator)
(defn generate-string []
  "a")
