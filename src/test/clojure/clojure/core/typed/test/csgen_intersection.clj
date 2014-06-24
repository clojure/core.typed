(ns clojure.core.typed.test.csgen-intersection
  (:require [clojure.core.typed.test.test-utils :refer :all]
            [clojure.test :refer :all]
            #_[clojure.tools.trace :refer [trace-vars]]
            [clojure.core.typed.cs-gen :as cgen]
            ))

#_(trace-vars cgen/infer)

(deftest csgen-intersect
  (is-tc-e (do 
             (defprotocol 
               [[x :variance :covariant]]
               ICollection')
             (defprotocol 
               [[x :variance :covariant]]
               ISeq')
             (defprotocol 
               [[x :variance :covariant]]
               ISeqable')
             (defprotocol 
               [[x :variance :covariant]]
               IList')
             (defalias
               NEColl'
               (TFn [[x :variance :covariant]]
                    (ICollection' x)))
             (defalias
               NEASeq'
               (TFn [[x :variance :covariant]]
                    (I 
                      (ICollection' x)
                      (ISeqable' x)
                      (ISeq' x)
                      (IList' x)
                      #_(CountRange 1))))
             (fn [seq' :- (All [x] [(NEColl' x) -> (NEASeq' x)])
                  a :- (NEColl' Int)] 
               :- (NEASeq' Number)
               (seq' a)))))
