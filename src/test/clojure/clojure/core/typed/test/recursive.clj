(ns clojure.core.typed.test.recursive
  (:require [clojure.core.typed :as t :refer [ann-record ann-protocol defprotocol>]])
  (:import [clojure.lang ISeq]))

(ann-protocol IValidator
              validate- [IValidator t/Any -> ValidationResult])
(defprotocol> IValidator
  "Validator abstraction"
  (validate- [this value] "Evaluates the validator."))

(t/ann-form validate- [IValidator t/Any -> ValidationResult])

(ann-record ValidationError [validator :- IValidator
                             value :- t/Any])
(defrecord ValidationError [validator value])
 
(ann-record ValidationResult
            [status :- (U ':ok ':error)
             result :- t/Any
             errors :- (U nil (ISeq ValidationError))
             input :- t/Any])
(defrecord ValidationResult [status result errors input])
 
