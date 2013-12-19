(ns clojure.core.typed.test.recursive
  (:require [clojure.core.typed :as t :refer [ann-record ann-protocol defprotocol>]])
  (:import [clojure.lang ISeq]))

(ann-protocol IValidator
              validate- [IValidator Any -> ValidationResult])
(defprotocol> IValidator
  "Validator abstraction"
  (validate- [this value] "Evaluates the validator."))

(t/ann-form validate- [IValidator Any -> ValidationResult])

(ann-record ValidationError [validator :- IValidator
                             value :- Any])
(defrecord ValidationError [validator value])
 
(ann-record ValidationResult
            [status :- (U ':ok ':error)
             result :- Any
             errors :- (U nil (ISeq ValidationError))
             input :- Any])
(defrecord ValidationResult [status result errors input])
 
