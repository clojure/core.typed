(ns clojure.core.typed.test.recursive
  (:require [clojure.core.typed :as t :refer [ann-record ann-protocol]])
  (:import [clojure.lang ISeq]))

(t/defprotocol IValidator
  "Validator abstraction"
  (validate- [this value] :- ValidationResult
             "Evaluates the validator."))

(t/ann-form validate- [IValidator t/Any -> ValidationResult])

(ann-record ValidationError [validator :- IValidator
                             value :- t/Any])
(defrecord ValidationError [validator value])
 
(ann-record ValidationResult
            [status :- (t/U ':ok ':error)
             result :- t/Any
             errors :- (t/U nil (t/Seq ValidationError))
             input :- t/Any])
(defrecord ValidationResult [status result errors input])
 
