(ns clojure.core.typed.test.bug1
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.checker.type_rep])
  (:import [clojure.core.typed.checker.type_rep Result]
           [clojure.lang Keyword]))

(t/ann *frees-mode* (t/U nil t/Kw))
(def ^:dynamic *frees-mode* nil)

(t/ann frees [t/Any -> t/Any])
(defmulti ^:private frees (fn [t] [*frees-mode* (class t)]))

(defmethod frees [::any-var Result]
  [t]
  (t/print-env "before")
  (t/ann-form t Result)
  (let [{:keys [t fl o]} t]
    (t/print-env "after")))
