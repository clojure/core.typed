(ns clojure.core.typed.test.bug1
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.chk.common.type_rep])
  (:import [clojure.core.typed.chk.common.type_rep Result]
           [clojure.lang Keyword]))

(t/ann *frees-mode* (U nil Keyword))
(def ^:dynamic *frees-mode* nil)

(t/ann frees [Any -> Any])
(defmulti ^:private frees (fn [t] [*frees-mode* (class t)]))

(defmethod frees [::any-var Result]
  [t]
  (t/print-env "before")
  (t/ann-form t Result)
  (let [{:keys [t fl o]} t]
    (t/print-env "after")))
