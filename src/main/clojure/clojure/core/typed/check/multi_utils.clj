(ns clojure.core.typed.check.multi-utils
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.type-rep :as r]))

(defonce ^:dynamic *current-mm* nil)
(set-validator! #'*current-mm* (some-fn nil? 
                                        (con/hmap-c? :dispatch-fn-type r/Type?
                                                     :dispatch-val-ret r/TCResult?)))
