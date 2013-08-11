(ns clojure.core.typed.test.extend-record
  (:require [clojure.core.typed :as t :refer [ann-protocol ann-record]]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.parse-unparse :as prs]))

(ann-protocol PMaths
              mult-by-two [PMaths -> PMaths])
(t/defprotocol> PMaths
  (mult-by-two [this]))

(ann-record SpecialNumber [x :- Number])
(defrecord SpecialNumber [x]
  PMaths
  (mult-by-two [this] (assoc this :x (* (:x this) 2))))

;(sub/sub-clj? PMaths SpecialNumber)
;(sub/sub-clj? SpecialNumber PMaths)
