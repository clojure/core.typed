(ns clojure.core.typed.test.extend-record
  (:require [clojure.core.typed :as t :refer [ann-protocol ann-record]]
            #_[clojure.core.typed.checker.jvm.subtype :as sub]
            #_[clojure.core.typed.checker.jvm.parse-unparse :as prs]
            #_[clojure.core.typed.current-impl :as impl]))

(t/defprotocol PMaths
  (mult-by-two [this] :- PMaths))

(t/defprotocol PMathsExtend
  (mult-extended [this] :- PMathsExtend))

(ann-record SpecialNumber [x :- Number])
(defrecord SpecialNumber [x]
  PMaths
  (mult-by-two [this] 
    (t/ann-form this SpecialNumber)
    (assoc this :x (* (:x this) 2))))

(extend-protocol PMaths
  nil
  (mult-by-two [this]
    (t/ann-form this nil)
    this)
  String
  (mult-by-two [this]
    (t/ann-form this String)
    this))

(extend-protocol PMathsExtend
  SpecialNumber
  (mult-extended [this]
    (t/ann-form this SpecialNumber)
    (assoc this :x (* (:x this) 2))
    this))

;(not (sub/sub-clj? PMaths SpecialNumber))
;(sub/sub-clj? SpecialNumber PMaths)
;(sub/sub-clj? String PMaths)
;(sub/sub-clj? SpecialNumber (t/Map Any Any))

;(ancestors SpecialNumber)
;(prs/parse-clj 'PMaths)
;(impl/with-clojure-impl
;  (clojure.core.typed.checker.datatype-ancestor-env/get-datatype-ancestors  (prs/parse-clj 'SpecialNumber)))

;(sub/sub-clj? SpecialNumber PMathsExtend)
;(not (sub/sub-clj? PMathsExtend SpecialNumber))

;(sub/sub-clj?
;  (HMap :mandatory {:mult-by-two (Fn [Any -> nil])} :complete? true)
;  (U (HMap :mandatory {} :absent-keys #{:mult-by-two}) 
;     (HMap :mandatory {:mult-by-two (Fn [Nothing -> String])})))
;
;(sub/sub-clj?
;  [Any -> nil]
;  [Nothing -> Any])

;(sub/sub-clj? String PMaths)
;(sub/sub-clj? PMaths String)
;(prs/parse-clj '(I String PMaths))
