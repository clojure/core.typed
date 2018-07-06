(ns cljs.core.typed.test.js-obj
  (:require [cljs.core.typed :as t]))

(t/ann jo [-> (t/JSObj {:a t/JSNumber})])
(defn jo []
  #js {:a 2})

#_
(t/ann jo2 [-> (t/JSObj {:a t/JSNumber})])
;; TODO
#_
(defn jo2 []
  (js-obj :a 2))
